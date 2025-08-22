import time
import random
import dspy
import models as models
from utils import current_dspy_lm
from multiprocessing import Process, Queue
from record import record
from respond_to_error_patterns import respond_to_error_patterns
# from opentelemetry.trace.propagation import suppress_instrumentation

# Note: parallel_consensus mechanism below assumes that the passed function returns
# an "answer" object that can have str() called on it.

def run_model_with_backoff(model_idx, fan_idx, model, func, *args, **kwargs):
    #print("run_model_with_backoff")
    # must re-import in subprocess(?)
    import dspy
    from respond_to_error_patterns import respond_to_error_patterns
    
    with dspy.context(lm=model):
        error_responses = [
            {
                "pattern": r'RateLimitError',
                "retry": {
                    "min_delay": 30,
                    "max_delay": 180,
                    "max_attempts": 5
                },
                "raise_exception": RuntimeError("Ran out of retries on rate limit errors.")
            },
            {
                "pattern": r'litellm.APIError',
                "retry": {
                    "min_delay": 30,
                    "max_delay": 180,
                    "max_attempts": 5
                },
                "raise_exception": RuntimeError("Ran out of retries on API errors.")
            }
        ]
        try:
            result = respond_to_error_patterns(error_responses, func, *args, **kwargs)
        except Exception as e:
            print(f"worker[{model_idx},{fan_idx}] with model={model} hit error: {e}. Returning None.")
            result = None
        return result                                                                     

def _worker(model_idx, fan_idx, model, func, args, kwargs, queue, initial_delay=True):
    # must re-import in subprocess(?)
    import random, time

    if initial_delay:
        # Introduce a short random sleep before issuing call, bc the simultaneous
        # LLM calls seem to cause errors with OpenAI.
        pre_delay = random.uniform(0, 10)
        time.sleep(pre_delay)

    result = run_model_with_backoff(model_idx, fan_idx, model, func, *args, **kwargs)
    queue.put(((model_idx, fan_idx), result))

def try_models_parallel(models, func, fanout, *args, **kwargs):
    print("try_models_parallel")
    queue = Queue()
    processes = []

    for model_idx, model in enumerate(models):
        for fan_idx in range(fanout):
            p = Process(
                target=_worker,
                args=(model_idx, fan_idx, model, func, args, kwargs, queue)
            )
            p.start()
            processes.append(p)

    results = {}
    for _ in range(len(models) * fanout):
        key, result = queue.get()
        results[key] = result

    for p in processes:
        p.join()

    # Optional: sort for deterministic order
    results_arr = [results[(i, j)] for i in range(len(models)) for j in range(fanout)]
    results_arr = [r for r in results_arr if r is not None]
    return results_arr

def try_models_serial(models, func, fanout, *args, **kwargs):
    results = {}

    for model_idx, model in enumerate(models):
        for fan_idx in range(fanout):
            result = run_model_with_backoff(model_idx, fan_idx, model, func, *args, **kwargs)
            results[(model_idx, fan_idx)] = result
            
    results_arr = [results[(i, j)] for i in range(len(models)) for j in range(fanout)]
    results_arr = [r for r in results_arr if r is not None]
    return results_arr                                      

# Get the more common of True or False, and throw an error in the case of a tie.
def majority_bool(items, require_agreement=False):
    print("majority_bool", items)
    if not all(isinstance(x, bool) for x in items):
        raise ValueError("All items must be booleans")
    count_true = sum(items)
    count_false = len(items) - count_true
    if require_agreement and count_true > 0 and count_false > 0:
        raise ValueError("The answer is not unanimous.")
    if count_true > count_false:
        return True
    elif count_false > count_true:
        return False
    else:
        raise ValueError("Tie between True and False")

class PickerByIndexSignature(dspy.Signature):
    """Pick the most consensus answer from a list of answers from different experts, without knowing the question."""
    answer_choices: str = dspy.InputField(desc="A numbered list of answers that were given by different experts.")
    consensus_answer_index: int = dspy.OutputField(desc="The number associated with the most representative expert answer -- the one that best captures the consensus of all the experts. Note the choices are one-indexed, so this index should be one-indexed as well.")

class ParallelConsensus(dspy.Module):
    def __init__(self, fn, worker_models, fanout, picker_sig=PickerByIndexSignature, verbose=False):
        self.verbose = verbose
        self.worker_fn = fn
        self.worker_models = worker_models
        self.fanout = fanout
        self.picker = dspy.Predict(picker_sig)

    def name(self):
        model_name = current_dspy_lm()
        return f"parallel_consensus_{len(self.worker_models)}models_{self.fanout}each_{self.worker_fn.__name__}_{model_name}"

    def record(self, record_name, content, verbose=True):
        record(f"{self.name()}/{record_name}", content, verbose)

    def format_choices_string(self, answers):
        """Formats a list of answers into a numbered list as a single string."""
        return "\n".join(f"{index + 1}. {str(answer)}" for index, answer in enumerate(answers))

    # Last-minute change: jk_be_serial=True bc I dont think its safe to use parallel-consensus because of some occasional bad interaction with telemetry.  God I hate telemetry.
    def forward(self, *args, require_agreement=False, jk_be_serial=True, **kwargs):
        if jk_be_serial:
            # we provide a serial bail-out option.
            answers = try_models_serial(self.worker_models, self.worker_fn, self.fanout, *args, **kwargs)
        else:
            answers = try_models_parallel(self.worker_models, self.worker_fn, self.fanout, *args, **kwargs)
        if all(isinstance(answer, bool) for answer in answers):
            # short circuit the consensus if all are bools (no need for an LLM)
            consensus_answer = majority_bool(answers, require_agreement)
        else:
            # We don't yet implement the require_agreement option in this branch
            assert len(answers) > 0, f"ParallelConsensus forward method got zero answers."
            answer_choices_str = self.format_choices_string(answers)
            self.record("answer_choices_str", answer_choices_str, False)
            pickerpred = self.picker(answer_choices=answer_choices_str)
            consensus_idx = pickerpred.consensus_answer_index - 1 # make it zero-indexed
            try:
                assert consensus_idx >= 0, f"The consensus answer index {consensus_idx} was less than zero."
                assert consensus_idx < len(answers), f"The consensus answer index {consensus_idx} exceeded the number of answer choices {len(answers)}."
            except Exception as e:
                print(f"ParallelConsensus forward method got error: {e}. Defaulting to the first answer.")
                consensus_idx = 0
            self.record("consensus_answer_number", consensus_idx + 1, False)
            #self.record("consensus_idx(zero-indexed)", consensus_idx, False)
            consensus_answer = answers[consensus_idx]
        self.record("consensus_answer", consensus_answer)
        return dspy.Prediction(consensus=consensus_answer)

def parallel_consensus(worker_fn, worker_models, worker_fanout, *args, **kwargs):
    rep_answerer = ParallelConsensus(worker_fn, worker_models, worker_fanout)
    rep_pred = rep_answerer(*args, **kwargs)
    return rep_pred.consensus
