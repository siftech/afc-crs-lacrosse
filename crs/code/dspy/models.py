import dspy
from multiprocessing import Process, Queue
from contextlib import contextmanager
import sys
from functools import wraps


USE_CACHE = False # FIXME: True saves money and ensures consistency when debugging, but False is what we want for deployment and certain types of testing.

# See *default-llm-args* and *pov-llm-args* in code/lisp/amp/globals.lisp for how to use these models in LAX pipelines
# max_tokens is Max output tokens
gpt4o_100 = dspy.LM('openai/gpt-4o-2024-08-06', max_tokens=100, temperature=0, cache=USE_CACHE)
gpt4omini_100 = dspy.LM('openai/gpt-4o-mini-2024-07-18', max_tokens=100, temperature=0, cache=USE_CACHE)
gpt4o_1000 = dspy.LM('openai/gpt-4o-2024-08-06', max_tokens=1000, temperature=0, cache=USE_CACHE)
gpt4o_1000_cached = dspy.LM('openai/gpt-4o-2024-08-06', max_tokens=1000, temperature=0, cache=True)
gpt41_1000_cached = dspy.LM('openai/gpt-4.1-2025-04-14', max_tokens=1000, temperature=0, cache=True)
gpt4omini_1000 = dspy.LM('openai/gpt-4o-mini-2024-07-18', max_tokens=1000, temperature=0, cache=USE_CACHE)
gpt4o_10000 = dspy.LM('openai/gpt-4o-2024-08-06', max_tokens=10000, temperature=0, cache=USE_CACHE)
gpt4omini_10000 = dspy.LM('openai/gpt-4o-mini-2024-07-18', max_tokens=10000, temperature=0, cache=USE_CACHE)
gpt41_32768 = dspy.LM('openai/gpt-4.1-2025-04-14', max_tokens=32768, temperature=0, cache=USE_CACHE)
gpt4o_16384 = dspy.LM('openai/gpt-4o-2024-08-06', max_tokens=16384, temperature=0, cache=USE_CACHE)
gpt4turbo = dspy.LM('openai/gpt-4-turbo-2024-04-09', max_tokens=4096, temperature=0, cache=USE_CACHE)
gpt41mini = dspy.LM('openai/gpt-4.1-mini-2025-04-14', max_tokens=32768, temperature=0, cache=USE_CACHE)
gpt41nano = dspy.LM('openai/gpt-4.1-nano-2025-04-14', max_tokens=32768, temperature=0, cache=USE_CACHE)
gpt41 = gpt41_32768
gpt41_tmp05 = dspy.LM('openai/gpt-4.1-2025-04-14', max_tokens=32768, temperature=0.5, cache=USE_CACHE)
gpt41_tmp1 = dspy.LM('openai/gpt-4.1-2025-04-14', max_tokens=32768, temperature=1.0, cache=USE_CACHE)
gpt4o = gpt4o_16384
gpt4o_tmp05 = dspy.LM('openai/gpt-4o-2024-08-06', max_tokens=16384, temperature=0.5, cache=USE_CACHE)
gpt4o_tmp1 = dspy.LM('openai/gpt-4o-2024-08-06', max_tokens=16384, temperature=1.0, cache=USE_CACHE)
gpt4onew = dspy.LM('openai/gpt-4o-2024-11-20', max_tokens=16384, temperature=0, cache=USE_CACHE)
gpt4omini = gpt4omini_10000
o3 = dspy.LM('openai/o3-2025-04-16', max_tokens=100000, temperature=1.0, cache=USE_CACHE)
o1 = dspy.LM('openai/o1-2024-12-17', max_tokens=100000, temperature=1.0, cache=USE_CACHE)
o4mini = dspy.LM('openai/o4-mini-2025-04-16', max_tokens=100000, temperature=1.0, cache=USE_CACHE)
o3mini = dspy.LM('openai/o3-mini-2025-01-31', max_tokens=100000, temperature=1.0, cache=USE_CACHE)
o1mini = dspy.LM('openai/o1-mini-2024-09-12', max_tokens=65536, temperature=1.0, cache=USE_CACHE)
sonnet3 = dspy.LM('anthropic/claude-3-sonnet-20240229', max_tokens=4096, temperature=0, cache=USE_CACHE)
haiku35 = dspy.LM('anthropic/claude-3-5-haiku-20241022', max_tokens=8192, temperature=0, cache=USE_CACHE)
sonnet35 = dspy.LM('anthropic/claude-3-5-sonnet-20241022', max_tokens=8192, temperature=0, cache=USE_CACHE)
sonnet37 = dspy.LM('anthropic/claude-3-7-sonnet-20250219', max_tokens=64000, temperature=0, cache=USE_CACHE)
sonnet37_tmp05 = dspy.LM('anthropic/claude-3-7-sonnet-20250219', max_tokens=64000, temperature=0.5, cache=USE_CACHE)
sonnet37_tmp1 = dspy.LM('anthropic/claude-3-7-sonnet-20250219', max_tokens=64000, temperature=1.0, cache=USE_CACHE)
sonnet4 = dspy.LM('anthropic/claude-sonnet-4-20250514', max_tokens=64000, temperature=0, cache=USE_CACHE)
sonnet4_tmp05 = dspy.LM('anthropic/claude-sonnet-4-20250514', max_tokens=64000, temperature=0.5, cache=USE_CACHE)
sonnet4_tmp1 = dspy.LM('anthropic/claude-sonnet-4-20250514', max_tokens=64000, temperature=1.0, cache=USE_CACHE)
opus4 = dspy.LM('anthropic/claude-opus-4-20250514', max_tokens=32000, temperature=0, cache=USE_CACHE)
# Don't use these unless/until we get our Google API key sorted and stuffed in bashrc or whatever
gemini25flash = dspy.LM('gemini/gemini-2.5-flash-preview-05-20', max_tokens=8192, temperature=0, cache=USE_CACHE)
gemini25pro = dspy.LM('gemini/gemini-2.5-pro-preview-05-06', max_tokens=65536, temperature=0, cache=USE_CACHE)
gemini20flash = dspy.LM('gemini/gemini-2.0-flash', max_tokens=8192, temperature=0, cache=USE_CACHE)
gemini20flashlite = dspy.LM('gemini/gemini-2.0-flash-lite', max_tokens=8192, temperature=0, cache=USE_CACHE)
gemini15flash = dspy.LM('gemini/gemini-1.5-flash', max_tokens=8192, temperature=0, cache=USE_CACHE)
gemini15pro = dspy.LM('gemini/gemini-1.5-pro', max_tokens=8192, temperature=0, cache=USE_CACHE)

def find_model(model_name):
    # Get the module where `pipeline_model` is defined
    if not isinstance(model_name, str):
        return model_name
    module = sys.modules[__name__]
    model = getattr(module, model_name, None)
    if model is None:
        raise NameError(f"Model '{model_name}' not found in the module '{__name__}'")
    return model
        
@contextmanager
def pipeline_model(model_name):
    model = find_model(model_name)
    
    # Use the model in dspy.context
    with dspy.context(lm=model):
        yield

def with_escalation(models):
    def decorator(func):
        def wrapped_function(*args, **kwargs):
            return try_models(models, func, *args, **kwargs)
        return wrapped_function
    return decorator

def try_models(models, func, *args, **kwargs):
    models = [find_model(model) for model in models]
    result = None
    used_model = None
    for model in models:
        if result is not None:
            break
        print(f"Attempting {func.__name__} with model", model.model)
        try:
            with dspy.context(lm = model):
                result = func(*args, **kwargs)
                used_model = model.model
        except Exception as e:
            if "Cache Error:" in str(e):
                raise e
            else:
                print(e)
                result = None
    print("Successfully used model:", used_model)
    return result

def dispatch_and_vote(model_names, func, *args, **kwargs):
    models = [find_model(model) for model in model_names]
    result = None
    used_model = None
    for model in models:
        if result is not None:
            break
        print(f"Attempting {func.__name__} with model", model.model)
        try:
            with dspy.context(lm = model):
                result = func(*args, **kwargs)
                used_model = model.model
        except Exception as e:
            if "Cache Error:" in str(e):
                raise e
            else:
                print(e)
                result = None
    print("Successfully used model:", used_model)
    return result

def _worker(model_idx, fan_idx, model_name, func, args, kwargs, queue):
    print(f"worker {model_idx} tasks {model_name} with {repr(func)}")
    import dspy  # must re-import in subprocess
    model = find_model(model_name)
    with dspy.context(lm=model):
        result = func(*args, **kwargs)
        queue.put(((model_idx, fan_idx), result))

def try_models_parallel(model_names, func, *args, fanout=1, **kwargs):
    queue = Queue()
    processes = []

    for model_idx, model_name in enumerate(model_names):
        for fan_idx in range(fanout):
            p = Process(
                target=_worker,
                args=(model_idx, fan_idx, model_name, func, args, kwargs, queue)
            )
            p.start()
            processes.append(p)

    results = {}
    for _ in range(len(model_names) * fanout):
        key, result = queue.get()
        results[key] = result

    for p in processes:
        p.join()
        
    # Optional: sort for deterministic order
    flat_results = [results[(i, j)] for i in range(len(model_names)) for j in range(fanout)]

    return (results, flat_results)

