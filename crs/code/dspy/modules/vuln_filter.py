import dspy
import models as models
from modules.ask_yes_no_about_vulnerability import ask_yes_no_about_vulnerability
from modules.parallel_consensus import parallel_consensus
from utils import current_dspy_lm

# Use some models we don't use much elsewhere, to avoid rate limits (which are per-model IIUC).
VOTER_POOL = [models.gpt4omini, models.gpt41mini, models.haiku35]
SINGLE_VOTER_MODELS = [models.o3, models.sonnet4, models.gemini25pro, models.gpt4o, models.sonnet37, models.gemini15pro, models.haiku35, models.gpt4omini, models.gpt41mini]

def ask(filter_question, vuln):
    answer = ask_yes_no_about_vulnerability(filter_question, vuln)
    print(f"A: {answer} ({current_dspy_lm()})")
    assert answer is True or answer is False, "Answer was not True or False."
    return answer

# A slower, more reliable backup
@models.with_escalation(SINGLE_VOTER_MODELS)
def ask_reliable(filter_question, vuln):
    print("Revert to reliable individual asker.")
    return ask(filter_question, vuln)

# The vuln filter returns True if the vuln passes all requirements.
@models.with_escalation(SINGLE_VOTER_MODELS)
def vuln_filter(vuln, sanitizer_names=None):
    print("vuln_filter", str(vuln))

    must_be_true_questions = [
        #f"Is this cyber vulnerability detectable by one of the sanitizers in the following list? Sanitizer names: {sanitizer_names}",
    ]

    must_be_false_questions = [
        "Is the CWE associated with this vulnerability missing or incorrect?",
        "Does this cyber vulnerability require collection of intermediate data to compromise cryptographic security? Ie, is this only subject to prediction attacks?",
        "Would an attacker have to create any symlinks or other file system effects before trying to exploit this vulnerability?",
        "Is this vulnerability solely exploitable by timing attacks, e.g. thread safety violations, race conditions, etc.?",
        #"Would an attacker need to control environment variables to exploit this vuln?", # not sure about this one
        "Does the vulnerability relate to permitting unauthorized access to files or directories?",
        "Is this a cryptgraphic vulnerability for which an attacker would have to perform repeated operations?",
        "Is this a portability bug rather than a direct security vulnerability?",
    ]

    # As a first resort, use parallel consensus with cheap fast lesser used models
    worker_models = VOTER_POOL
    worker_fanout = 1 # this is N
    
    for filter_question in must_be_true_questions:
        print(f"Q: {filter_question}")
        try:
            try:
                # require_agreement=True throws an error if the voters don't agree, which causes a more competent model to be queried below.
                answer = parallel_consensus(ask, worker_models, worker_fanout, filter_question, vuln, require_agreement=True)
            except:
                answer = ask_reliable(filter_question, vuln)
        except Exception as e:
            print("Error asking filter question:", str(e))
            print("Defaulting to True.")
            answer = True
        if not answer:
            print("Required True. Failing the filter.")
            return False

    for filter_question in must_be_false_questions:
        print(f"Q: {filter_question}")
        try:
            try:
                # require_agreement=True throws an error if the voters don't agree, which causes a more competent model to be queried below.
                answer = parallel_consensus(ask, worker_models, worker_fanout, filter_question, vuln, require_agreement=True)
            except:
                answer = ask_reliable(filter_question, vuln)
        except Exception as e:
            print("Error asking filter question:", str(e))
            print("Defaulting to False.")
            answer = False
        if answer:
            print("Required False. Failing the filter.")
            return False

    print("Vuln passed all filter questions.")
    return True
