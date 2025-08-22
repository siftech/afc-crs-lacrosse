import dspy
import models as models
from modules.ask_yes_no_about_vulnerability import ask_yes_no_about_vulnerability
from modules.parallel_consensus import parallel_consensus
from custom_types import ScoredVulnerability
from utils import current_dspy_lm
from modules.vuln_filter import ask, ask_reliable, VOTER_POOL, SINGLE_VOTER_MODELS

def make_scored_vuln(vuln, score):
    return ScoredVulnerability(**vuln.model_dump(), score=score)

# The vuln filter returns True if the vuln passes all requirements.
@models.with_escalation(SINGLE_VOTER_MODELS)
def score_vuln(vuln, sanitizer_names):
    score = get_vuln_score(vuln, sanitizer_names)
    return make_scored_vuln(vuln, score)

# Some duplicated code below from filter_vuln but we don't have time to be elegant...
def get_vuln_score(vuln, sanitizer_names):
    print("get_vuln_score", str(vuln))

    total_score = 0
    
    score_questions = {
        "Does the vulnerability allow arbitrary code execution or file access?": 4,
        "Is it inherently unreliable to reproduce the vulnerability using a single static input, e.g. because it relies on a race condition?": -3,
        "Is the vulnerability a classic security vulnerability?": 2,
        "Is the vulnerability a critical security risk?": 2,
        "Is it likely feasible to patch the vulnerability without breaking key functionality while only editing a single file?": 3,
        "Is this primarily a thread safety concern?": -2,
        "Will Java array checking catch this vulnerability and throw an exception?": -4,
        f"Is this cyber vulnerability detectable by one of the sanitizers in the following list? Sanitizer names: {sanitizer_names}": 3,
        "Does the vulnerability involve a logic flaw (e.g., command injection, improper file handling, path traversal)?": 2,
        "Does the vulnerability involve unsafe input handling?": 2,
        "Does the vulnerability involve a backdoor?": 2,
        "Is the vulnerability's CWE from the following list of the 2024 most dangerous software weaknesses? CWEs: 79, 787, 89, 352, 22, 125, 78, 416, 862, 434, 94, 20, 77, 287, 269, 502, 200, 863, 918, 119, 476, 798, 190, 400, 306": 5,
    }

    # As a first resort, use parallel consensus with cheap fast lesser used models
    worker_models = VOTER_POOL
    worker_fanout = 1 # this is N
    
    for score_question, score in score_questions.items():
        print(f"Q: {score_question}")
        try:
            try:
                # require_agreement=True throws an error if the voters don't agree, which causes a more competent model to be queried below.
                answer = parallel_consensus(ask, worker_models, worker_fanout, score_question, vuln, require_agreement=True)
            except:
                answer = ask_reliable(score_question, vuln)
        except Exception as e:
            print("Error asking scoring question:", str(e))
            print("Defaulting to 'no'.")
            answer = False
        if answer:
            total_score += score
            print(f"Answer is yes. Added {score} to score. New score is {total_score}.")
        else:
            print(f"Answer is no. Score remains {total_score}.")

    print(f"Vuln earned total score: {total_score}.")
    return total_score
