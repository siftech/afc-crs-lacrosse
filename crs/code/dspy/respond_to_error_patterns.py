import time
import random
import re
from typing import Callable, Any, List, Dict

# Sample pattern_responses list
#pattern_responses = [
#    {
#        "pattern": r"TLS handshake",
#        "retry": {
#            "min_delay": 1,
#            "max_delay": 3,
#            "max_attempts": 3
#        },
#        "raise_exception": RuntimeError("Too many TLS failures")
#    },
#    {
#        "pattern": r"Rate limit",
#        "retry": {
#            "min_delay": 2,
#            "max_delay": 5,
#            "max_attempts": 2
#        }
#        # No raise_exception: original will be re-raised after retries
#    },
#    {
#        "pattern": r"Authentication error",
#        "raise_exception": RuntimeError("Missing API key")
#        # No retry specified, so the custom exception gets raised straight away
#    }
#]

def respond_to_error_patterns(
        pattern_responses: List[Dict],
        func: Callable,
        *args,
        **kwargs
) -> Any:
    attempt_counts = {id(r): 0 for r in pattern_responses}
        
    while True:
        try:
            return func(*args, **kwargs)
        
        except Exception as e:
            matched = None
            for response in pattern_responses:
                if re.search(response["pattern"], str(e)):
                    matched = response
                    break
            
            if not matched:
                raise  # Not a handled error pattern

            retry_conf = matched.get("retry")
            raise_exc = matched.get("raise_exception")
            r_id = id(matched)
            attempt_counts[r_id] += 1
            attempt_num = attempt_counts[r_id]

            if retry_conf:
                if attempt_num > retry_conf.get("max_attempts", 0):
                    print(f"respond_to_error_patterns giving up after {attempt_num} tries.")
                    if raise_exc:
                        raise raise_exc from e
                    else:
                        raise  # Give up after max retries
                delay = random.uniform(retry_conf["min_delay"], retry_conf["max_delay"])
                print(f"[Retry {attempt_num}] Matched pattern '{matched['pattern']}'. Sleeping {delay:.2f}s...")
                time.sleep(delay)
                print(f"respond_to_error_patterns woke up.")
            else:
                if raise_exc:
                    raise raise_exc from e
                else:
                    raise  # No retry, no custom raise: just re-raise
                    
