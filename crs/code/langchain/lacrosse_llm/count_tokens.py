from litellm import token_counter  # type: ignore

# from lacrosse_llm import Model, model_string
from .llm import LLM


# count the number of tokens in a string
def num_tokens_from_string(llm: LLM, string: str) -> int:
    """Returns the number of tokens in a text string."""
    return token_counter(model=llm.model, text=string)
