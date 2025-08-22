import re
import openai
from langchain_anthropic import ChatAnthropic
from langchain_core.messages import BaseMessage

from langchain_openai import ChatOpenAI

__all__ = ['monkeypatch_chat_gpt', 'monkeypatch_anthropic', 'TooManyTokensError']


class TooManyTokensError(Exception):
    max_tokens: int
    actual_tokens: int

    def __init__(self, max_tokens: int, actual_tokens: int):
        super().__init__(f"Too many tokens for model: {actual_tokens} > {max_tokens}")
        self.max_tokens = max_tokens
        self.actual_tokens = actual_tokens


### Monkey-patching ChatOpenAI to signal a TooManyTokensError

TOKEN_ERROR_STRING = (
    r"^"
    r"This model's maximum context length is (\d+) tokens. "
    r"However, your messages resulted in (\d+) tokens. "
    "Please reduce the length of the messages."
    r"$"
)
TOKEN_ERROR_REGEX = re.compile(TOKEN_ERROR_STRING)


def monkeypatch_chat_gpt(chat_gpt: ChatOpenAI) -> None:
    ## monkeypatch its error reporting
    # sourcery skip: use-named-expression
    old_meth = chat_gpt.invoke

    def new_meth(self, *args, **kwargs):
        # sourcery skip: raise-from-previous-error
        try:
            return old_meth(self, *args, **kwargs)
        except openai.BadRequestError as e:
            r = e.response
            dct = r.json()
            msg = dct["error"]["message"]
            if m := re.match(TOKEN_ERROR_REGEX, msg):
                raise TooManyTokensError(
                    max_tokens=int(m.group(1)), actual_tokens=int(m.group(2))
                )
            else:
                raise e

    # end-run pydantic field checking
    object.__setattr__(chat_gpt, "invoke", new_meth)


### Monkey-patching Anthropic is different, because exhausting the tokens
### does not trigger a runtime error in Anthropic's library, so we have to
### proactively watch the output.
def monkeypatch_anthropic(chat_anthropic: ChatAnthropic) -> None:
    ## monkeypatch its error reporting
    # sourcery skip: use-named-expression
    old_meth = chat_anthropic.invoke

    def new_meth(self, *args, **kwargs):
        response: BaseMessage = old_meth(self, *args, **kwargs)
        dct = response.response_metadata
        msg = dct['stop_reason']
        if m := re.match('max_tokens', msg):
            usage = dct['usage']
            raise TooManyTokensError(
                max_tokens=usage['output_tokens'], actual_tokens=usage['input_tokens'])
        return response

    # end-run pydantic field checking
    object.__setattr__(chat_anthropic, "invoke", new_meth)
