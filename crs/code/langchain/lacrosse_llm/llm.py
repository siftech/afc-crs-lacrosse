import logging
import time
import re
from sys import stderr
#from asyncio import sleep
#from time import sleep
from typing import Dict, List, Optional, Tuple

import openai

"""
Contains definition of `LLM` class to wrap the `openai.OpenAI`
and `LLMRequestError` exception.
"""

__all__ = ["LLM", "LLMRequestError", "NoChoicesError", "LLMLengthError", "ProxyMisconfigurationError"]

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


class LLMRequestError(Exception):
    pass


class NoChoicesError(LLMRequestError):
    pass


class LLMLengthError(LLMRequestError):
    pass

class ProxyMisconfigurationError(LLMRequestError):
    pass


class LLM:
    def __init__(
        self,
        *,
        model: str,
        temperature: float = 0,
        max_tokens: int = 4096,
        api_key: Optional[str] = None,
        base_url: Optional[str] = None,
        competition_env: bool = True,
    ):

        self.api_key = api_key
        self.base_url = base_url
        self.model = model
        self.temperature = temperature
        self.max_tokens = max_tokens

        self.client = openai.OpenAI(api_key=self.api_key, base_url=self.base_url)

    def chat_completion(
            self, messages: List[Dict[str, str]],
            retry: int = 5,
    ) -> Tuple[str, Optional[Exception]]:
        logger.debug(f"Starting chat_completion to {self.model}")
        start = time.time()
        try:
            logger.debug(f"Submitting a request to {self.model}")
            response = self.client.chat.completions.create(
                model=self.model,
                # the following seems to be mistyped, according to the type decls, but it works
                messages=messages,  # type: ignore
                # Only request *one* completion
                n=1,
                # this will raise a timeout exception
                # timeout=0.0001,
                temperature=self.temperature,
                max_tokens=self.max_tokens,
                # timeout=60.0,
            )
            logger.debug(f"Got response after {time.time() - start} seconds.")
            logger.debug(f"response = \n{response}")
        except openai.BadRequestError as e:
            logger.error(f"Got bad request error from LLM: {e}")
            r = e.response.json()
            logger.error(f"Error response: {r}")
            logger.error(f"Requested model name: {self.model}")
            raise e
        except openai.APIConnectionError as e:
            logger.error(f"ERROR: API Connection\n{e}")
            logger.error(f"Requested model name: {self.model}")
            raise e
        except openai.InternalServerError as e:
            logger.error(f"ERROR: InternalServerError Connection\n{e}")
            r = e.response.json()
            logger.error(f"Error response: {r}")
            logger.error(f"Requested model name: {self.model}")
            if re.match(r"VertexAIException InternalServerError - File .* was not found.", r['message']):
                logger.error("Raising ProxyMisconfigurationError.")
                raise ProxyMisconfigurationError(f"ProxyMisconfigurationError for {self.model}:\n{e}")
            raise e
        except openai.RateLimitError as e:
            logger.debug(f"Got RateLimitError after {time.time() - start} seconds.")
            # Retry after waiting a bit
            # Cause: You have hit your assigned rate limit.
            # Solution: Pace your requests. Read more in our Rate limit guide.
            # Handle rate limit error (we recommend using exponential backoff)
            # https://platform.openai.com/docs/guides/rate-limits
            if retry >= 60: # don't wait for more than a minute -- give up
                logger.error(f"ERROR: Rate Limit Error: {e} retries have failed.")
                raise e
            else:
                logger.error(f"Got rate limit error. Waiting {retry} seconds and retrying.")
                time.sleep(retry)
                return self.chat_completion(messages, retry=retry * 2)

        end = time.time()
        logger.info(f"Completion time: {end - start:.2f}s")

        if len(response.choices) < 1:
            raise NoChoicesError("LLM chat completion did not return any choices")

        choice = response.choices[0]
        if choice.finish_reason == "length":
            # there may be salvageable examples here...
            # FIXME: should annotate this somehow to show that it didn't work
            # as expected.
            if choice.message.content is not None:
                return choice.message.content, LLMLengthError(
                    f"LLM finished for reason of length, trying to recover content:\n{choice}"
                )
            else:
                raise LLMLengthError(
                    f"LLM finished for reason of length, but content not recoverable:\n{choice}"
                )
        elif choice.finish_reason != "stop":
            raise LLMRequestError(
                f"LLM finished for reason other than 'stop': {choice.finish_reason}"
            )
        elif choice.message.content is None:
            raise LLMRequestError(
                f"LLM request completed with no message in expected location: {response}"
            )

        return choice.message.content, None
