import os
import re
from typing import Optional, Union

from langchain_anthropic import ChatAnthropic
from langchain_community.chat_models import ChatLiteLLM
from langchain_core.prompt_values import StringPromptValue
from langchain_core.runnables import RunnableLambda
from langchain_openai import ChatOpenAI

Model = Union[ChatOpenAI, ChatAnthropic, ChatLiteLLM]


def model_string(model: Model) -> str:
    if isinstance(model, ChatOpenAI):
        return model.model_name
    else:
        return model.model


def double_braces(code: str) -> str:
    """
    Returns a copy of `code` with all single braces replaced with
    doubles (for use in langchain).
    """
    return re.sub(r"}", r"}}", re.sub(r"{", r"{{", code))


def strip_doubles(code):
    return re.sub(r"}}", r"}", re.sub(r"{{", r"{", code))


def prompt_strip_doubles(prompt_value) -> StringPromptValue:
    assert isinstance(prompt_value, StringPromptValue)
    if hasattr(prompt_value, "model_copy"):
        x = prompt_value.model_copy()
    else:
        x = prompt_value.copy()
    x.text = strip_doubles(x.text)
    return x


StripDoubles = RunnableLambda(lambda x: prompt_strip_doubles(x))

VulnResponseRegex = re.compile(
    r"\[VULNERABLE\](.*)\[/VULNERABLE\]", flags=re.IGNORECASE
)  # noqa


def parse_vuln_resp(contents: str) -> bool:
    m = re.search(VulnResponseRegex, contents)
    if m is None:
        raise Exception(f'Could not parse "VULNERABLE" field from:\n{contents}')
    substring: str = m.group(1)
    m = re.search(r"(yes)", substring, flags=re.IGNORECASE)
    if m is None:
        m = re.search(r"(no)", substring, flags=re.IGNORECASE)
        if m is None:
            raise Exception(
                f'Could not parse "VULNERABLE" field value from:\n{substring}'
            )
        else:
            return False
    else:
        return True


def parse_resp_field(
    contents: str, tag_name: str,
    multi_line: bool = False,
    optional: bool = False
) -> Optional[str]:
    re_str = r"\[" + tag_name + r"\]" + r"\s*(.*\S)\s*" + r"\[/" + tag_name + r"\]"
    flags = re.IGNORECASE
    if multi_line:
        flags = flags | re.MULTILINE | re.DOTALL
    m = re.search(re_str, contents, flags=flags)
    if m is None:
        if optional:
            return None
        else:
            raise ValueError(
                f'Could not parse "{tag_name}" field from response:\n{contents}'
            )
    return m.group(1)


def find_git_root(p: str) -> str:
    "From the pathname `p`, find the parent git repository."
    d = os.path.dirname(os.path.abspath(p))
    while d:
        if os.path.isdir(os.path.join(d, ".git")):
            return d
        d = os.path.dirname(d.rstrip("/"))
    raise ValueError(
        f"No parent git repo found for {p} interpreted as {os.path.abspath(p)}."
    )

def mkstemp_patch_file(
    basename: str,
    llm: str,
    patch_type: str,
    output_dir: str,
    ) -> str:
    """
    Create a temporary file for a patch and return its path (closing the file descriptor).
    Required args are basename (str), output_dir (str), and patch_type (str).
    """
    import tempfile
    filename=f"{basename}_{llm}_{patch_type}_"
    fd, path = tempfile.mkstemp(prefix=filename, suffix=".patch", dir=output_dir)
    os.close(fd)
    return path
