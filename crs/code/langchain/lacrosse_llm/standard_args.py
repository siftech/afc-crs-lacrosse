import argparse
import json
import logging
import os
import re
import sys
from collections.abc import Callable, Sequence
from typing import Any, Dict, List, Literal, Optional, TextIO, Tuple, TypedDict

from langchain_core.language_models import LanguageModelInput
from langchain_core.messages import BaseMessage, MessageLikeRepresentation
from langchain_core.prompt_values import PromptValue
from langchain_core.runnables import RunnableLambda

from lacrosse_llm import LLM

ModelDesignator = Literal["ChatOpenAI", "ChatAnthropic", "ChatLiteLLM", "PromptSaver"]


class ModelSpec(TypedDict):
    klass: ModelDesignator
    model: str


ARGUMENT_MODEL_TABLE: Dict[str, ModelSpec] = {
    "chat-gpt": {"klass": "ChatLiteLLM", "model": "oai-gpt-4o"},
    "gpt-4-turbo": {"klass": "ChatLiteLLM", "model": "oai-gpt-4-turbo"},
    "claude": {"klass": "ChatLiteLLM", "model": "claude-3-opus"},
    "opus": {"klass": "ChatLiteLLM", "model": "claude-3-opus"},
    "sonnet": {"klass": "ChatLiteLLM", "model": "claude-3.5-sonnet"},
    "sonnet-3.5": {"klass": "ChatLiteLLM", "model": "claude-3.5-sonnet"},
    "haiku": {"klass": "ChatLiteLLM", "model": "claude-3-haiku"},
    "gemini": {"klass": "ChatLiteLLM", "model": "gemini-1.5-pro"},
}


def setup_args(description: Optional[str] = None) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--json-input-file", required=True, type=argparse.FileType(mode="r")
    )
    parser.add_argument(
        "--json-output-file", required=True, type=argparse.FileType(mode="w")
    )
    parser.add_argument(
        "--chain-of-thought", action="store_true", default=True, dest="chain_of_thought"
    )
    parser.add_argument(
        "--no-chain-of-thought", action="store_false", dest="chain_of_thought"
    )
    parser.add_argument(
        "--neo-fuzz", action="store_true", default=True, dest="neo_fuzz"
    )
    parser.add_argument("--no-neo-fuzz", action="store_false", dest="neo_fuzz")
    parser.add_argument(
        "--langchain_debug",
        action="store_true",
        default=False,
        dest="langchain_debug",
        help="Turn on the (quite verbose) langchain debug logger. Not universally implemented.",
    )
    parser.add_argument(
        "--standalone", action="store_true", default=False, dest="standalone"
    )

    group = parser.add_mutually_exclusive_group()

    def add_llm_arg(arg: str, klass: str, model_name: str):
        group.add_argument(
            "--" + arg, action="store_const", required=False, const=arg, dest="llm"
        )

    for arg_name, props in ARGUMENT_MODEL_TABLE.items():
        add_llm_arg(arg_name, props["klass"], props["model"])

    parser.description = description

    return parser


def save_llm_input(
        x: LanguageModelInput, file="/tmp/promptsaver.txt"
) -> LanguageModelInput:
    out: str = ""
    if isinstance(x, PromptValue):
        out = x.to_string()
    elif isinstance(x, str):
        out = x
    elif isinstance(x, Sequence):
        s: MessageLikeRepresentation
        for s in x:
            # Union[BaseMessage, Tuple[str, str], str, Dict[str, Any]]
            if isinstance(s, BaseMessage):
                out += s.pretty_repr(html=False)
            elif isinstance(s, str):
                out += s
            elif isinstance(s, tuple):
                for si in s:
                    out += si
            elif isinstance(s, dict):
                out += str(dict)
            else:
                TypeError(
                    "Expected MessageLikeRepresentation but got {}".format(type(s))
                )
    else:
        TypeError("Expected LanguageModelInput but got {}".format(type(x)))
    with open(file, "w") as f:
        f.write(out)
    return x


MockLLM = RunnableLambda


# noinspection PyUnusedLocal
def PromptSaver(model: str) -> MockLLM:
    """
    Create and return a mock LLM that returns its input, and writes it to a file.

    Debugging utility: this can be plugged into a langchain chain and used to inspect
    the prompt sent to the LLM.

    Parameters
    ----------
    model: str, ignored
      For a normal LLM constructor, this would be a model name.  Here it is
      simply ignored.

    Returns
    -------
    Mock LLM: RunnableLambda
      An object that can be substituted for an LLM in a langchain chain.
    """
    return RunnableLambda(lambda x: save_llm_input(x))


def is_anthropic_model(model: str) -> bool:
    return model.startswith("claude-")


def is_openai_model(model: str) -> bool:
    return model.rfind("gpt") != -1


def is_gemini_model(model: str) -> bool:
    return model.rfind("gemini") != -1


def init_llm(
        # model_class: ModelDesignator,
        model: str,
        competition_env: bool = True,
) -> LLM:
    api_key = None
    base_url = None
    temperature = 0
    max_tokens = 4096

    if "LITELLM_KEY" in os.environ:
        api_key = os.environ["LITELLM_KEY"]
    if api_key is None:
        if competition_env:
            raise ValueError(
                f"No API key provided; for {model} do not have appropriate API key"
            )

    if "AIXCC_LITELLM_HOSTNAME" in os.environ and competition_env:
        base_url = os.environ["AIXCC_LITELLM_HOSTNAME"]
    if base_url is None:
        if competition_env:
            raise ValueError("base_url is None")
        else:
            base_url = "http://0.0.0.0:4000"

    instance = LLM(
        api_key=api_key,
        base_url=base_url,
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        competition_env=competition_env,
    )
    return instance


def add_check_args(parser: argparse.ArgumentParser):
    parser.add_argument(
        "--just-one",
        action="store_true",
        default=False,
        dest="just_one",
        help="If checking for multiple CWEs stop after finding first one.",
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--check-all",
        action="store_true",
        default=False,
        dest="check_all",
        help="Check all top 25 CWEs",
    )
    group.add_argument("--cwe", type=str, help="Which CWE to check")


def read_json_opts(opts_file: TextIO) -> Dict[str, Any]:
    opts: Dict = json.load(opts_file)
    # prepare the opts to be acceptable to Pydantic
    for k in opts.keys():
        if opts[k] == "null":
            opts[k] = None
    return opts


def prepare(
        func_args: Optional[List[str]] = None, description: Optional[str] = None,
        setup_func: Callable = setup_args
) -> Tuple[argparse.Namespace, Dict[str, Any]]:
    """
    Generic preparatory processing for any lacrosse_llm function.
    Parameters
    ----------
    setup_func: Callable, optional
       Alternative function to configure the argument parser.
    func_args: List[str], optional
       This is a set of simulated command line arguments to be parsed.
    description: str, optional
       Description of the app to appear in the help message.

    Returns
    -------
    Tuple[Namespace, Dict]
        Returns the argparse.Namespace object resulting from parsing the command-line arguments,
        and a Dictionary of options, populated from the JSON file.
    """
    logger = logging.getLogger(sys.argv[0])
    logger.addHandler(logging.StreamHandler(sys.stdout))
    logger.debug(f"sys.argv = {sys.argv}")
    args: argparse.Namespace
    parser: argparse.ArgumentParser = setup_func(description)
    if re.search("check_file", sys.argv[0]):
        add_check_args(parser)
    if func_args is not None:
        args = parser.parse_args(func_args)
    else:
        args = parser.parse_args()
    if hasattr(args, "json_input_file"):
        input_file = args.json_input_file
        opts = read_json_opts(input_file)
        input_file.close()
    else:
        opts = {}
    if args.llm is None or args.llm not in ARGUMENT_MODEL_TABLE:
        raise ValueError(f"{args.llm} is not a valid LLM model specifier.")
    opts["model_class"] = ARGUMENT_MODEL_TABLE[args.llm]["klass"]
    opts["model"] = ARGUMENT_MODEL_TABLE[args.llm]["model"]
    llm = init_llm(opts["model"], competition_env=not args.standalone)
    opts["llm"] = llm
    return args, opts


def main(
        func: Callable[[Dict, argparse.Namespace], Dict],
        func_args: Optional[List[str]] = None,
) -> bool:
    args: argparse.Namespace
    opts: Dict[str, Any]
    args, opts = prepare(func_args)
    val: Dict[str, Any]
    try:
        val = func(opts, args)
    except Exception as e:
        print(f"Error in checking file: {e}")
        sys.exit(1)
    output_file = args.json_output_file
    json.dump(val, fp=output_file)
    output_file.close()
    return True
