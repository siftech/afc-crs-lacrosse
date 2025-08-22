import argparse
import logging
import os.path
import re
import sys
from io import TextIOWrapper
from typing import Any, Dict, List, Optional, Tuple, Union

from lacrosse_llm import LLM, LLMLengthError
from lacrosse_llm.standard_args import ARGUMENT_MODEL_TABLE, init_llm
from lacrosse_llm.utils import double_braces, strip_doubles

llm: LLM
logger = logging.getLogger(__name__)
# Log to stdout so that the NeoFuzz infrastructure can find the
# output...
logger.addHandler(logging.StreamHandler(sys.stdout))
logger.setLevel(logging.INFO)

system_prompt_string: str = f"""
System: You are a security researcher, expert in detecting security vulnerabilities
and creating and testing secure code. Apply symbolic execution when possible.
Provide example inputs only in the format
[EXAMPLE]
<example input to harness>
[\\EXAMPLE]
"""

user_prompt_string: str = """
    User: The attached code is a test harness used to test a target program, as well as a patch that was applied to the target program.  I think the patch may have introduced security vulnerabilities, especially memory safety vulnerabilities.
    Please create {num_examples} inputs to the test harness to cover a wide variety of test cases
    that may expose the newly-introduced vulnerabilities in the target program.
    Only use the format
    [EXAMPLE]
    <example input to harness>
    [\\EXAMPLE]
    Do not provide any other output
    
    {code}
    """


def check_file(filename: Union[str, TextIOWrapper], patchfile: Union[str, TextIOWrapper], nex: int) -> Tuple[str, Optional[Exception]]:
    code: str
    if isinstance(filename, TextIOWrapper):
        code = filename.read()
        filename = filename.name
    else:
        try:
            with open(filename, "r") as file:
                code = file.read()
        except IOError as e:
            logger.error(f"File {filename} not found or unreadable. Error: {e}")
            exit(1)
    if isinstance(patchfile, TextIOWrapper):
        code += patchfile.read()
        patchfile = patchfile.name
    else:
        try:
            with open(patchfile, "r") as file:
                code += file.read()
        except IOError as e:
            logger.error(f"File {patchfile} not found or unreadable. Error: {e}")
            exit(1)
    resp, err = check_str(code, nex)
    return resp, err


def check_str(code: str, nex: int) -> Tuple[str, Optional[Exception]]:
    code = double_braces(code)
    if llm is None:
        raise ValueError("llm should be initialized.")
    # prompt: PromptTemplate = PromptTemplate.from_template(system_prompt_string + "\n" + user_prompt_string)
    # # system_msg = SystemMessage(content=system_prompt_string)
    # # full_prompt = PipelinePromptTemplate(final_prompt=prompt, pipeline_prompts=[("system", system_msg)])
    # resp: str = (prompt | StripDoubles | llm | StrOutputParser()).invoke(
    #     dict(code=code, num_examples=nex)
    # )

    resp, err = llm.chat_completion(
        [
            {"role": "system", "content": system_prompt_string},
            {
                "role": "user",
                "content": strip_doubles(
                    user_prompt_string.format(num_examples=nex, code=code)
                ),
            },
        ]
    )

    print(f"LLM response is:\n{resp}", file=sys.stderr)
    logger.debug(resp)
    return resp, err


def parse_response(response: str) -> List[str]:
    pattern = r"\[EXAMPLE\](.*?)\[\\EXAMPLE\]"
    matches = re.findall(pattern, response, re.DOTALL)
    return [match.strip() for match in matches]


class NonLocalExit(Exception):
    pass


def prepare(
    func_args: Optional[List[str]] = None, description: Optional[str] = None
) -> Tuple[argparse.Namespace, Dict[str, Any]]:
    """
    Generic preparatory processing for any lacrosse_llm function.
    Parameters
    ----------
    description: str
       Short description of this shell command.
    func_args: List[str], optional
       This is a set of simulated command line arguments to be parsed.

    Returns
    -------
    Tuple[Namespace, Dict]
        Returns the argparse.Namespace object resulting from parsing the command-line arguments,
        and a Dictionary of options, populated from the JSON file.
    """
    global user_prompt_string
    logger = logging.getLogger(sys.argv[0])
    logger.addHandler(logging.StreamHandler(sys.stdout))
    logger.debug(f"sys.argv = {sys.argv}")
    args: argparse.Namespace
    parser: argparse.ArgumentParser = setup_args(description)
    if func_args is not None:
        args = parser.parse_args(func_args)
    else:
        args = parser.parse_args()
    opts: Dict[str, Any] = {}
    if args.llm is None or args.llm not in ARGUMENT_MODEL_TABLE:
        raise ValueError(f"{args.llm} is not a valid LLM model specifier.")
    opts["model_class"] = ARGUMENT_MODEL_TABLE[args.llm]["klass"]
    opts["model"] = ARGUMENT_MODEL_TABLE[args.llm]["model"]
    competition_env: bool = not args.standalone
    llm = init_llm(opts["model"], competition_env=competition_env)
    opts["llm"] = llm
    return args, opts


_DESCRIPTION = """
Python script that invokes a LLM to suggest inputs that might reveal a
vulnerability.
"""


def setup_args(description: Optional[str] = None) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--patch-file", required=True, type=argparse.FileType(mode="r")
    )
    parser.add_argument(
        "--harness-file", required=True, type=argparse.FileType(mode="r")
    )
    parser.add_argument(
        "--examples", default=10, type=int, help="number of examples to generate"
    )
    parser.add_argument(
        "--output-dir",
        default="seeds",
        type=str,
        help="target directory to write seeds to",
        required=True,
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


def main(
    func_args: Optional[List[str]] = None,
) -> List[str]:
    args: argparse.Namespace
    opts: Dict[str, Any]
    logger.debug("\n\n\n" + "--- START of RUN ---" * 3)
    args, opts = prepare(func_args=func_args, description=_DESCRIPTION)
    global llm
    llm = opts["llm"]

    print(
        f"Creating {args.examples} seeds to fuzz harness '{args.harness_file}' in {args.output_dir} with model {llm}"
    )
    response, err = check_file(args.harness_file, args.patch_file, args.examples)
    try:
        examples = parse_response(response)
    except Exception as ex:
        if isinstance(err, LLMLengthError):
            raise err
        else:
            raise ex

    filenames: List[str] = []

    for i, e in enumerate(examples, start=1):
        idstring = str(i).zfill(3)
        filestring = os.path.join(args.output_dir, f"seed-{llm.model}-{idstring}")
        with open(filestring, "w") as file1:
            file1.write(e)
        filenames.append(filestring)

    return filenames


if __name__ == "__main__":
    main()
    sys.exit(0)
