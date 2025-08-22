#!/usr/bin/env python3

import argparse
import logging
import os
import re
import sys
import time
from typing import Any, Dict, List, Optional

from lacrosse_llm import LLM
from lacrosse_llm.standard_args import ARGUMENT_MODEL_TABLE, prepare

gpt_models = ['chat-gpt', 'claude', 'gpt', 'gpt-4o', '4o']
claude_models = ['opus', 'sonnet', 'haiku', 'sonnet-3.5']
model_choices = gpt_models + claude_models
MAX_TOKENS = 4096

logger = logging.getLogger(__name__)
# Log to stdout so that the NeoFuzz infrastructure can find the
# output...
logger.addHandler(logging.StreamHandler(sys.stdout))
logger.setLevel(logging.INFO)

system_prompt_string: str = r"""
System: You are a security researcher, expert in detecting security vulnerabilities
and creating and testing secure code. Apply symbolic execution when possible.
"""

user_prompt_string = """
User: Analyze the code for high probability vulnerabilities and write a well formed
git patch that eliminates the vulnerability.
The original file is {filename}
Respond in the following format with no other output
[PATCH]
<unix patch file>
[\\PATCH]
Do not provide any other output
"""


# Load a source file return it as a string
def load_source_file(filename: str) -> str:
    code: str
    try:
        with open(filename, 'r') as file:
            code = file.read()
    except IOError as e:
        logger.error(f"ERROR opening {filename}: {e}.")
        raise e
    return code


def generate_patch(llm: LLM, code: str, filename: str) -> str:
    logger.info(f"generate_patch: llm: {LLM}  filename: {filename}")
    basename, suffix = os.path.splitext(os.path.basename(filename))
    full_user_prompt = user_prompt_string.format(filename=filename, basename=basename, suffix=suffix) + "\n" + code
    logger.info(f"full_user_prompt: {full_user_prompt}")
    input_message = [
        {
            "role": "system",
            "content": system_prompt_string
        },
        {
            "role": "user",
            "content": full_user_prompt
        }
    ]
    repair_resp: str
    err: Optional[Exception] = None
    repair_resp, err = llm.chat_completion(input_message)
    logger.info(f"LLM patch gen response is:\n{repair_resp}")

    if err is not None:
        logger.error(f"LLM error is: {err}; can't recover")
        raise err

    return repair_resp


def parse_response(response: str) -> List[str]:
    pattern = r'\[PATCH\](.*?)\[\\PATCH\]'
    # md_pattern = re.compile(r'^```(?:\w+)?\s*\n(.*?)(?=^```)```', re.MULTILINE | re.DOTALL)
    md_pattern = r'```(?:\w+)?\s*\n(.*?)(?=^```)```'

    matches = re.findall(pattern, response, re.DOTALL)
    logger.debug(f"Parsing response, found {len(matches)} matches:")
    vals = []
    for match in matches:
        logger.debug(f'"""\n{match}\n"""')
        match = match.strip()
        mdmatch = re.match(md_pattern, match, re.MULTILINE | re.DOTALL)
        if mdmatch:
            logger.debug("Found markdown")
            vals.append(mdmatch.group(1))
        else:
            logger.debug("No markdown")
            vals.append(match)

    return [val.strip() for val in vals]


def setup_args(description: Optional[str] = None) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--filename", required=True, type=str, help="The name of the file to patch"
    )
    parser.add_argument(
        "--chain-of-thought", action="store_true", default=True, dest="chain_of_thought"
    )
    parser.add_argument(
        "--no-chain-of-thought", action="store_false", dest="chain_of_thought"
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


def generate_patch_directly(llm: LLM, filename: str, patch_path: str) -> Optional[str]:
    val: str
    code = load_source_file(filename)
    start_time = time.time()
    try:
#        val = generate_patch(llm, code, filename=filename)
        val = generate_patch(llm, code, filename=patch_path)
    except Exception as e:
        logger.error(f"Error in checking file: {e}")
        raise e
    elapsed_time = time.time() - start_time
    logger.info(f"Patch generation from {llm.model} took {elapsed_time:.2f} seconds")
    patch = parse_response(val)
    if len(patch) >= 1:
        logger.info(f"Patch value is:\n{patch[0]}")
        return patch[0]

    logger.warning("Got an un-parseable response.")
    rev_patch: List[str] = parse_response(repair_response(llm, val))
    if len(rev_patch) >= 1:
        logger.info(f"Repaired patch value is:\n{rev_patch[0]}")
        return rev_patch[0]

    logger.warning(f"No patch found")
    return None

reformat_user_prompt_string = """
User: I asked you to provide a patch file to fix some code, in between two tags, like this:
[PATCH]
<unix patch file>
[\\PATCH]
You gave me the following output, which is not in the correct format:
[CHAT RESPONSE]
{response}
[\\CHAT RESPONSE]
Please reformat your response in between [PATCH] and [\\PATCH]:
"""

def repair_response(llm: LLM, response: str) -> str:
    full_user_prompt = reformat_user_prompt_string.format(response=response)
    input_message = [
        {
            "role": "system",
            "content": system_prompt_string
        },
        {
            "role": "user",
            "content": full_user_prompt
        }
    ]
    repair_resp: str
    err: Optional[Exception] = None
    repair_resp, err = llm.chat_completion(input_message)
    logger.info(f"LLM response repair is:\n{repair_resp}")

    if err is not None:
        logger.error(f"LLM error is: {err}; can't recover")
        raise err

    return repair_resp


def main(func_args: Optional[List[str]] = None) -> bool:
    args: argparse.Namespace
    opts: Dict[str, Any]
    args, opts = prepare(func_args, setup_func=setup_args)
    patch = generate_patch_directly(opts["llm"], args.filename)
    return True


if __name__ == '__main__':
    main()
    sys.exit(0)
