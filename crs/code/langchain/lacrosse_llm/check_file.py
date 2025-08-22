import argparse
import logging
import re
import sys
from typing import Any, Dict, List, NotRequired, Optional, TypedDict, cast

import langchain.globals
from langchain_core.language_models import BaseChatModel
from langchain_core.messages import SystemMessage
from langchain_core.prompts import PromptTemplate

from lacrosse_llm.cwe import cwe_examples, find_cwe, parse_cwe, top_25_cwes
from lacrosse_llm.standard_args import main
from lacrosse_llm.utils import StripDoubles, double_braces

system_prompt_string = "You are an expert at finding security vulnerabilities in code."
system_prompt = SystemMessage(system_prompt_string)  # type: ignore

weakness_prompt_string = """
The weakness class {cwe}: {cwe_name} is defined as follows:
{cwe_description}.
"""
weakness_prompt = PromptTemplate.from_template(f"System: {system_prompt_string}\n{weakness_prompt_string}")

task_prompt = PromptTemplate.from_template("Your task is to see if a piece of code exhibits {cwe}")

example_prompt = PromptTemplate.from_template("""
Question: Does this code exhibit {cwe}?
{code}
Answer: {answer} 
""")

question_prompt_string = """
Follow the format below:

Question: QUESTION
Answer: YES or NO answer
"""

query_string = """
Question: Does the following code exhibit {cwe}?
``` {language}
{code}
```
"""
query = PromptTemplate.from_template(query_string)

# cot_prompt = SystemMessage("Reasoning: Let's think step by step in order to produce the answer.")  # type: ignore
cot_prompt_string = "Let's think step by step.\n"

llm: Optional[BaseChatModel] = None


class RespDict(TypedDict):
    is_vulnerable: bool
    vuln_id: NotRequired[str]
    vuln_name: NotRequired[str]
    explanation: NotRequired[str]
    fix: NotRequired[str]
    components: NotRequired[List['RespDict']]
    contents: NotRequired[str]


# ### FIXME: Should parameterize this so that different LLMs can be used.
# def init_llm() -> BaseChatModel:
#     global llm
#     if llm is None:
#         llm = ChatOpenAI(model='gpt-4-turbo-preview')
#     return llm
#
#
class ResponseError(Exception):
    pass


def parse_vuln_resp(contents: str) -> bool:
    m = re.search(r'^\W*Answer: +(.*)', contents, flags=re.IGNORECASE | re.MULTILINE)  # noqa
    if m is None:
        raise ResponseError(f"Could not parse Answer from:\n{contents}")
    substring: str = m.group(1)
    m = re.search(r'(yes)', substring, flags=re.IGNORECASE)
    if m is not None:
        return True
    m = re.search(r'(no)', substring, flags=re.IGNORECASE)
    if m is None:
        raise ResponseError(f"Could not parse Response from:\n{substring}")
    else:
        return False


#
#
# def parse_resp_field(contents: str, tag_name: str, multi_line: bool = False) -> str:
#     re_str = r"\[" + tag_name + r"\]" + r"\s*(.*\S)\s*" + r"\[/" + tag_name + r"\]"
#     flags = re.IGNORECASE
#     if multi_line:
#         flags = flags | re.MULTILINE | re.DOTALL
#     m = re.search(re_str, contents, flags=flags)
#     if m is None:
#         raise Exception(f"Could not parse \"{tag_name}\" field from response:\n{contents}")
#     return m.group(1)
#
#
def parse_resp_string(contents: str) -> RespDict:
    vuln: bool = parse_vuln_resp(contents)
    if not vuln:
        return RespDict(is_vulnerable=False, contents=contents)
    return RespDict(is_vulnerable=True, contents=contents)


def check_for_cwe(cwe: str, code: str, opts: Dict[str, Any], args: argparse.Namespace) -> RespDict:
    """

    Returns
    -------
    RespDict
    """
    logger = logging.getLogger("check_file")
    logger.setLevel(logging.WARN)
    logger.addHandler(logging.StreamHandler(sys.stdout))
    llm = opts['llm']
    id: int = parse_cwe(cwe)
    try:
        cwe_dict = find_cwe(cwe, top_25_cwes)
    except IndexError:
        cwe_dict = find_cwe(cwe)
    prompt_string = (system_prompt_string +
                     "\n" +
                     question_prompt_string +
                     weakness_prompt_string.format(cwe=cwe, cwe_name=cwe_dict['Name'],
                                                   cwe_description=cwe_dict['Description']))
    language: Optional[str] = opts.get('language', None)
    if language:
        examples = cwe_examples(id, language)
        if len(examples) == 1:
            prompt_string += "\nExample:\n"
        elif len(examples) >= 2:
            prompt_string += "\nExamples:\n"
        for example in examples:
            prompt_string += double_braces(example) + "\n"
    else:
        logger.warning("Programming language for target not specified.")
    if args.few_shot:
        logger.warning("Few Shot Prompting not yet implemented.")
    prompt_string += query_string
    if args.chain_of_thought:
        prompt_string += cot_prompt_string
    prompt_string += "Answer: "

    prompt = PromptTemplate.from_template(prompt_string)

    if args.langchain_debug:
        langchain.globals.set_debug(True)

    r = (prompt | StripDoubles | llm).invoke(
        dict(code=code, cwe=cwe, language=language or "")
    )

    rs: str = r if isinstance(r, str) else r.content
    rd: RespDict = parse_resp_string(rs)
    rd['vuln_id'] = cwe
    return rd


def merge_resp_dicts(rds: List[RespDict]) -> RespDict:
    if len(rds) == 1:
        return rds[0]
    is_vuln: bool = False
    vuln_id: Optional[str] = None
    for rd in rds:
        is_vuln = is_vuln or rd['is_vulnerable']
        if rd['is_vulnerable']:
            vuln_id = rd['vuln_id'] if vuln_id is None else ""
    ret = RespDict(is_vulnerable=is_vuln, components=rds)
    if vuln_id is not None and vuln_id:
        ret['vuln_id'] = vuln_id
    return ret


def check_file_for_cwes(opts: Dict[str, Any], args: argparse.Namespace) -> Dict[str, Any]:
    if not args.check_all or args.cwe:
        print(f"Error: {sys.argv[0]} requires --cwe option or --all-cwes option.")
    filename: str = opts["site"]["sourceFile"]
    code: str
    with open(filename, 'r') as file:
        code = double_braces(file.read())
    if args.check_all:
        r: List[RespDict] = []
        vuln: bool = False
        for cwe in [x['ID'] for x in top_25_cwes]:
            cr: RespDict = check_for_cwe(cwe, code, opts, args)
            r.append(cr)
            vuln = vuln or cr['is_vulnerable']
            if vuln and args.just_one:
                return cast(Dict[str, Any], merge_resp_dicts(r))
        return cast(Dict[str, Any], merge_resp_dicts(r))
    # Checking for a single CWE
    return cast(Dict[str, Any], check_for_cwe(args.cwe, code, opts, args))


if __name__ == "__main__":
    main(check_file_for_cwes)
    sys.exit(0)
