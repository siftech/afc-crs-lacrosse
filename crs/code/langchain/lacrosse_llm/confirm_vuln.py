import argparse
import sys
from typing import Optional, Dict, Any, cast

from langchain_core.language_models import BaseChatModel
from langchain_core.output_parsers import StrOutputParser
from langchain_core.prompts import BasePromptTemplate, PromptTemplate
from langchain_core.pydantic_v1 import BaseModel

import lacrosse_llm.cwe
from lacrosse_llm.standard_args import main
from lacrosse_llm.utils import double_braces, StripDoubles, parse_resp_field, parse_vuln_resp
import lacrosse_llm.isabel_flaws as isabel_flaws

vuln_prompt_string: str = """
System: You are a security researcher, expert in detecting security vulnerabilities.
Provide response only in following format: 
[VULNERABLE] <YES or NO> [/VULNERABLE]
[VULNERABILITY TYPE] <CVE ID if known, otherwise "Unknown"> [/VULNERABILITY TYPE]
[VULNERABILITY NAME] <CVE NAME if CVE ID is known, otherwise "Unknown"> [/VULNERABILITY NAME]
[WEAKNESS TYPE] <CWE ID if known, otherwise "Unknown"> [/WEAKNESS TYPE]
[WEAKNESS NAME] <CWE NAME if CWE ID is known, otherwise "Unknown"> [/WEAKNESS NAME]
[EXPLANATION] <explanation for prediction> [/EXPLANATION]

Use N/A in fields other than \"VULNERABLE\" if there are no
vulnerabilities. Do not include anything else in response.

User: Is the following code snippet prone to any security vulnerability?
{code_snippet}
Response:
"""
vuln_prompt: PromptTemplate = PromptTemplate(
    input_variables=["code_snippet"], template=vuln_prompt_string
)

known_vuln_prompt_string: str = """
System: You are a security researcher, expert in detecting security vulnerabilities.
You are told that the following code snippet exhibits Common Weakness Enumeration 
flaw {cwe_id}, \"{cwe_name}\" described as follows:
\"{cwe_description}\".

Provide response only in following format: 
[VULNERABLE] <YES or NO> [/VULNERABLE]
[EXPLANATION] <explanation for prediction> [/EXPLANATION]

Use N/A in fields other than \"VULNERABLE\" if there are no
vulnerabilities. Do not include anything else in response.

User: Does the following code snippet exhibit weakness {cwe_id}? 
{code_snippet}
Response:
"""
known_vuln_prompt: PromptTemplate = PromptTemplate(
    input_variables=["code_snippet", "cwe_id", "cwe_name", "cwe_description"], template=known_vuln_prompt_string
)

llm: Optional[BaseChatModel] = None


class RespDict(BaseModel):
    is_vulnerable: bool
    vuln_id: Optional[str]
    vuln_name: Optional[str]
    weakness_id: Optional[str]
    weakness_name: Optional[str]
    explanation: Optional[str]


def no_vuln_resp_dict() -> RespDict:
    return RespDict(
        is_vulnerable=False,
        vuln_id=None,
        vuln_name=None,
        weakness_id=None,
        weakness_name=None,
        explanation=None,
    )


def parse_resp_string(contents: str) -> RespDict:
    vuln: bool = parse_vuln_resp(contents)
    if not vuln:
        return no_vuln_resp_dict()
    return RespDict(
        is_vulnerable=True,
        vuln_id=parse_resp_field(contents, "VULNERABILITY TYPE", optional=True),
        vuln_name=parse_resp_field(contents, "VULNERABILITY NAME", optional=True),
        weakness_id=parse_resp_field(contents, "WEAKNESS TYPE", optional=True),
        weakness_name=parse_resp_field(contents, "WEAKNESS NAME", optional=True),
        explanation=parse_resp_field(contents, "EXPLANATION", multi_line=True),
    )


def check_file(filename: str, cwe_id: Optional[str] = None) -> RespDict:
    code: str
    with open(filename, "r") as file:
        code = file.read()
    return check_str(code, cwe_id=cwe_id)


def confirm_vuln(opts: Dict[str, Any], _args: argparse.Namespace) -> Dict:
    global llm
    llm = opts["llm"]
    vuln_id: Optional[str] = None
    try:
        results: Dict[str, Any] = opts['result']
        if 'shortDescription' in results:
            claimed_vuln: str = results['shortDescription']
            if claimed_vuln is not None:
                vuln_id = isabel_flaws.lookup_vuln(claimed_vuln)
    except IndexError:
        print("WARN: JSON object did not have results, or did not have shortDescription in results.")
    val: RespDict
    try:
        val = check_file(opts["site"]["sourceFile"], cwe_id=vuln_id)
    except Exception as e:
        print(f"Error in checking file: {e}")
        sys.exit(1)
    print(f"TASK RESULT: val.is_vulnerable = {val.is_vulnerable}")
    print("TASK RESULT: DONE")
    return val.dict()


def known_vulnerability_prompt(cwe_id: str) -> BasePromptTemplate:
    cwe: lacrosse_llm.cwe.Weakness = lacrosse_llm.cwe.find_cwe(cwe_id)
    return known_vuln_prompt.partial(cwe_id=cwe_id). \
        partial(cwe_name=cwe['Name']). \
        partial(cwe_description=cwe['Description'])


def check_str(code: str, cwe_id: Optional[str] = None) -> RespDict:
    code = double_braces(code)
    if llm is None:
        raise ValueError("llm should be initialized.")
    prompt: PromptTemplate
    if cwe_id is None:
        prompt = vuln_prompt
    else:
        prompt = cast(PromptTemplate, known_vulnerability_prompt(cwe_id))
    repair_resp: str = (prompt | StripDoubles | llm | StrOutputParser()).invoke(
        dict(code_snippet=code)
    )
    print(f"Vulnerability response is:\n{repair_resp}", file=sys.stderr)
    return parse_resp_string(repair_resp)


if __name__ == "__main__":
    main(confirm_vuln)
    sys.exit(0)
