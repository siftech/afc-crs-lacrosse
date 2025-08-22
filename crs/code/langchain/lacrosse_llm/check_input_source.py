"""
Script to run LangChain to check input source of
a function.
"""

import argparse
import os
import json
import sys
from typing import cast
from datetime import datetime
from pathlib import Path

from copy import deepcopy

from langchain.output_parsers import PydanticOutputParser
from langchain_core.output_parsers.pydantic import PydanticBaseModel
from langchain_core.pydantic_v1 import BaseModel, Field

from langchain_core.language_models import BaseChatModel
# from langchain_core.messages import BaseMessage
# from langchain_core.prompts import PromptTemplate
from langchain_core.prompts import ChatPromptTemplate
from langchain_openai import ChatOpenAI

from lacrosse_llm.utils import double_braces

INPUT_SOURCE_SYSTEM_PROMPT_STR: str = """
System: You are a security researcher, expert in understanding security vulnerabilities.
An supertype-subtype hierarchy of input sources has been provided:
- Supertype: Network -> Subtype(s): Ethernet, TCP, UDP
- Supertype: TTY
- Supertype: USB
- Supertype: PCI
- Supertype: Block devices
- Supertype: Syscalls from userspace -> Subtype(s): File IO, Process management

Provide responses in the following format:
{format_instructions}

When providing input sources, provide the hierarchy path to the most specific applicable input source.
Do not include unnecessary information in your response.
"""

INPUT_SOURCE_USER_PROMPT_STR: str = """
What input sources provide inputs to {function} during normal operation?
{code_file}
Response:
"""


class InputSourceResponse(BaseModel):
    """
    Response for Input Source Query
    """
    input_sources: dict[str, str] = Field(
        description=('Input source dictionary for the provided function where '
                     'key is <SUPERTYPE> and value is <SUBTYPE> '
                     '(set <SUBTYPE> to NONE if no subtype exists).'
                     )
    )
    explanation: str = Field(
        description='Explanation for input sources for the provided function')


def init_llm(config: argparse.Namespace) -> BaseChatModel:
    """
    Init LLM
    """
    llm = ChatOpenAI(model=config.model, temperature=config.temperature)
    return llm


def read_file(filename: str):
    """
    Read code file
    """
    code: str
    with open(filename, 'r', encoding='utf-8') as file:
        code = file.read()
    return code


def check_input_source(code: str, function_name: str, config: argparse.Namespace) -> InputSourceResponse:
    """
    Check input source of `function_name` in `code`
    """
    llm = init_llm(config)
    parser = PydanticOutputParser(pydantic_object=InputSourceResponse) # type: ignore

    code = double_braces(code)

    input_source_chat_prompt: ChatPromptTemplate = ChatPromptTemplate.from_messages(
        [
            ('system', INPUT_SOURCE_SYSTEM_PROMPT_STR),
            ('human', INPUT_SOURCE_USER_PROMPT_STR),
        ])

    chain = input_source_chat_prompt | llm | parser
    response: InputSourceResponse = chain.invoke(
        {'function': function_name, 'code_file': code,
         'format_instructions': parser.get_format_instructions()})

    print(f"Response: {response}")

    return response


def print_response(json_obj: dict):
    """
    Print response from LLM
    """
    for key, value in json_obj.items():
        print(f"{key} - {value}")


def get_config(arguments=None, make_save_dir: bool = True):
    """
    Get config
    """

    parser = argparse.ArgumentParser('Check code for vulnerabilities and patch')
    parser.add_argument('code_file', type=str, help='Code file')
    parser.add_argument('function_name', type=str, help='Function to check')
    parser.add_argument('--output_file', type=str, default='model-response.json',
                        help='JSON file for model response')
    parser.add_argument('--model', default='gpt-4-turbo-preview', type=str,
                        help="Model name")
    parser.add_argument('--temperature', default=0.0, type=float,
                        help="LLM temperature")
    config = parser.parse_args(args=arguments)

    if make_save_dir:
        # Create a save directory
        config.save_dir = Path(f"results-{datetime.now().isoformat()}".replace(':', '-'))
        print(f"Creating a save directory: {config.save_dir}")

        if not os.path.exists(config.save_dir):
            os.makedirs(config.save_dir)

        with open(f"{config.save_dir}/experiment-config.json", 'w', encoding='utf-8') as f:
            json_data = deepcopy(vars(config))
            json_data['save_dir'] = str(json_data['save_dir'])
            json.dump(json_data, f, indent=4)

    return config


def main():
    """
    Main function
    """
    config = get_config(make_save_dir=True)

    try:
        code = read_file(config.code_file)
        resp = check_input_source(code, config.function_name, config)
        # print_response(resp)

        with open(f"{config.save_dir}/{config.output_file}", 'w', encoding='utf-8') as f:
            json.dump(json.loads(resp.json()), f, indent=4)

    except Exception as e:
        print(f"Error in checking file: {e}")
        sys.exit(1)
    # print(json.dumps(val))


if __name__ == '__main__':
    main()
    sys.exit(0)
