"""
Test input source 
"""

import pytest

from lacrosse_llm.check_input_source import get_config
from lacrosse_llm.check_input_source import InputSourceResponse
# from lacrosse_llm.check_input_source import check_input_source

# Run the following: python lacrosse_llm/check_input_source.py ../../dataset/linux-kernel/files/net-rds-recv.c "rds_inc_addref"
SAMPLE_1_RESPONSE = {
    "input_sources": {
        "Network": "TCP"
    },
    "explanation": "The function 'rds_inc_addref' is involved in managing reference counts for incoming RDS (Reliable Datagram Sockets) messages. During normal operation, inputs to this function come from network activities, specifically over TCP connections established for RDS communication. This is evident from the context of RDS being a networking protocol that operates over TCP (among other transports) for reliable message delivery."
}

# Run the following: python lacrosse_llm/check_input_source.py ../../dataset/linux-kernel/files/arch-um-kernel-exitcode.c "exitcode_proc_open"
SAMPLE_2_RESPONSE = {
    "input_sources": {
        "Syscalls from userspace": "File IO"
    },
    "explanation": "The function 'exitcode_proc_open' is part of the Linux kernel's proc file system handling, specifically for a proc file named 'exitcode'. This function is triggered when the 'exitcode' proc file is opened. The primary input source for this operation comes from user space applications performing file I/O operations (e.g., open, read, write) on the '/proc/exitcode' file. These operations are facilitated by syscalls from user space, specifically falling under the category of File I/O syscalls."
}

def test_argument_parser_default():
    """
    Test argument parser default values
    """

    arguments = ['dummy-val', 'rds_inc_addref']
    config = get_config(arguments, make_save_dir=False)

    assert config.code_file == 'dummy-val'
    assert config.function_name == 'rds_inc_addref'
    assert config.output_file == 'model-response.json'
    assert config.model == 'gpt-4-turbo-preview'
    assert config.temperature == 0.0

def test_argument_parser_non_default():
    """
    Test argument parser with non-default values
    """

    arguments = ['dummy-val', 'rds_inc_addref',
                 '--output_file', 'test-model-response.json',
                 '--model', 'claude-3',
                 '--temperature', '0.5']
    config = get_config(arguments, make_save_dir=False)

    assert config.code_file == 'dummy-val'
    assert config.function_name == 'rds_inc_addref'
    assert config.output_file == 'test-model-response.json'
    assert config.model == 'claude-3'
    assert config.temperature == 0.5

def test_response_data_structure():
    """
    Test data structure of response
    """

    response = InputSourceResponse(
        input_sources={"Network": "TCP"},
        explanation="The function 'rds_inc_addref' is involved in managing reference counts for incoming RDS (Reliable Datagram Sockets) messages. During normal operation, inputs to this function come from network activities, specifically over TCP connections established for RDS communication. This is evident from the context of RDS being a networking protocol that operates over TCP (among other transports) for reliable message delivery.")

    assert response.input_sources == SAMPLE_1_RESPONSE['input_sources']
    assert response.explanation == SAMPLE_1_RESPONSE['explanation']
