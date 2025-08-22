import pytest
import os
from lacrosse_llm.patch import parse_resp_field, parse_resp_string


@pytest.fixture(scope='module')
def sample_output():
    filename = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "data/patch_parse_test.txt",
    )
    with open(filename, 'r') as f:
        return f.read()


def test_parse_resp_field(sample_output):
    assert parse_resp_field(sample_output, "FIXEDCODE", multi_line=True)


def test_parse_resp_string(sample_output):
    parsed = parse_resp_string(sample_output)
    assert parsed.is_vulnerable
    assert parsed.fixed_code is not None
    assert parsed.examples is not None
