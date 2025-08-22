import pytest

import lacrosse_llm.cwe
from lacrosse_llm.cwe import top_25_cwes


def test_cwe_parse():
    assert 122 == lacrosse_llm.cwe.parse_cwe("CWE-122")
    with pytest.raises(ValueError):
        lacrosse_llm.cwe.parse_cwe("122")


def test_cwe_find():
    assert lacrosse_llm.cwe.find_cwe(122)["Name"] == "Heap-based Buffer Overflow"
    assert lacrosse_llm.cwe.find_cwe("CWE-122")["Name"] == "Heap-based Buffer Overflow"
    found = lacrosse_llm.cwe.find_cwe(1284)
    assert found
    assert found["Name"] == "Improper Validation of Specified Quantity in Input"


def test_top_25():
    assert len(lacrosse_llm.cwe.top_25_cwes) == 25


def test_cwe_c_vulns():
    has_c_examples = set(
        [
            362,
            20,
            77,
            78,
            269,
            119,
            190,
            476,
            125,
            787,
            416,
            798,
        ]
    )
    found_c_examples = {
        x["ID"]
        for x in top_25_cwes
        if lacrosse_llm.cwe.cwe_examples(x["ID"], "C", top_25_cwes)
    }
    assert found_c_examples == has_c_examples
