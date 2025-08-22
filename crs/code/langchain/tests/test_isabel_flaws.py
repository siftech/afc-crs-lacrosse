import pytest

import lacrosse_llm.cwe
from lacrosse_llm import isabel_flaws

def test_rules():
    assert 'CWE-122' == isabel_flaws.cwe_equivs['HeapError']
