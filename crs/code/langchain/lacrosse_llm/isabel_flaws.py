from lacrosse_llm.isabel_rules import rules

from typing import Dict, Optional, List


# noinspection SpellCheckingInspection
def parse_rules():
    collector: List[Dict[str, str]] = []
    for _class, flaws in rules:
        collector += flaws
    return collector


Flaws = parse_rules()

short_descs = [x['short_desc'] for x in Flaws]

# noinspection SpellCheckingInspection
cwe_equivs = {"HeapError": "CWE-122"}


def lookup_vuln(claimed_vuln: str) -> Optional[str]:
    """
    Find and return the CWE identifier that corresponds to `claimed_vuln` from ISABEL (gdb).

    Parameters
    ----------
    claimed_vuln: str
      Value sent from Isabel, which has been extracted from gdb.

    Returns
    -------
    Optional[str]: CWE identifier or None.
    """
    if claimed_vuln not in short_descs:
        raise ValueError(f"Unexpected failure description: {claimed_vuln}")
    if claimed_vuln in cwe_equivs:
        return cwe_equivs[claimed_vuln]
    return None


