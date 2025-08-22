import importlib.resources
import re
from importlib.resources.abc import Traversable
from typing import Any, Dict, List, Optional, Tuple, TypedDict, Union, cast

import xmltodict


def parse_xml(xml_file) -> Dict[str, Any]:
    """
    Read CWE file into a (nested) dictionary and return it.

    Remove all the `"@"` prefixes to names in the dictionary.
    Parameters
    ==========
    xml_file: file
    Returns
    =======
    dict
    """

    def iterate_nested_dict(nested_dict, return_dict):
        for key, value in nested_dict.items():
            if key[0] == "@":
                key = key[1:]
            if key == "ID":
                value = int(value)
            if isinstance(value, dict):
                new_dict: Dict[str, Any] = {}
                iterate_nested_dict(value, new_dict)
                return_dict[key] = new_dict
            elif isinstance(value, list):
                newval = []
                for item in value:
                    if isinstance(item, (dict, list)):
                        d: Dict[str, Any] = {}
                        iterate_nested_dict(item, d)
                        newval.append(d)
                    else:
                        newval.append(item)
                return_dict[key] = newval
            else:
                return_dict[key] = value

    new_dict: Dict[str, Any] = {}
    raw_dict = xmltodict.parse(xml_file)
    iterate_nested_dict(raw_dict, new_dict)
    return new_dict


def load_cwes():
    with importlib.resources.files("lacrosse_llm").joinpath(
            "resources/software-development-cwes.xml"
    ).open("rb") as file:
        # noinspection SpellCheckingInspection
        cwes = parse_xml(file)['Weakness_Catalog']['Weaknesses']['Weakness']
    with importlib.resources.files("lacrosse_llm").joinpath(
            "resources/research-concepts-cwes.xml"
    ).open("rb") as file:
        # noinspection SpellCheckingInspection
        cwes += parse_xml(file)['Weakness_Catalog']['Weaknesses']['Weakness']
    return cwes


def load_top_25_cwes():
    with importlib.resources.files("lacrosse_llm").joinpath(
            "resources/cwe-top-25.xml"
    ).open("rb") as file:
        # noinspection SpellCheckingInspection
        cwes = parse_xml(file)['Weakness_Catalog']['Weaknesses']['Weakness']
    return cwes


cwes = load_cwes()
top_25_cwes = load_top_25_cwes()
Weakness = TypedDict('Weakness',
                     {
                         "ID": int,
                         "Name": str,
                         "Abstraction": str,
                         "Structure": str,
                         "Status": str,
                         "Description": str,
                         "Extended_Description": Dict[str, Any],
                         "Related_Weaknesses": Dict[str, Any],
                         "Applicable_Platforms": Dict[str, Any],
                         "Alternate_Terms": Dict[str, Any],
                         "Modes_Of_Introduction": Dict[str, Any],
                         "Likelihood_Of_Exploit": str,
                         "Common_Consequences": Dict[str, Any],
                         "Detection_Methods": Dict[str, Any],
                         "Potential_Mitigations": Dict[str, Any],
                         "Demonstrative_Examples": Dict[str, Any],
                         "Observed_Examples": Dict[str, Any],
                         "Affected_Resources": Dict[str, Any],
                         "Taxonomy_Mappings": Dict[str, Any],
                         "Related_Attack_Patterns": Dict[str, Any],
                         "References": Dict[str, Any],
                         "Mapping_Notes": Dict[str, Any],
                         "Notes": Dict[str, Any],
                         "Content_History": Dict[str, Any],
                     }
                     )
CweRegex = re.compile(r"CWE-(\d+)$", flags=re.IGNORECASE)


def parse_cwe(cwe_id: str) -> int:
    match = re.match(CweRegex, cwe_id)
    if match is None:
        raise ValueError(f"Ill-formed CWE ID: {cwe_id}")
    return int(match.group(1))


def find_cwe(id: Union[str, int], all_cwes: List[Weakness] = cwes) -> Weakness:
    weak_list = all_cwes
    if isinstance(id, str):
        id = parse_cwe(id)
    for weak in weak_list:
        if weak["ID"] == id:
            return weak
    raise IndexError(f"No CWE numbered {id} in our set of CWEs.")


def cwe_examples(id: Union[str, int], lang: str, all_cwes: List[Weakness] = top_25_cwes) -> List[str]:
    if isinstance(id, str):
        id = parse_cwe(id)
    try:
        cwe = find_cwe(id, all_cwes)
    except IndexError:
        return []
    # xmltodict imports this weirdly: we get a list of elements with a single tag, instead of
    # a list of elements under 'Demonstrative_Examples'...
    if 'Demonstrative_Examples' not in cwe:
        return []
    exs = cwe['Demonstrative_Examples']['Demonstrative_Example']
    if isinstance(exs, dict):
        exs = [exs]

    def language_match(ec) -> bool:
        if isinstance(ec, dict):
            if 'Language' in ec and ec['Language'].lower() == lang.lower():
                return True
        elif isinstance(ec, list):
            if all(map(language_match, ec)):
                return True
        else:
            TypeError(f"Unexpected ExampleCode value: {ec}")
        return False


    res = []
    for ex in exs:
        assert isinstance(ex, dict), f"{ex} is not a dict"
        if 'Example_Code' in ex:
            ec = ex['Example_Code']
            if language_match(ec):
                collected = _collect_code(ex)
                if collected is not None:
                    res.append(collected)
    return res

DemonstrativeExample = Dict[str, Dict[str, Any]]  # refine this later...


def _collect_code(ex: DemonstrativeExample) -> str:
    id = ex['Demonstrative_Example_ID']
    assert isinstance(id, str)
    bad, good = _retrieve_code_example(id)
    res: str = ""
    assert bad is not None or good is None
    if bad is None and good is None:
        return ""
    if 'Intro_Text' in ex:
        res += ex['Intro_Text']
        res += "\n"
    res += bad + "\n"
    if 'Body_Text' in ex:
        if isinstance(ex['Body_Text'], list):
            for x in ex['Body_Text']:
                res += x + "\n"
        else:
            assert isinstance(ex['Body_Text'], str)
            res += cast(str, ex['Body_Text']) + "\n"
    # both bad and good
    if good is not None:
        res += good
    return res


### Take an example ID and try to read the corresponding file out of the resources.
### Return 2 values: first a bad example string, then a good example string.  These are
### Optional, and can be None.
def _retrieve_code_example(id: str) -> Tuple[Optional[str], Optional[str]]:
    bad: Optional[str] = None
    good: Optional[str] = None
    simple: Traversable = importlib.resources.files("lacrosse_llm").joinpath(f"resources/code_examples/{id}.md")
    if simple.is_file():
        return simple.read_text(), None
    badt: Traversable = importlib.resources.files("lacrosse_llm").joinpath(f"resources/code_examples/{id}-bad.md")
    goodt: Traversable = importlib.resources.files("lacrosse_llm").joinpath(f"resources/code_examples/{id}-good.md")
    if badt.is_file():
        bad = badt.read_text()
    if goodt.is_file():
        good = goodt.read_text()
    return bad, good
