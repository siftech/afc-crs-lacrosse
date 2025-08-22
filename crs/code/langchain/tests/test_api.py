# import pytest
import json
import logging
import os
import pathlib
import sys
from argparse import Namespace
from contextlib import nullcontext as does_not_raise
from tempfile import NamedTemporaryFile, TemporaryDirectory
from typing import Any, Dict, Literal

import pytest
from langchain_anthropic import ChatAnthropic
from langchain_core.language_models import BaseChatModel
from langchain_core.runnables import RunnableLambda
from langchain_openai import ChatOpenAI
from pydantic import TypeAdapter

import lacrosse_llm.confirm_vuln
import lacrosse_llm.patch
from lacrosse_llm import PatchFileInputs
from lacrosse_llm.check_file import check_file_for_cwes
from lacrosse_llm.standard_args import init_llm, main, prepare, save_llm_input


@pytest.fixture(autouse=True)
def change_test_dir(request, monkeypatch):
    monkeypatch.chdir(request.fspath.dirname)


def test_main(mocker):
    def dummy_func(opts, args):  # noqa
        return lacrosse_llm.confirm_vuln.no_vuln_resp_dict().dict()

    output_file_name = "/tmp/lax-llm-result.json"
    outfile = pathlib.Path(output_file_name)
    pathlib.Path.unlink(outfile, missing_ok=True)

    mock_argv = [
        "--json-input-file",
        os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "data/isabel-result.json"
        ),
        "--json-output-file",
        output_file_name,
        "--chat-gpt",
    ]
    mocker.patch("sys.argv", mock_argv)
    print(f"\nsys.argv = {sys.argv}")
    result = main(dummy_func, sys.argv)
    assert result
    assert os.path.exists(output_file_name)
    with open(output_file_name, "r") as file:
        written = json.load(file)
    assert not written["is_vulnerable"]


def test_init_llm():
    llm: BaseChatModel = init_llm("ChatAnthropic", "claude-3-sonnet-20240229")
    print(llm)
    assert isinstance(llm, ChatAnthropic)
    assert llm.model == "claude-3-sonnet-20240229"
    llm = init_llm("ChatOpenAI", "gpt-4-turbo-preview")
    assert isinstance(llm, ChatOpenAI)


@pytest.fixture()
def temp_dir():
    return TemporaryDirectory(prefix="lacrosse-llm-test-api", delete=False).name


@pytest.fixture()
def json_input_file(temp_dir):
    tf_obj = NamedTemporaryFile(suffix=".json", delete=False, delete_on_close=False)
    source_file = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "data/heap_overflow_01_stdin.c",
    )
    args: PatchFileInputs = PatchFileInputs(
        file=source_file,
        patch_file=os.path.join(temp_dir, "heap_overflow_01_stdin.patch"),
        patched_file=os.path.join(temp_dir, "heap_overflow_01_stdin.c"),
    )
    with open(tf_obj.name, "w") as f:
        print(args.model_dump_json(), file=f)
    return tf_obj.name


class TestPatch:
    @pytest.mark.parametrize(
        "llm",
        [
            "--sonnet",
            pytest.param("--haiku", marks=pytest.mark.xfail),
            pytest.param("--opus", marks=pytest.mark.xfail),
            pytest.param("--chat-gpt"),
        ],
    )
    def test_internal(self, temp_dir, json_input_file, llm):
        args, opts = prepare(
            [
                "--json-input-file",
                json_input_file,
                "--json-output-file",
                os.path.join(temp_dir, "json-output.json"),
                llm,
            ]
        )
        lacrosse_llm.patch.llm = opts["llm"]
        val = lacrosse_llm.patch.check_file(opts["file"])
        assert val.is_vulnerable
        assert val.fixed_code

    @pytest.mark.parametrize(
        "llm",
        [
            "--sonnet",
            pytest.param("--haiku", marks=pytest.mark.xfail),
            pytest.param("--opus", marks=pytest.mark.xfail),
            pytest.param("--chat-gpt", marks=pytest.mark.xfail),
        ],
    )
    def test_main(self, temp_dir, json_input_file, llm):
        try:
            lacrosse_llm.patch.main([
                "--json-input-file",
                json_input_file,
                "--json-output-file",
                os.path.join(temp_dir, "json-output.json"),
                llm,
            ])
        except SystemExit as e:
            print(f"Trying to patch, got system exit: {e}")
            assert False
        json_output_file = os.path.join(temp_dir, "json-output.json")
        with open(json_input_file, "r") as file:
            json_string = file.read()
        json_inputs = TypeAdapter(PatchFileInputs).validate_json(json_string)
        with open(json_output_file, "r") as file:
            results = json.load(file)
        assert results["is_vulnerable"]
        assert results["fixed_code"]
        assert os.path.exists(os.path.join(temp_dir, "json-output.json"))
        assert os.path.exists(json_inputs.patch_file)
        assert os.path.exists(json_inputs.patched_file)


class TestCheckAnyVuln:
    @pytest.mark.parametrize(
        "class_name,model_name",
        [
            ("ChatOpenAI", "gpt-3.5-turbo"),
            ("ChatAnthropic", "claude-3-sonnet-20240229"),
        ],
    )
    def test_unspecified_vuln(
            self, class_name: lacrosse_llm.standard_args.ModelDesignator, model_name: str
    ):
        if "OPENAI_API_KEY" not in os.environ:
            logging.warning(
                "Cannot run test_chat_gpt without binding envar OPENAI_API_KEY"
            )
            return False
        opts: Dict[str, Any] = {
            "llm": init_llm(class_name, model_name),
            "site": {
                "sourceFile": os.path.join(
                    os.path.dirname(os.path.abspath(__file__)),
                    "data/heap_overflow_01_stdin.c",
                )
            },
            "result": {"shortDescription": None},
        }
        args = Namespace()
        print(f"\nTrying to check file {opts['site']['sourceFile']}")
        val: dict = lacrosse_llm.confirm_vuln.confirm_vuln(opts, args)
        assert val
        assert val["is_vulnerable"]


class TestCheckSpecifiedVuln:
    @pytest.mark.parametrize(
        "llm_type",
        [
            "chat-gpt",
            "claude",
        ],
    )
    def test_main(self, mocker, llm_type):
        output_file_name: str = f"/tmp/test-confirm-vuln-{llm_type}-result.json"
        outfile = pathlib.Path(output_file_name)
        pathlib.Path.unlink(outfile, missing_ok=True)

        mock_argv = [
            "confirm_vuln",
            "--json-input-file",
            os.path.join(
                os.path.dirname(os.path.abspath(__file__)), "data/isabel-result.json"
            ),
            "--json-output-file",
            output_file_name,
            f"--{llm_type}",
        ]
        mocker.patch("sys.argv", mock_argv)
        print(f"\nsys.argv = {sys.argv}")
        result = lacrosse_llm.standard_args.main(lacrosse_llm.confirm_vuln.confirm_vuln)
        assert os.path.exists(output_file_name)
        assert result


def test_kv_prompt():
    kv = lacrosse_llm.confirm_vuln.known_vulnerability_prompt("CWE-122")
    print(kv)


def mock_llm(answer: bool):
    def mocker_save_output(x):
        save_llm_input(x)
        return f"Answer: {'YES' if answer else 'NO'}"

    return RunnableLambda(mocker_save_output)


exception = None


@pytest.mark.parametrize(
    "cwe, llm_type,answer, expectation",
    [
        pytest.param("CWE-1284", "mock", True, does_not_raise()),
        pytest.param("CWE-1284", "mock", False, does_not_raise()),
        pytest.param(
            "CWE-1284", "claude", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        pytest.param(
            "CWE-1284", "chat-gpt", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        # pytest.param(
        #     "CWE-1284",
        #     "chat-gpt", True, pytest.raises(TooManyTokensError), marks=pytest.mark.xfail
        # ),
    ]
    + [
        pytest.param("CWE-20", "mock", True, does_not_raise()),
        pytest.param(
            "CWE-20", "claude", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        pytest.param(
            "CWE-20", "chat-gpt", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        # pytest.param(
        #     "CWE-20", "chat-gpt", True, pytest.raises(TooManyTokensError), marks=pytest.mark.xfail
        # ),
    ]
    + [
        pytest.param("CWE-122", "mock", True, does_not_raise()),
        pytest.param(
            "CWE-122", "claude", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        pytest.param(
            "CWE-122", "chat-gpt", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        # pytest.param("CWE-122",
        #     "chat-gpt", True, pytest.raises(TooManyTokensError), marks=pytest.mark.xfail
        # ),
    ],
)
def test_darpa_ex_cwe(
        cwe: str,
        answer: bool,
        llm_type: Literal["mock", "chat-gpt", "claude"],
        expectation,
        shared_datadir,
):
    if llm_type == "mock":
        llm = mock_llm(answer)
    elif llm_type == "chat-gpt":
        llm = lacrosse_llm.standard_args.init_llm("ChatOpenAI", "gpt-4-turbo")
    elif llm_type == "claude":
        llm = lacrosse_llm.standard_args.init_llm(
            "ChatAnthropic", "claude-3-sonnet-20240229"
        )
    else:
        raise ValueError(f"Unknown llm type: {llm_type}")
    opts = {
        "site": {"sourceFile": str((shared_datadir / "bad_crypto.c"))},
        "language": "C",
        "llm": llm,
    }
    args = Namespace(check_all=None, cwe=cwe, few_shot=False, chain_of_thought=False)
    res = "errored"
    with expectation:
        res = check_file_for_cwes(opts, args)
    assert res != "errored"
    assert res["is_vulnerable"] == answer
    assert res["vuln_id"] == cwe


@pytest.mark.parametrize(
    "cwe, llm_type,answer, expectation",
    [
        pytest.param("CWE-1284", "mock", True, does_not_raise()),
        pytest.param("CWE-1284", "mock", False, does_not_raise()),
        pytest.param(
            "CWE-1284", "claude", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        pytest.param(
            "CWE-1284", "chat-gpt", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        # pytest.param(
        #     "CWE-1284",
        #     "chat-gpt", True, pytest.raises(TooManyTokensError), marks=pytest.mark.xfail
        # ),
    ]
    + [
        pytest.param("CWE-20", "mock", True, does_not_raise()),
        pytest.param(
            "CWE-20", "claude", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        pytest.param(
            "CWE-20", "chat-gpt", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        # pytest.param(
        #     "CWE-20", "chat-gpt", True, pytest.raises(TooManyTokensError), marks=pytest.mark.xfail
        # ),
    ]
    + [
        pytest.param("CWE-122", "mock", True, does_not_raise()),
        pytest.param(
            "CWE-122", "claude", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        pytest.param(
            "CWE-122", "chat-gpt", True, does_not_raise(), marks=pytest.mark.xfail
        ),
        # pytest.param("CWE-122",
        #     "chat-gpt", True, pytest.raises(TooManyTokensError), marks=pytest.mark.xfail
        # ),
    ],
)
def test_darpa_subset_cwe(
        cwe: str,
        answer: bool,
        llm_type: Literal["mock", "chat-gpt", "claude"],
        expectation,
        shared_datadir,
):
    if llm_type == "mock":
        llm = mock_llm(answer)
    elif llm_type == "chat-gpt":
        llm = lacrosse_llm.standard_args.init_llm("ChatOpenAI", "gpt-4o")
    elif llm_type == "claude":
        llm = lacrosse_llm.standard_args.init_llm(
            "ChatAnthropic", "claude-3-sonnet-20240229"
        )
    else:
        raise ValueError(f"Unknown llm type: {llm_type}")
    opts = {
        "site": {"sourceFile": str((shared_datadir / "bad_crypto_subset.c"))},
        "language": "C",
        "llm": llm,
    }
    args = Namespace(check_all=None, cwe=cwe, few_shot=False, chain_of_thought=True)
    res = "errored"
    with expectation:
        res = check_file_for_cwes(opts, args)
    if res == "errored" or res["is_vulnerable"] != answer:
        if getattr(res, "contents", None) is None:
            print(f"No response content in response:\n\t{res}")
        else:
            print(f"Response contents is {res['contents']}")
    assert res != "errored"
    assert res["is_vulnerable"] == answer
    assert res["vuln_id"] == cwe


def test_environ():
    assert (
            os.environ.get("OPENAI_API_KEY")
            == "sk-proj-L0xmSAQcH2gyWqujTFVJT3BlbkFJJRPSdpJTBnxSys6VxyT5"
    )
