import os
import json
from modules.parse_sanitizer import parse_sanitizer_output
from modules.characterize_bug import patch_bug, diagnose_bug, critique_commit
from utils import place_source_path
from code_scan import read_file_lines, extract_function, get_code_region
from diff_scan import extract_function_changes
from record import record
import git_fns
from patching import diff2patch
import importlib
import dspy
from models import pipeline_model, find_model, try_models
from record import record


# NOTE: This file is still (barely) outside the flow that would actually be used by LAX.
# It is part of the testing flow.
# What we're primarily doing in this layer is preparing/constraining inputs and applying the
# prescribed model as DSPy's default model.
# Also note, top to bottom this file is a basically a historical progression of how this
# harness worked, so the only relevant harness code is at the bottom--
# generate_patch_with_inputs at time of writing.

# The top-level of what would be used by lacrosse is what this calls:
# currently the generate_patch function in some pipeline file (see ./pipelines)
# (pipelines might be cleaner as classes but hey we're flying by the seat of our patch-pants here...)
# Note: As of now, the calling lax code will need to set a default LLM using our `with pipeline_model('gtp4o')` like below or dspy's built-in `dspy.context(lm=model)`.

fake_files_to_patch = {
    "d030af5eb4c64470c8fd5a87a8f6aae547580aa3": ["src/http/ngx_http_request.c"],
    "0dbd46415432759475e6e4bb5adfaada6fb7d506": ["src/http/ngx_http_core_module.c"],
    "c502a1695c0e9d0345101a5f2a99ee0e3c890a4d": ["src/http/ngx_http_request.c"],
    "b9d6a2caf41565fb05c010ad0c8d2fd6bd3c4c42": ["src/http/ngx_http_core_module.c"],
    "b101d59b3dda654dee1deabc34816e2ca7c96d38": ["src/core/ngx_cycle.c", "src/http/ngx_http_variables.c"],
    "cf6f5b1d4d85c98b4e2e2fb6f694f996d944851a": ["src/mail/ngx_mail_pop3_handler.c"],
    "cc4b16fc10dcc579d5f697f3ff70c390b5e7c7d2": ["src/core/ngx_cycle.c"],
    "dcf9f055bf1863555869493d5ac5944b5327f128": ["src/http/ngx_http_request.c"],
    "a2f5fad3ef16615ed23d21264560748cdc21a385": ["src/core/ngx_cycle.c", "src/http/ngx_http_variables.c"],
    "348b50dbb52e7d6faad7d75ce9331dd9860131c4": ["src/os/unix/ngx_linux_sendfile_chain.c"],
    "316d57f895c4c915c5ce3af8b09972d47dd9984e": ["src/mail/ngx_mail_pop3_handler.c"],
    "9c5e32dcd9779c4cfe48c5377d0af8adc52a2be9": ["src/http/ngx_http_script.c"],
    "ef970a54395324307fffd11ab37266479ac37d4c": ["src/http/modules/ngx_http_userid_filter_module.c"],
    "b6c0a37554e300aa230ea2b8d7fe53dd8604f602": ["src/mail/ngx_mail_smtp_handler.c"],
}

# This entry point is still specific to the semi-finals way of doing things, as it takes bic stuff.
# Here we should use the bic hash to extract the relevant code that the delta applies to.
# Even after that, there remains a divergence between the old way of doing things and the new (simpler) way:
# In the old way the version of the code that the buggy delta applies to is not the same as the version that the
# patch is meant to apply to, whereas in the newer paradigm they are the same.
# Ideally the functions we develop here, at some high level, should work with the old way but subsume the new way,
# e.g. by passing the same code base as two separate arguments.
def generate_patch_for_bic(pipeline_name, model_name, bic_sanitizer_output, head_sanitizer_output, code_base_path, cp_base_path, bic_hash, bic_delta_path, vuln, harness):
    print("generate_patch_for_bic", pipeline_name, model_name)
    # print("bic_hash=", bic_hash)
    
    with open(bic_delta_path, 'r') as f:
        bic_delta = f.read()

    # For now let's assume the file-to-patch is at the top of the stack trace
    stack_trace, harness_call, bug_summary, bug_hint = parse_sanitizer_output(bic_sanitizer_output)
    top_func, top_path, top_line, top_char = stack_trace[0]
    actual_top_path, rel_top_path = place_source_path(top_path, code_base_path)
    
    fake_f2p = fake_files_to_patch[bic_hash]
    assert len(fake_f2p) == 1, f"The fake file to patch is not a single file"
    fake_f2p = fake_f2p[0]
    if fake_f2p != rel_top_path:
        print(f"WARNING: The top path in the stack trace is not the fake file-to-patch. top_path={rel_top_path}, fake_F2P={fake_f2p}")
    file_to_patch = fake_f2p
    #print("file_to_patch=", file_to_patch)
    print("code_base_path=", code_base_path)
    cp_code_base_path = os.path.join(cp_base_path, "src/nginx")
    print("cp_code_base_path=", cp_code_base_path)
    actual_f2p_path, rel_f2p_path = place_source_path(file_to_patch, code_base_path)
    actual_f2p_cp_path, rel_f2p_cp_path = place_source_path(file_to_patch, cp_code_base_path)
    print("actual_f2p_path=", actual_f2p_path)
    print("rel_f2p_path=", rel_f2p_path)
    print("actual_f2p_cp_path=", actual_f2p_cp_path)
    print("rel_f2p_cp_path=", rel_f2p_cp_path)
    
    bic_codelines = git_fns.on_repo_at_commit(code_base_path, bic_hash, lambda repo_path: read_file_lines(actual_f2p_path))

    head_codelines = read_file_lines(actual_f2p_cp_path)

    # print("bic_codelines len", len(bic_codelines))
    # print("head_codelines len", len(head_codelines))

    bic_ftp_code = "".join(bic_codelines)
    head_ftp_code = "".join(head_codelines)

    print("code_base_path=", code_base_path)

    return generate_patch(pipeline_name, model_name, head_sanitizer_output, head_ftp_code, cp_code_base_path, file_to_patch, vuln, harness, bic_sanitizer_output, bic_ftp_code, bic_delta)


def generate_patch(pipeline_name, model_name, head_sanitizer_output, head_ftp_code, code_base_path, file_to_patch, vuln=None, harness=None, bic_sanitizer_output=None, bic_ftp_code=None, bic_delta=None):
    print("generate_patch", pipeline_name, model_name)
    assert head_sanitizer_output is not None, "gen_patch: head_sanitizer_output arg is None!"
    assert head_ftp_code is not None, "gen_patch: head_ftp_code arg is None!"

    # Here we use the pipeline_name to dispatch to different modules.
    try:
        pipeline_module = importlib.import_module(f"pipelines.{pipeline_name}")
        with pipeline_model(model_name):
            patch = pipeline_module.generate_patch(head_sanitizer_output, head_ftp_code, code_base_path, file_to_patch, vuln, harness, bic_sanitizer_output, bic_ftp_code, bic_delta)
    except ImportError:
        raise ValueError(f"Module {pipeline_name} not found. We expect there to be a module named the same thing as the pipeline.")
    except AttributeError:
        raise ValueError(f"Module {pipeline_name} does not have a 'generate_patch' function. We expect it to.")
    except Exception as e:
        raise e

    patch_name = f"{pipeline_name}_{model_name}"
    record(f"{patch_name}_patch", patch, False)
    
    return patch


def generate_patch_for_codebase(codebase_path, pipeline_name, model_name="gpt4o", head_sanitizer_output=None, delta=None, vuln=None, harness=None):
    print("generate_patch_no_ftp", pipeline_name, model_name)

    # Here we use the pipeline_name to dispatch to different modules.
    try:
        pipeline_module = importlib.import_module(f"pipelines.{pipeline_name}")
        with pipeline_model(model_name):
            patch = pipeline_module.generate_patch_for_codebase(codebase_path, head_sanitizer_output, delta, vuln, harness)
    except ImportError:
        raise ValueError(f"Module {pipeline_name} not found. We expect there to be a module named the same thing as the pipeline.")
    except AttributeError:
        raise ValueError(f"Module {pipeline_name} does not have a 'generate_patch_no_ftp' function. We expect it to.")
    except Exception as e:
        raise e

    patch_name = f"{pipeline_name}_{model_name}"
    record(f"{patch_name}_patch", patch, False)
    
    return patch

# NOTE: We currently assume that the inputs will ALWAYS include code.
def generate_patch_with_inputs(codebase_path, inputs_hsv, pipeline_name, models_hsv, sanitizer_stderr=None, delta=None, pov_blob=None, pov_harness=None, patch_validator=None):
    # inputs_hsv is assumed to be an hyphen-separated value string of some non-empty subset of:
    # code-delta-sani-blob-harn. To be extended as needed, e.g. with "sarif" or somesuch.
    inputs = inputs_hsv.split('-')
    print("inputs", inputs)
    
    # Treat models as a hyphen-separated value string as well now.
    models = models_hsv.split('-')
    models = [find_model(model) for model in models]

    # Here we mask some of the inputs depending on how we're constraining the patch test
    # (inputs is a whitelist of available inputs)
    if "delta" not in inputs:
        delta = None

    if "sani" not in inputs:
        sanitizer_stderr = None

    if "blob" not in inputs:
        pov_blob = None

    if "harn" not in inputs:
        pov_harness = None

    #print("delta", delta)
    #print("sanitizer_stderr", sanitizer_stderr)
    #print("blob", pov_blob)
    #print("harness", pov_harness)

    validated = None
    score = None
    
    # Here we use the pipeline_name to dispatch to different modules.
    try:
        pipeline_module = importlib.import_module(f"pipelines.{pipeline_name}")
        if patch_validator is None: # A quick ugly hack to retrofit old, validation-less pipelines
            patch_obj = try_models(models, pipeline_module.generate_patch, codebase_path, sanitizer_stderr, delta, pov_blob, pov_harness)
        else:
            patch_obj = try_models(models, pipeline_module.generate_patch, codebase_path, sanitizer_stderr, delta, pov_blob, pov_harness, patch_validator=patch_validator)
        print("type of patch_obj is", type(patch_obj))
        if isinstance(patch_obj, dict):
            patch = patch_obj["patch"]
            validated = patch_obj["validated"]
            score = patch_obj["score"]
        else:
            patch = patch_obj
    except ImportError:
        raise ValueError(f"Module {pipeline_name} not found. We expect there to be a module named the same thing as the pipeline.")
    except AttributeError:
        raise ValueError(f"Module {pipeline_name} does not have a 'generate_patch' function. We expect it to.")
    #except Exception as e:
    #    raise e

    patch_name = f"{inputs_hsv}--{pipeline_name}--{models_hsv}"
    record(f"{patch_name}_patch", patch, False)

    for model in models:
        history = model.history[-3:]
        record(f"model_history_last3prompts", str(history), False)
    
    return (patch, validated, score)
