import os
import dspy
import models as models
from utils import read_file, well_formed_delta, well_formed_sanitizer_stderr
from modules.find_ftp_at_top_of_sanitizer import find_ftp_at_top_of_sanitizer
from modules.find_vuln_in_ftp import find_vuln_in_ftp
import models
from modules.parallel_consensus import parallel_consensus

FIXED_VOTING_MODELS_TMP05 = [models.sonnet37_tmp05, models.o3, models.gpt4o_tmp05, models.sonnet4_tmp05]
FIXED_VOTING_MODELS_TMP1 = [models.sonnet37_tmp1, models.o3, models.gpt4o_tmp1, models.sonnet4_tmp1]

class DiscoverVulnWithPov(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose
        
    def forward(self, codebase_path, sanitizer_stderr, pov_blob=None, pov_harness=None, pov_harness_name=None, delta=None, hints="None."):
        ftp_relpath = find_ftp_at_top_of_sanitizer(sanitizer_stderr, codebase_path, delta, pov_harness_name)
        print("ftp_relpath=", ftp_relpath)
        ftp_abspath = os.path.join(codebase_path, ftp_relpath)
        print("ftp_abspath=", ftp_abspath)
        assert os.path.exists(ftp_abspath), f"The FTP path doesn't point to an actual file: {ftp_abspath}"
        ftp_code = read_file(ftp_abspath)
        # vuln = find_vuln_in_ftp(ftp_code, ftp_relpath, sanitizer_stderr, pov_blob=pov_blob, pov_harness=pov_harness, delta=delta, hints=hints)

        # This is a critical early step which can send us down the wrong path.
        # We occasionally get a weird rogue answer that sets us up for hardship later.
        # Use the parallel consensus module to try to get a more reliable answer!
        # Get N=1 votes each from an assortment of models (currently 5), including the current default model
        # so that the outer model fallback mechanism used in tools/characterize-vulns.py
        # still provides some diversity across attempts.
        # The other models in the voting pool have temperature higher than zero to encourage some variety within the voting pool.
        worker_models = [dspy.settings.lm, *FIXED_VOTING_MODELS_TMP05]
        worker_fanout = 1 # this is N
        vuln = parallel_consensus(find_vuln_in_ftp, worker_models, worker_fanout, ftp_code, ftp_relpath, sanitizer_stderr, pov_blob=pov_blob, pov_harness=pov_harness, delta=delta, hints=hints)
        
        return dspy.Prediction(vuln=vuln)

def discover_vulns_with_pov(codebase_path, sanitizer_stderr, pov_blob=None, pov_harness=None, pov_harness_name=None, delta=None, hints="None."):
    print("discover_vulns_with_pov")
    discoverer = DiscoverVulnWithPov()
    pred = discoverer(codebase_path=codebase_path, sanitizer_stderr=sanitizer_stderr, pov_blob=pov_blob, pov_harness=pov_harness, pov_harness_name=pov_harness_name, delta=delta, hints=hints)
    return pred.vuln
