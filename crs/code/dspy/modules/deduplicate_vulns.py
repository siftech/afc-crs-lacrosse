import json
import dspy
import models as models
from modules.filter_strs_by_index import filter_strs_by_index
from record import record
from utils import current_dspy_lm
from custom_types import Vulnerability

class DeduplicateVulns(dspy.Module):
    def __init__(self, verbose=False):
        self.verbose = verbose

    def name(self):
        model_name = current_dspy_lm()
        return f"discover_vulns_{model_name}"
        
    def record(self, record_name, content, verbose=False):
        record(f"{self.name()}/{record_name}", content, verbose)    

    def parse_known_vulns(self, known_vulns_json):
        if known_vulns_json is None:
            return None
        else:
            with open(known_vulns_json) as f:
                known_vulns_data = json.load(f)
                if known_vulns_data is None:
                    return None
                known_vulns = []
                for i, vuln_dict in enumerate(known_vulns_data):
                    print(f"vuln_dict({i})=")
                    print(json.dumps(vuln_dict, indent=2))
                    vuln = Vulnerability.model_validate(vuln_dict)
                    known_vulns.append(vuln)
                return known_vulns

    # Filter down the dedupe candidates using their associated filepaths.
    # Any overlap in filepaths means it's a candidate.
    def triage_known_vulns_on_filepath(self, known_vulns, vuln):
        print("triage_known_vulns_on_filepath", len(known_vulns), vuln)
        v_fps = set([v_sfr.target_filepath for v_sfr in vuln.suggested_file_repairs])
        print("vuln has filepaths:", v_fps)
        candidates = []
        for i, known_vuln in enumerate(known_vulns):
            kv_fps = set([kv_sfr.target_filepath for kv_sfr in known_vuln.suggested_file_repairs])
            print(f"known_vuln {i} has filepaths:", kv_fps)
            has_overlap = bool(v_fps & kv_fps)
            if has_overlap:
                print(f"overlap detected! kv_fps={kv_fps} v_fps={v_fps}")
                candidates.append(known_vuln)
        print(f"Returning {len(candidates)} candidates from triage:")
        for candidate in candidates:
            print(str(candidate))
        return candidates
    
    def forward(self, vulns, known_vulns_json=None, known_inferior_vulns_json=None):
        deduped_vulns = []

        print()
        print(f"dedupe against known_vulns json={known_vulns_json}")
        for vuln in vulns:
            # Parse as late as possible, bc we want to allow known vulns to get added up until the last second.
            # Assumes that we use atomic writes! (on the lisp side).
            try:
                known_vulns = self.parse_known_vulns(known_vulns_json)
                if known_vulns is None:
                    print("known_vulns is None")
                    deduped_vulns.append(vuln)
                elif len(known_vulns) == 0:
                    print("known_vulns has length 0")
                    deduped_vulns.append(vuln)
                else:
                    print(f"known_vulns has length {len(known_vulns)}")

                    # First triage based on filepath
                    print(f"Number of known_vulns before triage: {len(known_vulns)}")
                    known_vulns = self.triage_known_vulns_on_filepath(known_vulns, vuln)
                    print(f"Number of known_vulns after triage: {len(known_vulns)}")
                    
                    known_vuln_descriptions = [vuln.description for vuln in known_vulns]
                    if self.verbose:
                        self.record("known_vuln_descriptions", "\n\n###############\n\n".join([f"{i + 1}. {desc}" for i, desc in enumerate(known_vuln_descriptions)]), True)
                    
                    dupe_vuln_descriptions = filter_strs_by_index(known_vuln_descriptions, f"Identify any descriptions from this list of known cyber vulnerabilities that describe the same vulnerability (with the same set of crashing inputs when sanitized) as the following vulnerability description: {vuln.description}")
                    dupe_vulns_str = "\n\n###############\n\n".join([f"{i + 1}. {desc}" for i, desc in enumerate(dupe_vuln_descriptions)])
                    self.record("dupe_vuln_descriptions", f"Found {len(dupe_vuln_descriptions)} duplicate descriptions for vuln:\n\n{vuln.description}\n\n================================\n\n{dupe_vulns_str}", True)
                    if len(dupe_vuln_descriptions) == 0:
                        deduped_vulns.append(vuln)
            except Exception as e:
                print(f"Caught error when deduping vuln {str(vuln)}: {e}.")
                print("Proceed assuming it is not a dupe.")
                deduped_vulns.append(vuln)

        print(f"Number of deduped vulns: {len(deduped_vulns)}")
                
        print()
        print(f"dedupe against known_inferior_vulns json={known_inferior_vulns_json}")
        inferior_dupe_map = None
        if known_inferior_vulns_json is not None:
            inferior_dupe_map = []
            # We don't filter new vulns on the basis of inferior dupes (like regular dupes above), but we do record them so we can inform optimus
            # so that optimus can avoid submitting inferior dupes.
            for vuln in deduped_vulns:
                try:
                    known_inferior_vulns = self.parse_known_vulns(known_inferior_vulns_json)
                    if known_inferior_vulns is None:
                        print("known_inferior_vulns is None")
                    elif len(known_inferior_vulns) == 0:
                        print("known_inferior_vulns has length 0")
                    else:
                        print(f"known_inferior_vulns has length {len(known_inferior_vulns)}")
                        
                        # First triage based on filepath
                        print(f"Number of known_inferior_vulns before triage: {len(known_inferior_vulns)}")
                        known_inferior_vulns = self.triage_known_vulns_on_filepath(known_inferior_vulns, vuln)
                        print(f"Number of known_inferior_vulns after triage: {len(known_inferior_vulns)}")
                        
                        known_inferior_vuln_descriptions = [vuln.description for vuln in known_inferior_vulns]
                        if self.verbose:
                            self.record("known_inferior_vuln_descriptions", "\n\n###############\n\n".join([f"{i + 1}. {desc}" for i, desc in enumerate(known_inferior_vuln_descriptions)]), True)
                            
                        dupe_inferior_vuln_descriptions = filter_strs_by_index(known_inferior_vuln_descriptions, f"Identify any descriptions from this list of known cyber vulnerabilities that describe the same vulnerability (with the same set of crashing inputs when sanitized) as the following vulnerability description: {vuln.description}")
                        dupe_inferior_vulns_str = "\n\n###############\n\n".join([f"{i + 1}. {desc}" for i, desc in enumerate(dupe_inferior_vuln_descriptions)])
                        self.record("dupe_inferior_vuln_descriptions", f"Found {len(dupe_inferior_vuln_descriptions)} duplicate inferior descriptions for vuln:\n\n{vuln.description}\n\n================================\n\n{dupe_inferior_vulns_str}", True)
                        for dupe_inferior_vuln_description in dupe_inferior_vuln_descriptions:
                            inferior_dupe_map.append([vuln.description, dupe_inferior_vuln_description])
                except Exception as e:
                    print(f"Caught error when trying to find dupe inferior vulns for vuln {str(vuln)}: {e}.")
                    print("Proceed assuming there are no inferior dupes for this vuln.")

        try:
            print(f"Length of inferior_dupe_map: {len(inferior_dupe_map)}")
        except:
            print(f"inferior_dupe_map is: {inferior_dupe_map}")
                    
        return dspy.Prediction(deduped_vulns=deduped_vulns, inferior_dupe_map=inferior_dupe_map)

def deduplicate_vulns(vulns, known_vulns_json=None, known_inferior_vulns_json=None):
    print("deduplicate_vulns", vulns, known_vulns_json, known_inferior_vulns_json)
    deduper = DeduplicateVulns(verbose=True)
    pred = deduper(vulns=vulns, known_vulns_json=known_vulns_json, known_inferior_vulns_json=known_inferior_vulns_json)
    return pred.deduped_vulns, pred.inferior_dupe_map

    
