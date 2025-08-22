#!/usr/bin/env python3
import time
import random
import argparse
import json
import subprocess
import sys
import logging
import os
import re
import queue
import base64
import requests
import builtins
from typing import Dict, List, Union

# Tell amp import for telling optimus submission status
lacrosse_path_env_var = os.environ.get("DOCKER_LACROSSE_HOME")
tell_amp_tool = os.path.join(lacrosse_path_env_var, "code", "matchmaker", "tell-amp")
lacrosse_user_env_var = os.environ.get("CIRCA_BASENAME")
tell_optimus_amp = f"OPTIMUS0-AMP-{lacrosse_user_env_var}-ACCEPTOR"
lacrosse_mm_env_var = os.environ.get("CIRCA_MM_HOST")

# This is the updated submission system for the AIxCC final compeition. This might need to be hooked into some API.
#  This understands the schema under aixcc docs/api/competition-api.json
# TODO Finalize and test bundle submissions

# Define initial retries for pov, patch, bundle, freeform, and broadcast sarif submissions
init_sarif_retry = init_freeform_retry = init_pov_retry = init_patch_retry = init_broadcast_retry = init_bundle_retry = 0

# Helper method for prefixed submit script prints/logging
def print(*args, **kwargs):
  builtins.print("AFC-Submit:", *args, **kwargs)

# Helper method for checking environment variables
def get_environment_var(var: str) -> str:
    logging.info(f"Checking if environemnt variable {var} is set.")
    if (var in os.environ):
        return os.environ[var]
    else:
        logging.error(f"Environment variable unset: \"{var}\"")
        exit(1)

# Our --user values for submitting responses.
COMPETITION_API_TEAM_ID = get_environment_var("COMPETITION_API_TEAM_ID")
COMPETITION_API_TEAM_SECRET = get_environment_var("COMPETITION_API_TEAM_SECRET")

# Helper method for building the auth key from compeition id:secret
def get_auth():
  comp_auth_string = f"{COMPETITION_API_TEAM_ID}:{COMPETITION_API_TEAM_SECRET}"
  comp_auth_bytes = comp_auth_string.encode("utf-8")
  encoded_auth_bytes = base64.b64encode(comp_auth_bytes)
  return encoded_auth_bytes.decode("utf-8")

# Address for the compeition. This will the prefix to different endpoints.
COMPETITION_API_ENDPOINT = get_environment_var("COMPETITION_API_ENDPOINT")

# Helper method for encoding pov blobs and patch files in base64
def encode_to_base64(submission_file):
  with open(submission_file, "rb") as f:
     encoded_bytes = base64.b64encode(f.read())
     return encoded_bytes.decode("utf-8")

###############################################
### POV Submission
###############################################

# Submit a pov
def submit_pov(task_id, engine, testblob, fuzzer_name, sanitizer):
  global init_pov_retry
  print(f"Encoding the testblob: {testblob} to base64...")
  encoded_testblob = encode_to_base64(testblob)
  print(f"encoded testblob is {testblob}")

  # Submission system endpoint
  competition_pov_endpoint = f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/pov"
  print(f"competition_pov_endpoint is {competition_pov_endpoint}")

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  # Build submission data json
  submission_data = {
    "architecture": "x86_64",
    "engine": engine,
    "testcase": encoded_testblob,
    "fuzzer_name": fuzzer_name,
    "sanitizer": sanitizer,
  }
  print(f"submission_data is {submission_data}")

  # Send the submission
  pov_submission_result = requests.post(competition_pov_endpoint, headers=submit_headers, json=submission_data)
  # TODO add conditional to check result return code should be a "200 OK"
  print(f"submission result of pov is {pov_submission_result}")
  pov_submission_json = pov_submission_result.json()
  print(f"submission result json of pov is {pov_submission_result}")

  # Get pov_id and status
  pov_id = pov_submission_json.get("pov_id")
  pov_status = pov_submission_json.get("status")

  # Return pov_id to begin polling, return 0 on error.
  print(f"pov_status is {pov_status}")
  if pov_status == "accepted":
    print(f"pov_id: {pov_id} was accepted. Polling for pov success...")
    return pov_id
  elif pov_status == "deadline_exceeded":
    print(f"pov_id \"{pov_id}\" was submitted passed the deadline.")
  elif pov_status == "errored" and init_pov_retry < 5:
    print(f"pov_id \"{pov_id}\" received status {pov_status} and will be resubmitted")
    init_pov_retry += 1
    rand_delay = random.uniform(2, 30)
    print(f"Delaying for \"{rand_delay}\" seconds before retrying initial pov submission")
    time.sleep(rand_delay)
    submit_pov(task_id, engine, testblob, fuzzer_name, sanitizer)
  else:
    print(f"pov_id: \"{pov_id}\" was not successful and has status \"{pov_status}\".")
  return 0

# Poll over the submitted pov_id to get the status
def poll_submitted_pov(task_id, pov_id, lax_id):
  pov_polling_endpoint = f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/pov/{pov_id}"
  print(f"pov_polling_endpoint is {pov_polling_endpoint}")

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  # Poll the compeition server.
  poll_pov_result = requests.get(pov_polling_endpoint, headers=submit_headers)
  # TODO poll_pov_result response code will be 404 if pov_id does not exist...
  poll_pov_json = poll_pov_result.json()

  # polling pov will also return a pov_id, which will hopefully be the same pov_id as the previous polling...
  # get pov status, if status is still "accepted" loop, else return and tell optimus...
  poll_pov_status = poll_pov_json.get("status")

  if poll_pov_status == "accepted":
    time.sleep(30) # Sleep for 30 seconds before polling again
    poll_submitted_pov(task_id, pov_id, lax_id)
  elif poll_pov_status == "errored":
    print(f"Pov polling has determined status \"{poll_pov_status}\" and will start resubmission.")
    return "errored"
  else:
    # Build message to tell optimus the status at polling complete
    passed_pov_msg = f'((:type :pov-status)(:pov-id "{pov_id}") (:status :{poll_pov_status}) (:lax-id {lax_id}))'
    print(f"Pov polling has completed with status {poll_pov_status}; opt-msg will be: {passed_pov_msg}")
    print(f"EXECUTING: {tell_amp_tool} -n {tell_optimus_amp} -h {lacrosse_mm_env_var}  -f {lacrosse_mm_env_var} opt-msg ")
    # Send message to optimus via amp
    subprocess.run([tell_amp_tool, "-n", tell_optimus_amp, "-h", lacrosse_mm_env_var, "-f", lacrosse_mm_env_var, passed_pov_msg])
    return f"{poll_pov_status}"

###############################################
### Patch Submission
###############################################

# Submitting a patch just requires the patchblob and the task_id. No longer requires a complementing POV_ID.
def submit_patch(task_id, patchblob):
  global init_patch_retry
  # Build submission endpoint
  competition_patch_endpoint=f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/patch"
  print(f"competition_patch_endpoint is {competition_patch_endpoint}")

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  # Build submission data json
  print(f"Encoding the patchblob: {patchblob} to base64...")
  encoded_patchblob = encode_to_base64(patchblob)
  print(f"encoded patchblob is {patchblob}")
  submission_data = {
    "patch": encoded_patchblob
  }
  print(f"submission_data is {submission_data}")

  # Submit the patch
  print(f"Posting patch for task_id {task_id}")
  patch_submission_result = requests.post(competition_patch_endpoint, headers=submit_headers, json=submission_data)
  print(f"Submitted patch for task id {task_id}")

  # TODO add conditional to check result return code should be a "200 OK"
  patch_submission_json = patch_submission_result.json()
  patch_id = patch_submission_json.get("patch_id")
  patch_status = patch_submission_json.get("status")
  print(f"patch_status is {patch_status}")

  # Get the status of the submitted patch, return the patch_id to begin polling. Return 0 on error.
  if patch_status == "accepted":
    print(f"patch_id: {patch_id} was accepted. Polling for patch success...")
    return patch_id
  elif patch_status == "deadline_exceeded":
    print(f"patch_id: \"{patch_id}\" was submitted passed the deadline.")
  elif patch_status == "errored" and init_patch_retry < 5:
    print(f"patch_id \"{patch_id}\" received status {patch_status} and will be resubmitted.")
    init_patch_retry += 1
    rand_delay = random.uniform(2, 30)
    print(f"Delaying for \"{rand_delay}\" seconds before retrying initial patch submission")
    time.sleep(rand_delay)
    submit_patch(task_id, patchblob)
  else:
    print(f"patch_id: \"{patch_id}\" received an unexpected status of \"{patch_status}\".")
  return 0

# Poll submitted patch
def poll_submitted_patch(task_id, patch_id, lax_id):
  patch_polling_endpoint = f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/patch/{patch_id}"
  print(f"patch_polling_endpoint is {patch_polling_endpoint}")

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  poll_patch_result = requests.get(patch_polling_endpoint, headers=submit_headers)

  # Polling the patch will return : status (accepted, passed, failed, deadline_exceeded or errored)
  # and : functionality_tests_passing (True, False, None);  None == not yet run/complete.
  # a passing status means the functionallity tests passed and the submission is complete

  # AFC-Submit: poll patch has received data: {'patch_id': 'e82d23c2-c311-4a9b-9c95-94f79d8375e6', 'status': 'passed', 'functionality_tests_passing': True}
  # AFC-Submit: poll patch has received data: {'patch_id': 'e82d23c2-c311-4a9b-9c95-94f79d8375e6', 'status': 'accepted', 'functionality_tests_passing': None}

  poll_patch_json = poll_patch_result.json()
  print(f"poll patch has received data: {poll_patch_json}")
  poll_patch_status = poll_patch_json.get("status")
  #print(f"poll patch has status: {poll_patch_status}")
  poll_patch_tests = poll_patch_json.get("functionality_tests_passing")
  #print(f"poll patch has functionality_tests_passing: {poll_patch_tests}")

  if poll_patch_status == "accepted":	# comp server still hasnt decided, poll again
    time.sleep(30)
    poll_submitted_patch(task_id, patch_id, lax_id)
  elif poll_patch_status == "errored":
    print(f"Patch polling has determined status \"{poll_patch_status}\" and will start resubmission.")
    return "errored"
  else: 	# Build and send message to tell optimus status of completed patch polling
    patch_msg = f"((:type :patch-status) (:functionality-tests-passing :{poll_patch_tests}) (:patch-id {patch_id}) (:status :{poll_patch_status}) (:lax-id {lax_id}))"
    print(f"Patch polling has completed with status {poll_patch_status} ; opt-msg will be: {patch_msg}")
    print(f"EXECUTING: {tell_amp_tool} -n {tell_optimus_amp} -h {lacrosse_mm_env_var}  -f {lacrosse_mm_env_var} opt-msg")
    subprocess.run([tell_amp_tool, "-n", tell_optimus_amp, "-h", lacrosse_mm_env_var, "-f", lacrosse_mm_env_var, patch_msg])
    return f"{poll_patch_status}"

###############################################
### Freeform Submission
###############################################

# Submit a freeform pov
def submit_freeform(task_id, testblob):
  global init_freeform_retry
  print(f"Encoding the testblob: {testblob} to base64...")
  encoded_testblob = encode_to_base64(testblob)
  print(f"encoded testblob is {testblob}")

  # Submission system endpoint
  competition_freeform_endpoint = f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/freeform/"
  print(f"competition_freeform_endpoint is {competition_freeform_endpoint}")

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  # Build submission data json
  submission_data = {
    "submission": encoded_testblob,
  }
  print(f"submission_data is {submission_data}")

  # Send the submission
  freeform_submission_result = requests.post(competition_freeform_endpoint, headers=submit_headers, json=submission_data)
  # TODO add conditional to check result return code should be a "200 OK"
  print(f"submission result of freeform is {freeform_submission_result}")
  freeform_submission_json = freeform_submission_result.json()
  print(f"submission result json of freeform is {freeform_submission_result}")

  # Get freeform_id and status
  freeform_id = freeform_submission_json.get("freeform_id")
  freeform_status = freeform_submission_json.get("status")

  # Return freeform_id to begin polling, return 0 on error.
  print(f"freeform_status is {freeform_status}")
  if freeform_status == "accepted":
    print(f"freeform_id: {freeform_id} was accepted.")
    return freeform_id
  elif freeform_status == "deadline_exceeded":
    print(f"freeform_id \"{freeform_id}\" was submitted passed the deadline.")
  elif freeform_status == "errored" and init_freeform_retry < 5:
    print(f"freeform_id \"{freeform_id}\" received status {freeform_status} and will be resubmitted.")
    init_freeform_retry += 1
    rand_delay = random.uniform(2, 30)
    print(f"Delaying for \"{rand_delay}\" seconds before retrying initial freeform submission")
    time.sleep(rand_delay)
    submit_freeform(task_id, testblob)
  else:
    print(f"freeform_id: \"{freeform_id}\" was not successful and has status \"{freeform_status}\".")
  return 0

###############################################
### CRS Generated Sarif Submission
###############################################

# Submit a sarif generated by our CRS
def submit_sarif(task_id, sarif_object):
  global init_sarif_retry
  # Build the sarif endpoint
  submitted_sarif_endpoint = f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/submitted-sarif"
  print(f"submitted_sarif_endpoint is {submitted_sarif_endpoint}")

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  # Build the submitted sarif payload. This is expected to be some {} sarif object.
  submitted_sarif_data = {
    "sarif": sarif_object
  }

  # Submit the CRS Generated Sarif
  sarif_submission_result = requests.post(submitted_sarif_endpoint, headers=submit_headers, json=submitted_sarif_data)

  # Sarif polling is not needed
  sarif_submission_json = sarif_submission_result.json()
  submitted_sarif_id = sarif_submission_json.get("submitted_sarif_id")
  submitted_sarif_status = sarif_submission_json.get("status")
  print(f"submitted_sarif_status is {submitted_sarif_status}")

  # Tell optimus the sarif status, if optimus even cares?
  if submitted_sarif_status == "accepted":
    print(f"submitted_sarif: {submitted_sarif_id} was accepted")
    return submitted_sarif_id
  elif submitted_sarif_status == "deadline_exceeded":
    print(f"submitted_sarif: {submitted_sarif_id} was submitted passed the deadline")
  elif submitted_sarif_status == "errored" and init_sarif_retry < 5:
    print(f"submitted_sarif: {submitted_sarif_id} received status {submitted_sarif_status} and will be resubmitted")
    init_sarif_retry += 1
    rand_delay = random.uniform(2, 30)
    print(f"Delaying for \"{rand_delay}\" seconds before retrying initial sarif submission")
    time.sleep(rand_delay)
    submit_sarif(task_id, sarif_object)
  return 0

###############################################
### Broadcast Assessment Sarif Submission
###############################################

# Submit a response to a broadcast sarif assessment
def submit_broadcast_sarif(task_id, broadcast_sarif_id, sarif_assessment, sarif_description):
  global init_broadcast_retry
  # Build the broadcast sarif endpoint
  broadcast_sarif_endpoint = f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/broadcast-sarif-assessment/{broadcast_sarif_id}/"
  print(f"broadcast_sarif_endpoint is {broadcast_sarif_endpoint}")

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  # Build the submitted sarif payload. This is expected to be some {} sarif object.
  broadcast_sarif_data = {
    "assessment": sarif_assessment,
    "description": sarif_description,
  }

  # Submit the sarif
  broadcast_submission_result = requests.post(broadcast_sarif_endpoint, headers=submit_headers, json=broadcast_sarif_data)

  # Sarif polling is not needed
  broadcast_submission_json = broadcast_submission_result.json()
  print(f"broadcast submission result json object is {broadcast_submission_json}")
  # broadcast_sarif_id = broadcast_submission_json.get("broadcast_sarif_id")
  broadcast_sarif_status = broadcast_submission_json.get("status")
  print(f"broadcast_sarif_status is {broadcast_sarif_status}")

  # Tell optimus the sarif status, if optimus even cares?
  if broadcast_sarif_status == "accepted":
    print(f"broadcast_sarif: {broadcast_sarif_id} was accepted")
    return broadcast_sarif_id
  elif broadcast_sarif_status == "deadline_exceeded":
    print(f"broadcast_sarif: {broadcast_sarif_id} was submitted passed the deadline")
  elif broadcast_sarif_status == "errored" and init_broadcast_retry < 5:
    print(f"broadcast_sarif: {broadcast_sarif_id} received status {broadcast_sarif_status} and will be resubmitted")
    init_broadcast_retry += 1
    rand_delay = random.uniform(2, 30)
    print(f"Delaying for \"{rand_delay}\" seconds before retrying initial broadcast sarif submission")
    time.sleep(rand_delay)
    submit_broadcast_sarif(task_id, broadcast_sarif_id, sarif_assessment, sarif_description)
  return 0

###############################################
### Bundle  Submission
###############################################

def submit_bundle(task_id, pov_id=None, patch_id=None, submitted_sarif_id=None, broadcast_sarif_id=None, description=None):
  global init_bundle_retry
  # Build bundle endpoint
  new_bundle_endpoint = f"{COMPETITION_API_ENDPOINT}/v1/task/{task_id}/bundle"

  # Build submission headers
  submit_headers = {
    "Authorization": f"Basic {get_auth()}",
    "Content-Type": "application/json",
  }
  print(f"submit_headers are {submit_headers}")

  # Build the bundle data, expectes ids of already submittied artifacts.
  # submitting a bundle of only two artifacts, all other artifacts may be "null"
  # "description" is optional
  # Create conditionals could use **kwargs.
  new_bundle_dict = {}

  if pov_id is not None:
    new_bundle_dict["pov_id"] = pov_id

  if patch_id is not None:
    new_bundle_dict["patch_id"] = patch_id

  if submitted_sarif_id is not None:
    new_bundle_dict["submitted_sarif_id"] = submitted_sarif_id

  if broadcast_sarif_id is not None:
    new_bundle_dict["broadcast_sarif_id"] = broadcast_sarif_id

  if description is not None:
    new_bundle_dict["description"] = description

  # new_bundle_data = json.dumps(new_bundle_dict, indent=2)
  print(f"new_bundle_dict is {new_bundle_dict}")

  # Submit the bundle
  bundle_submission_result = requests.post(new_bundle_endpoint, headers=submit_headers, json=new_bundle_dict)
  bundle_submission_json = bundle_submission_result.json()

  print(f"bundle_submission_result is {bundle_submission_result}")
  print(f"bundle_submission_result data is {bundle_submission_json}")

  # Bundle_id will be important if making additions to existing bundles...
  bundle_id = bundle_submission_json.get("bundle_id")
  bundle_status = bundle_submission_json.get("status")

  # Check bundle status
  if bundle_status == "accepted":
    print(f"bundle_id: {bundle_id} was accepted.")
    return bundle_id
  elif bundle_status == "deadline_exceeded":
    print(f"bundle_id: {bundle_id} was submitted passed the deadline.")
  elif bundle_status == "errored" and init_bundle_retry < 5:
    print(f"bundle_id: {bundle_id} received status {bundle_status} and will be resubmitted.")
    init_bundle_retry += 1
    rand_delay = random.uniform(2, 30)
    print(f"Delaying for \"{rand_delay}\" seconds before retrying initial bundle submission")
    time.sleep(rand_delay)
    submit_bundle(task_id, pov_id, patch_id, submitted_sarif_id, broadcast_sarif_id, description)
  return 0

###############################################
### Main and Parse Args
###############################################

def main():
  """
  Main function for doing submissions.
  Accept information from optimus.
  First arg should be what to submit, every following argument should be the data to construct the submission payload.
  """
  # what to submit i.e "pov" "patch" "sarif" "bundle"
  submission_option = sys.argv[1]

  # NOTE || potential TODO pov and patch data are expected to arrive as base64 data strings.
  payload_args = parse_args() # supporting data for submission sys.argv[2:]

  if submission_option == "pov":
    # Get the POV_ID from the submission, check it is not 0, then begin polling.
    pov_id = submit_pov(payload_args.task_id, payload_args.engine, payload_args.testblob, payload_args.fuzzer, payload_args.sanitizer)

    if pov_id == 0:
      # Tell optimus POV_submission errored...
      print("A pov_id was not received. Pov submission has likely exceeded the deadline.")
      return -1

    # Handle resubmissions for a pov that received "errored" status at polling.
    polled_pov_status = poll_submitted_pov(payload_args.task_id, pov_id, payload_args.lax_id)
    print(f"polled_pov_status is {polled_pov_status}")
    pov_retry_limit = 0
    while polled_pov_status == "errored" and pov_retry_limit < 5:

      print(f"Polling Pov {pov_id} has returned a {polled_pov_status} status.")
      print(f"A PoV will be resubmitted for {payload_args.task_id} with engine: {payload_args.engine} \
            with fuzzer: {payload_args.fuzzer} with sanitizer {payload_args.sanitizer} and with blob: {payload_args.testblob}")
      pov_retry_limit += 1

      pov_id = submit_pov(payload_args.task_id, payload_args.engine, payload_args.testblob, payload_args.fuzzer, payload_args.sanitizer)
      print(f"Resubmitted pov_id: {pov_id}")
      polled_pov_status = poll_submitted_pov(payload_args.task_id, pov_id, payload_args.lax_id)
      if polled_pov_status == "errored":
        # Delay for some 2 to 20 seconds before retrying on new "errored"...
        pov_rand_delay = random.uniform(2, 30)
        print(f"Delaying for \"{pov_rand_delay}\" seconds before retrying submission")
        time.sleep(pov_rand_delay)

    print(f"task_id: {payload_args.task_id} has a submitted pov of pov_id: {pov_id} with status {polled_pov_status}") # Optimus should know this in order to form bundles
    return polled_pov_status

  elif submission_option == "patch":
    # Get the patch_id from the submission, check it is not 0, then begin polling.
    patch_id = submit_patch(payload_args.task_id, payload_args.patch)

    if patch_id == 0:
        print("A patch_id was not received. Patch submission has likely exceeded the deadline.")
        return -1

    # Handle resubmissions for a patch that received "errored" status at polling.
    polled_patch_status = poll_submitted_patch(payload_args.task_id, patch_id, payload_args.lax_id)
    print(f"polled_patch_status is {polled_patch_status}")
    patch_retry_limit = 0
    while polled_patch_status == "errored" and patch_retry_limit < 5:
      print(f"Polling patch {patch_id} has returned a {polled_patch_status} status.")
      print(f"A patch will be resubmitted for {payload_args.task_id} with patch: {payload_args.patch}")
      patch_retry_limit += 1

      patch_id = submit_patch(payload_args.task_id, payload_args.patch)
      print(f"Resubmitted pov_id: {patch_id}")
      polled_patch_status = poll_submitted_patch(payload_args.task_id, patch_id, payload_args.lax_id)
      if polled_patch_status == "errored":
        # Delay for some 2 to 20 seconds before retrying on new "errored"...
        patch_rand_delay = random.uniform(2, 30)
        print(f"Delaying for \"{patch_rand_delay}\" seconds before retrying submission")
        time.sleep(patch_rand_delay)

    print(f"task_id: {payload_args.task_id} has a submitted patch of patch_id: {patch_id} with status {polled_patch_status}") # Optimus should know this in order to form bundles
    return polled_patch_status

  elif submission_option == "broadcast":
    broadcast_sarif_id = submit_broadcast_sarif(payload_args.task_id, payload_args.broadcast_sarif_id, payload_args.sarif_assessment, payload_args.sarif_description)

    if broadcast_sarif_id == 0:
      print("A broadcast sarif id was not received. Broadcast sarif has likely exceeded the deadline.")
      return -1

    print(f"task_id: {payload_args.task_id} has a broadcast sarif of sarif_id: {broadcast_sarif_id}") # Optimus should know this in order to form bundles
    return broadcast_sarif_id

  elif submission_option == "sarif":
    # return the submitted sarirf ID, or -1
    sub_sarif_id = submit_sarif(payload_args.task_id, payload_args.submitted_sarif)

    if sub_sarif_id == 0:
      print("A submitted sarif id was not received. Submitted sarif has likely exceeded the deadline.")
      return -1

    print(f"task_id: {payload_args.task_id} has a submitted sarif of sarif_id: {sub_sarif_id}") # Optimus should know this in order to form bundles
    return sub_sarif_id

  elif submission_option == "bundle":
    bundle_id = submit_bundle(payload_args.task_id, payload_args.pov_id, payload_args.patch_id,
                              payload_args.submitted_sarif_id, payload_args.broadcast_sarif_id, payload_args.description)

    if bundle_id == 0:
      print("A bundle_id was not received. Bundle submission has likely exceeded the deadline.")
      return -1

    return bundle_id

  elif submission_option == "freeform":
    freeform_id = submit_freeform(payload_args.task_id, payload_args.testblob)

    if freeform_id == 0:
      print("A freeform_id was not received. Freeform pov submission likely exceeded the deadline.")
      return -1

    print(f"task_id: {payload_args.task_id} has a submitted freeform pov of freeform_id: {freeform_id}") # Optimus should know this in order to form bundles
    return freeform_id

def parse_args():
  # initial arguement is what is important, every following arguement is whatever is needed...
  p = argparse.ArgumentParser(description=__doc__)
  p.add_argument('--task_id', required=True, help="Expects: string. Task identifier for a competition challenge task expecting submission.")
  p.add_argument('--engine', required=False, help="Expects: string. Fuzz Tooling Engine that exercises this vuln. Allowable engine values are specified in project.yaml." )
  p.add_argument('--testblob', required=False, help="Expects: path to patch file. Data of the pov or freeform pov for a task.")
  p.add_argument('--fuzzer', required=False, help="Expects: string. Fuzzer name or harness name. Which harness to pass the datafile to.")
  p.add_argument('--sanitizer', required=False, help="Expects: string. Which sanitizer the project was built with.")
  p.add_argument('--patch', required=False, help="Expects: path to patch file. Base64 data of the patch for a task.")
  p.add_argument('--broadcast_sarif_id', required=False, help="Expects string. ID of the Broadcast Sarif sent to the CRS from the competition system.")
  p.add_argument('--sarif_assessment', required=False, help="Expects string. \"incorrect\" or \"correct\".")
  p.add_argument('--sarif_description', required=False, help="Expects string. Nonempty description for assessment reasoning.")
  p.add_argument('--submitted_sarif', required=False, help="Expects sarif object. Sarif formatted object. A sarif submission for a task.")
  p.add_argument('--lax_id', required=True, help="Expects int. ID assignment by the AMP system.")
  p.add_argument('--pov_id', required=False, help="Expects: string. ID of a submitted and accepted/passing POV.")
  p.add_argument('--patch_id', required=False, help="Expects: string. ID of a submitted and accepted/passing POV.")
  p.add_argument('--submitted_sarif_id', required=False, help="Expects: string. ID of a submitted and accepted sarif that is NOT in response to a broadcast.")
  p.add_argument('--description', required=False, help="Expects: string. Description of the bundle to be submitted.")

  pargs = p.parse_args(sys.argv[2:])
  return pargs

if __name__ == "__main__":
  main()
