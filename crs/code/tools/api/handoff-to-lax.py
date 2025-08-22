import json
import subprocess
import sys
import logging
import os
import re
import builtins
from typing import Dict, List, Union

# Helper method for prefixed handoff script prints/logging
def print(*args, **kwargs):
  builtins.print("Lax-Handoff:", *args, **kwargs)

lacrosse_path_env_var = os.environ.get("DOCKER_LACROSSE_HOME")
populate_cp_root_tool = os.path.join(lacrosse_path_env_var, "code", "tools", "afc-populate-cp-root")

tell_amp_tool = os.path.join(lacrosse_path_env_var, "code", "matchmaker", "tell-amp")
lacrosse_user_env_var = os.environ.get("CIRCA_BASENAME")

tell_optimus_amp = f"OPTIMUS0-AMP-{lacrosse_user_env_var}-ACCEPTOR"

lacrosse_mm_env_var = os.environ.get("CIRCA_MM_HOST")
# lacrosse_mm_env_var = os.environ.get("CIRCA_BASENAME") + "-neo-fuzz-ccl"


print(f"afc populate cp root tool path is : {populate_cp_root_tool}")

def parse_json_to_lisp(payload):
  """
  Maybe we want this?
  Will convert the JSON data to LISP expression

  :param payload: JSON data message from competition API.
  :return: LIST string for optimus?
  """
  if isinstance(payload, dict):
    # dict rep'd as list of pairs
    elem = [f'(:{key.upper()} {parse_json_to_lisp(value)})' for key, value in payload.items()]
    return f"( {' '.join(elem)} )"
  elif isinstance(payload, list):
    elem = [parse_json_to_lisp(value) for value in payload]
    return f'({" ".join(elem)})'
  elif isinstance(payload, str):
    safe_payload = payload.replace('\\', '\\\\').replace('"', '\\"').replace('\n', ' ')
    return f'"{safe_payload}"'
  else:
    return str(payload)

def flatten_json(json_data, prefix=""):
  """
  parse_json_tell_optimus helper function.
  Will handle situations of weird nested json in lists/dicts such as
  nested `"metadata":{"round_id":"local-dev","task_id":"FEFE37E5-6C77-477B-AF33-512FB0E7F3D7"}` will
  result in metadata_round_id : "local-dev" definitions...

  :param json_data: json
  :param prefix: prefix for nested key labels based on parent key.
  """

  if isinstance(json_data, dict):
    for key, value in json_data.items():
      yield from flatten_json(value, f"{prefix}{key}_")
  elif isinstance(json_data, list):
    for index, item in enumerate(json_data):
      yield from flatten_json(item, f"{prefix}{index}_")
  else:
    yield prefix[:-1], json_data

def parse_json_tell_optimus(payload, message_id, message_time, option):
  """
  Primary function. Converts the incomping message from the Competition API into variables to hand off to lax
  This might, also, play a roll in communicating with the competition API.
  tl;dr this should tell optimus what to do...

  :param payload: JSON data message from competition API.
  :param option: Tells what optimus to do, related to the hit endpoint
    i.e if /task_v1/delete then tell optimus to delete the task.
  :return: (Tell optimus what to do)
  """
  
  json_data = json.loads(payload)
  print(f"Building message for optimus...")
  # Switch based on which endpoint was hit on the competition system. What should optimus do? Some of these could be handled by the crs-server api.
  if option == "status_get":
    # Return status of the CRS. Report back a health check. Tell optimus.
    print(f"Telling optimus its a status check...")

  elif option == "status_delete":
    # "Reset all stats in the status endpoint" -- not sure what this is talking about yet...
    print(f"Telling optimus its a status reset...")

  elif option == "sarif_post":
    # submit a sarif vulnerability broadcast. Tell optimus.
    print(f"Telling optimus its a sarif broadcast...")

    print(f"Attempting to build lisp message for Optimus:")

    sarif_post_message = "("+"(:TYPE :V1-SARIF--POST)" + \
    f"(:MESSAGE-ID  \"{message_id}\")" + \
    f"(:MESSAGE-TIME {message_time})" + "(:SARIF " + "("+ parse_json_to_lisp(json_data) +")"+")"+")"

    print(f"sarif_post_message is {sarif_post_message}")
    # TODO - There is nothing in the lisp to handle sarif messages...
    return sarif_post_message

  elif option == "task_post":
    # Incoming task for work. Tell optimus...

    # Important details for task:
    # round_id="null"
    # task_id="null"
    # task_type="null"
    # focus="null"
    # project_name="null"
    # deadline="null"

    # List of sas urls and task_id will be used to populate CP_ROOT
    sas_url_list = []
    task_content = {}
    print(f"Telling optimus a task has arrived...")
    for key, value in flatten_json(json_data):
      task_content[key] = value

      if re.search(r"source_\d+_url", key):
        print(f"Adding {key} to url list")
        sas_url_list.append(value)

      globals()[key] = value
      print(f" {key} = {value}")
      if key.lower() == "harnesses_included" and value is not True:
         print("Unharnessed CT!  Drop it!")
         return("((:type :v1-task-post-without-harnesses) (:action :ignore))")

    populate_args = ["bash", populate_cp_root_tool, task_id] + sas_url_list

    # Pass in the deadline if we have it
    if "deadline" in globals():
      populate_args += ["--deadline", str(deadline)]
    
    subprocess.run(populate_args)

    print(f"Attempting to build lisp message for Optimus:")

    task_post_message = "("+"(:TYPE :V1-TASK--POST)" + \
    f"(:MESSAGE-ID  \"{message_id}\")" + \
    f"(:MESSAGE-TIME  {message_time})" + "(:TASKS " + "("+ parse_json_to_lisp(json_data) +")"+")"+")"

    # print(f"task_post_message is {task_post_message}")
    return task_post_message

  elif option == "task_delete":
    # Cancel all previously submitted tasks. "edge case recovery". Tell optimus.
    print(f"Telling optimus to delete all current tasks...")

  elif option == "task_<task_id>_delete":
    # Cancel this task. Tell optimus.
    print(f"Telling optimus to delete a specified task...")

  else:
    # Do something or nothing.
    return 0

  # Critical. Triage incoming json payload from competition system.
  for key, value in flatten_json(json_data):
    # globals()[key] = value
    print(f" {key} = {value}")

def main():
  """
  Main function. This whole script is a helper for the CRS API. Given the schema keeps changing, the new api should be updated to
  call this script with whatever JSON it gets and the endpoint that gets it...
  """

  if len(sys.argv) < 5:
    print("Expects json data and a \"what api endpoint did this hit\" arguments")
    return

  # Quick fix to include message_id and message_time
  payload = sys.argv[1]
  message_id = sys.argv[2]
  message_time = sys.argv[3]
  option = sys.argv[4]

  optimus_message = parse_json_tell_optimus(payload, message_id, message_time, option)
  print(f"\nOptimus Message is: \n")
  print(f"{optimus_message}\n")


  # Tell optimus/AMP -- NOTE this relies on the docker network being defined (i.e not host). "Test-amp-lax" docker bridge network between ccl and api containers.
  print(f"EXECUTING: {tell_amp_tool} -n {tell_optimus_amp} -h {lacrosse_mm_env_var}  -f {lacrosse_mm_env_var} opt-msg")
  subprocess.run([tell_amp_tool, "-n", tell_optimus_amp, "-h", lacrosse_mm_env_var, "-f", lacrosse_mm_env_var, optimus_message])

  # lisp_message = parse_json_to_lisp(json.loads(payload))
  # print(f"\nLisp message is: \n")
  # print(f"{lisp_message}")

if __name__ == "__main__":
  main()
