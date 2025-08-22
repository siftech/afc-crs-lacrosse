
import json
import sys
import logging
import os
from typing import Dict, List, Union
import cl4py

# This may be obsolete. Might need the lisp to provide the args for an expected json payload.
def create_json(lisp_payload):
  """
  Will converts an optimus LISP message into a json payload for the Competition API.

  :param lisp_payload: LISP message?
  :return: JSON
  """
  # Split the message up
  lisp_tokens = re.findall(r'\(|\)|\S+', lisp_payload)
  if not lisp_tokens or lisp_tokens[0] != '(' or lisp_tokens[-1] != ')':
    print("Not lisp")
    return 0
  # Remove outside parentheses
  lisp_tokens = lisp_tokens[1:-1]
  json_dict = {lisp_tokens[i]: lisp_tokens[i + 1] for i in range(0, len(lisp_tokens), 2)}
  return json.dumps(json_dict)

# The old way, but with new stuff...
def submit_task():
  """
  A combination of create_json() and submit().
  The old way, but with the new stuff, in python so our python API can understand it.


  """




def submit(paylod, endpoint, auth_creds):
  """
  Build the curl message containing the JSON payload aimed at the competition API endpoint.

  :param task_id
  :param payload: JSON payload
  :param auth_creds: "username:password" <<-- from security_controller of the API.
  :param endpoint: endpoint of the competition API address. (Could be the whole address)
  """
  curl_command = [
    "curl", "-X", "POST", endpoint,
     "-H", "Content-Type: application/json",
     "--user", auth_creds, "-d", payload
  ]

  ret = subprocess.run(curl_command, capture_output=True, text=True)
  return ret.stdout

def main():
  """
  Accepts input arguments from the LISP performs the curl/API calls to the appro. Competition endpoint
  """
  if len(sys.argv) < 3:
    print("Expects json data and a \"what api endpoint did this hit\" arguments")
    return

  payload = sys.argv[1]
  option = sys.argv[2]
  message = parse_json(json.loads(payload))

if __name__ == "__main__":
  main()