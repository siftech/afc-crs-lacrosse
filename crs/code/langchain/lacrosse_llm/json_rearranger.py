#!/usr/bin/env python3

"""
Rearrange the CP JSON to make the harness and sanitizer IDs be _values_
instead of keys, to make them easier for the CL-JSON to handle.

Here's the original format:

```
{
  "cp_name": "Mock CP",
  "language": "c",
  "cp_sources": {
    "samples": {
      "address": "git@github.com:aixcc-sc/mock-cp-src.git",
      "ref": "v1.1.0"
    }
  },
  "docker_image": "ghcr.io/aixcc-sc/mock-cp:v2.0.3",
  "sanitizers": {
    "id_1": "AddressSanitizer: global-buffer-overflow",
    "id_2": "AddressSanitizer: SEGV"
  },
  "harnesses": {
    "id_1": {
      "name": "stdin_harness.sh",
      "source": "src/test/stdin_harness.sh",
      "binary": "out/stdin_harness.sh"
    }
  }
}
```


Here's the rewritten format:
```
{
  "cp_name": "Mock CP",
  "language": "c",
  "cp_sources": [
   {
      "id": "samples":
      "address": "git@github.com:aixcc-sc/mock-cp-src.git",
      "ref": "v1.1.0"
    }
  ],
  "docker_image": "ghcr.io/aixcc-sc/mock-cp:v2.0.3",
  "sanitizers": [
    {
      "id": "id_1",
      "value": "AddressSanitizer: global-buffer-overflow"
    },
    {
      "id": "id_2",
      "value": "AddressSanitizer: SEGV"
    }
  ],
  "harnesses": [
    {
      "name": "stdin_harness.sh",
      "source": "src/test/stdin_harness.sh",
      "binary": "out/stdin_harness.sh",
      "id": "id_1"
    }
  ]
}
```
"""

import json
import os
from sys import argv, stdout
from typing import List, Dict, Any


def listify(dict: Dict[str, Any]) -> List[dict]:
    new_list = []
    for id, props in dict.items():
        new_props = props.copy()
        new_props['id'] = id
        new_list.append(new_props)

    return new_list

def listify_sanitizers(dict: Dict[str, Any]) -> List[dict]:
    new_list = []
    for id, value in dict.items():
        new_list.append({'id': id, 'value': value})

    return new_list


def rearrange_json(contents: str) -> Dict[str, Any]:
    j = json.loads(contents)
    new = j.copy()
    new['harnesses'] = listify(j['harnesses'])
    new['sanitizers'] = listify_sanitizers(j['sanitizers'])
    new['cp_sources'] = listify(j['cp_sources'])
    return new


def main():
    if not os.path.exists(argv[1]):
        raise FileNotFoundError(argv[1])
    with open(argv[1], 'r') as file:
        contents: str = file.read()
    new = rearrange_json(contents)
    # print("Result of rearranging JSON is: {new}")
    json.dump(new, fp=stdout)
    print()


  
if __name__ == "__main__":
    main()
    exit(0)
