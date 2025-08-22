# AFC CRS API Tools

This directory contains the following tools:

`afc-crs-submit.py` a conversion of our original submit-pov/submit-patch scripts for the new API system.
  - NOT ready for use yet.

`handoff-to-lisp.py` a set of python commands for accepting tasking from the competition task distribution system.
  - The goal for this is to setup the incoming tasking in a manor similar to the `asc` so that the lisp doesn't need to change much.
  - NOT ready for use yet.

`converted_openapi3_crs_server` these .yaml and .json files are results of AIxCC provided crs-swagger*.json files being formatted for openapi3
then converted back into respective .yaml and .json files. These files are used in `fastapi-codegen --input converted_openapi3_crs_server.json --output afc_crs_server`
