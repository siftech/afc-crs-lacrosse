#!/usr/bin/env bash
# Builds a curl to trigger a sarif from a respective full scan ctask

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z $DEBUG || warn "${@}"; }
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

# Setup local example comp server endpoint vars
export CIRCA_BASEPORT=${CIRCA_BASEPORT:-10000}
dbug CIRCA_BASEPORT is $CIRCA_BASEPORT

export COMP_API_PORT=${COMP_API_PORT:-$(( CIRCA_BASEPORT + 501 ))}
LOCAL_EX_COMP_SERVER_ENDPOINT="http://localhost:${COMP_API_PORT}/webhook/sarif"

# Short sleep to let opti hand off the full scan task before receiving the sarif...
warn "sleeping for 30 seconds"
sleep 30

# Gets an Opti log file then greps for the TASK_ID of the full scan.
API_LOG="$1"
if [ ! -f "$API_LOG" ]; then
  die "OPTI Log file ${API_LOG} not found for triggering a sarif."
fi

# Get the TASK_ID of the full scan from the API_LOGS
dbug "Looking in $API_LOG for TASK_ID"
TASK_ID=$(grep -oP '\(:TASK_ID "\K[^"]+(?="\))' "$API_LOG" | tail -n 1)
dbug "TASK_ID is $TASK_ID"

# Use the TASK_ID in the trigger sarif curl command. FIXME - This sarif curl is specific to libpng.
warn "Triggering sarif command"

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d "{
  \"task_id\": \"$TASK_ID\",
  \"sarif\": {
    \"runs\": [
      {
        \"artifacts\": [
          {
            \"location\": {
              \"index\": 0,
              \"uri\": \"pngrutil.c\"
            }
          }
        ],
        \"automationDetails\": {
          \"id\": \"/\"
        },
        \"conversion\": {
          \"tool\": {
            \"driver\": {
              \"name\": \"GitHub Code Scanning\"
            }
          }
        },
        \"results\": [
          {
            \"correlationGuid\": \"9d13d264-74f2-48cc-a3b9-d45a8221b3e1\",
            \"level\": \"error\",
            \"locations\": [
              {
                \"physicalLocation\": {
                  \"artifactLocation\": {
                    \"index\": 0,
                    \"uri\": \"pngrutil.c\"
                  },
                  \"region\": {
                    \"endLine\": 1447,
                    \"startColumn\": 1,
                    \"startLine\": 1421
                  }
                }
              }
            ],
            \"message\": {
              \"text\": \"Associated risk: CWE-121\"
            },
            \"partialFingerprints\": {
              \"primaryLocationLineHash\": \"22ac9f8e7c3a3bd8:8\"
            },
            \"properties\": {
              \"github/alertNumber\": 2,
              \"github/alertUrl\": \"https://api.github.com/repos/aixcc-finals/example-libpng/code-scanning/alerts/2\"
            },
            \"rule\": {
              \"id\": \"CWE-121\",
              \"index\": 0
            },
            \"ruleId\": \"CWE-121\"
          }
        ],
        \"tool\": {
          \"driver\": {
            \"name\": \"CodeScan++\",
            \"rules\": [
              {
                \"defaultConfiguration\": {
                  \"level\": \"warning\"
                },
                \"fullDescription\": {
                  \"text\": \"vulnerable to #CWE-121\"
                },
                \"helpUri\": \"https://example.com/help/png_handle_iCCP\",
                \"id\": \"CWE-121\",
                \"properties\": {},
                \"shortDescription\": {
                  \"text\": \"CWE #CWE-121\"
                }
              }
            ],
            \"version\": \"1.0.0\"
          }
        },
        \"versionControlProvenance\": [
          {
            \"branch\": \"refs/heads/challenges/full-scan\",
            \"repositoryUri\": \"https://github.com/aixcc-finals/example-libpng\",
            \"revisionId\": \"fdacd5a1dcff42175117d674b0fda9f8a005ae88\"
          }
        ]
      }
    ],
    \"\$schema\": \"https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json\",
    \"version\": \"2.1.0\"
  }
}"