curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
  "challenge_repo_head_ref": "challenges/fp-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/fp-full-01",
  "fuzz_tooling_project_name": "freerdp",
  "duration": 3600000
}'

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
    "challenge_repo_head_ref": "challenges/sq-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/sq-full-01",
    "fuzz_tooling_project_name": "sqlite3",
    "duration": 3600000
}'

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_head_ref": "challenges/lx-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-full-01",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 3600000
}'
