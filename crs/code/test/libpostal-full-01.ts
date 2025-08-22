curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-libpostal.git",
  "challenge_repo_head_ref": "challenges/libpostal-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/libpostal-full-01",
  "fuzz_tooling_project_name": "libpostal",
  "duration": 36000
}'