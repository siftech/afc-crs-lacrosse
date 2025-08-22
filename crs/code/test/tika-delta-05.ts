curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "4d5194b7d13494f97b89c859282342f5efad9cef",
  "challenge_repo_head_ref": "challenges/tk-delta-05",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-05",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'