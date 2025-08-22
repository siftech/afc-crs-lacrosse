curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-curl.git",
  "challenge_repo_base_ref": "a29184fc5f9b1474c08502d1545cd90375fadd51",
  "challenge_repo_head_ref": "challenges/cu-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cu-delta-01",
  "fuzz_tooling_project_name": "curl",
  "duration": 36000
}'