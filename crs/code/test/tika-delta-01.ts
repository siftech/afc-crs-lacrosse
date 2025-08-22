curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "d0e3069a8e51554083c2980974f869337b4d6d39",
  "challenge_repo_head_ref": "challenges/tk-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'