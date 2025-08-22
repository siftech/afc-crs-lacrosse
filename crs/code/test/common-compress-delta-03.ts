curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
  "challenge_repo_base_ref": "6e608498013784abb6878cad7906c2ddc41e45f1",
  "challenge_repo_head_ref": "challenges/cc-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-delta-03",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 3600000
}'