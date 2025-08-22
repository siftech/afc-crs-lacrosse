curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-public/challenge-004-nginx-source.git",
  "challenge_repo_head_ref": "hg-asc-v2.0.0",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "d5fbd68fca66e6fa4f05899170d24e572b01853d",
  "fuzz_tooling_project_name": "nginx",
  "duration": 3600000
}'