curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-s2n-tls.git",
  "challenge_repo_head_ref": "challenges/s2n_tls-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/s2n_tls-full-01",
  "fuzz_tooling_project_name": "s2n-tls",
  "duration": 36000,
  "harnesses_included":false
}'