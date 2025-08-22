curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
  "challenge_repo_base_ref": "154edd0066d1aaf18daafb88253cacbf39017d61",
  "challenge_repo_head_ref": "challenges/cc-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-delta-02",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 3600000
}'