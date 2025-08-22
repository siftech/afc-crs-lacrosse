curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
  "challenge_repo_base_ref": "a92cc0f3ebc3d3f4cf5b6097920a391e9b5fcfcf",
  "challenge_repo_head_ref": "challenges/fp-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/fp-delta-01",
  "fuzz_tooling_project_name": "freerdp",
  "duration": 3600000
}'

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_base_ref": "0f876b983249cd3fb32b53d405f5985e83d8c3bd",
    "challenge_repo_head_ref": "challenges/lx-delta-02",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-delta-02",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 3600000
}'

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/integration-test.git",
  "challenge_repo_base_ref": "4a714359c60858e3821bd478dc846de1d04dc977",
  "challenge_repo_head_ref": "challenges/integration-test-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/integration-test-delta-01",
  "fuzz_tooling_project_name": "integration-test",
  "duration": 3600000
}'

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
  "challenge_repo_base_ref": "5bf8da2d7953974e5dfbd778429c3affd461f51a",
  "challenge_repo_head_ref": "challenges/lp-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/lp-delta-01",
  "fuzz_tooling_project_name": "libpng",
  "duration": 3600000
}'
