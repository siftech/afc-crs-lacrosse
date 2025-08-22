curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
   "challenge_repo_head_ref": "challenges/cc-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-full-01",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 3600000
}'

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
    "challenge_repo_head_ref": "challenges/zk-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/zk-full-01",
    "fuzz_tooling_project_name": "zookeeper",
    "duration": 3600000
}'

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-dropbear.git",
   "challenge_repo_head_ref": "challenges/db-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/db-full-01",
  "fuzz_tooling_project_name": "dropbear",
  "duration": 3600000
}'
