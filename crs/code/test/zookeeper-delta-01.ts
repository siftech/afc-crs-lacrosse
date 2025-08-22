curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
    "challenge_repo_base_ref": "f6f34f6d5b6d67205c34de617a0b99fe11e3d323",
    "challenge_repo_head_ref": "challenges/zk-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/zk-delta-01",
    "fuzz_tooling_project_name": "zookeeper",
    "duration": 3600000
}'