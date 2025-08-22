curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
    "challenge_repo_base_ref": "d19cef9ca254a4c1461490ed8b82ffccfa57461d",
    "challenge_repo_head_ref": "5ee4f185d0431cc88f365ce779aa04a87fe7690f",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/zk-ex1-delta-01",
    "fuzz_tooling_project_name": "zookeeper",
    "duration": 3600000
}'