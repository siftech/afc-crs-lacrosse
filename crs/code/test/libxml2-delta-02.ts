curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_base_ref": "0f876b983249cd3fb32b53d405f5985e83d8c3bd",
    "challenge_repo_head_ref": "challenges/lx-delta-02",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-delta-02",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 3600000
}'


