curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
    "challenge_repo_base_ref": "a92cc0f3ebc3d3f4cf5b6097920a391e9b5fcfcf",
    "challenge_repo_head_ref": "challenges/fp-delta-01",    
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/fp-delta-01",
    "fuzz_tooling_project_name": "freerdp",
    "duration": 3600000
}'


