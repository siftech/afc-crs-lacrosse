curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
    "challenge_repo_base_ref": "5bf8da2d7953974e5dfbd778429c3affd461f51a",
    "challenge_repo_head_ref": "challenges/lp-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lp-delta-01",
    "fuzz_tooling_project_name": "libpng",
    "duration": 3600000
}'