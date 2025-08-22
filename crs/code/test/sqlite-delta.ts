curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
    "challenge_repo_base_ref": "6a3e7f57f00f0a2b6b89b0db7990e3df47175372",
    "challenge_repo_head_ref": "challenges/sq-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/sq-delta-01",
    "fuzz_tooling_project_name": "sqlite3",
    "duration": 3600000
}'