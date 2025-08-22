curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_base_ref": "792cc4a1462d4a969d9d38bd80a52d2e4f7bd137",
    "challenge_repo_head_ref": "9d1cb67c31933ee5ae3ee458940f7dbeb2fde8b8",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-ex1-delta-01",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 300
}'

sleep 6m

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
    "challenge_repo_base_ref": "0cc367aaeaac3f888f255cee5d394968996f736e",
    "challenge_repo_head_ref": "fdacd5a1dcff42175117d674b0fda9f8a005ae88",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "v1.0.0",
    "fuzz_tooling_project_name": "libpng",
    "duration": 6000
}'