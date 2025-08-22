curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
    "challenge_repo_head_ref": "challenges/fp-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/fp-full-01",
    "fuzz_tooling_project_name": "freerdp",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
    "challenge_repo_head_ref": "challenges/sq-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/sq-full-01",
    "fuzz_tooling_project_name": "sqlite3",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_head_ref": "challenges/lx-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-full-01",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 14400 
}'

sleep 30m;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
    "challenge_repo_head_ref": "challenges/cc-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/cc-full-01",
    "fuzz_tooling_project_name": "apache-commons-compress",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
    "challenge_repo_head_ref": "challenges/zk-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/zk-full-01",
    "fuzz_tooling_project_name": "zookeeper",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-dropbear.git",
    "challenge_repo_head_ref": "challenges/db-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/db-full-01",
    "fuzz_tooling_project_name": "dropbear",
    "duration": 14400 
}'

sleep 30m;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
    "challenge_repo_base_ref": "a92cc0f3ebc3d3f4cf5b6097920a391e9b5fcfcf",
    "challenge_repo_head_ref": "challenges/fp-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/fp-delta-01",
    "fuzz_tooling_project_name": "freerdp",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_base_ref": "0f876b983249cd3fb32b53d405f5985e83d8c3bd",
    "challenge_repo_head_ref": "challenges/lx-delta-02",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-delta-02",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/integration-test.git",
    "challenge_repo_base_ref": "4a714359c60858e3821bd478dc846de1d04dc977",
    "challenge_repo_head_ref": "challenges/integration-test-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/integration-test-delta-01",
    "fuzz_tooling_project_name": "integration-test",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
    "challenge_repo_base_ref": "5bf8da2d7953974e5dfbd778429c3affd461f51a",
    "challenge_repo_head_ref": "challenges/lp-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lp-delta-01",
    "fuzz_tooling_project_name": "libpng",
    "duration": 14400 
}'

sleep 30m;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
    "challenge_repo_base_ref": "6a3e7f57f00f0a2b6b89b0db7990e3df47175372",
    "challenge_repo_head_ref": "challenges/sq-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/sq-delta-01",
    "fuzz_tooling_project_name": "sqlite3",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_base_ref": "792cc4a1462d4a969d9d38bd80a52d2e4f7bd137",
    "challenge_repo_head_ref": "9d1cb67c31933ee5ae3ee458940f7dbeb2fde8b8",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-ex1-delta-01",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 14400 
}'

sleep 30m;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
    "challenge_repo_base_ref": "f6f34f6d5b6d67205c34de617a0b99fe11e3d323",
    "challenge_repo_head_ref": "challenges/zk-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/zk-delta-01",
    "fuzz_tooling_project_name": "zookeeper",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
    "challenge_repo_base_ref": "154edd0066d1aaf18daafb88253cacbf39017d61",	
    "challenge_repo_head_ref": "challenges/cc-delta-02",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/cc-delta-02",
    "fuzz_tooling_project_name": "apache-commons-compress",
    "duration": 14400 
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
    "challenge_repo_base_ref": "154edd0066d1aaf18daafb88253cacbf39017d61",
    "challenge_repo_head_ref": "challenges/cc-delta-03",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/cc-delta-03",
    "fuzz_tooling_project_name": "apache-commons-compress",
    "duration": 14400 
}'

sleep 30m;