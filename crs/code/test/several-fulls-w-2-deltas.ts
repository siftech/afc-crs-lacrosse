curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
    "challenge_repo_head_ref": "fdacd5a1dcff42175117d674b0fda9f8a005ae88",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "v1.0.0",
    "fuzz_tooling_project_name": "libpng",
    "duration": 18000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_head_ref": "challenges/tk-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-full-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 18000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
   "challenge_repo_head_ref": "challenges/cc-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-full-01",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 18000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
    "challenge_repo_head_ref": "challenges/zk-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/zk-full-01",
    "fuzz_tooling_project_name": "zookeeper",
    "duration": 18000
}'

sleep 120m;

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
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-curl.git",
  "challenge_repo_head_ref": "challenges/cu-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cu-full-01",
  "fuzz_tooling_project_name": "curl",
  "duration": 18000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "git@github.com:aixcc-finals/afc-freerdp.git",
  "challenge_repo_head_ref": "challenges/fp-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/fp-full-01",
  "fuzz_tooling_project_name": "freerdp",
  "duration": 18000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
    "challenge_repo_head_ref": "challenges/sq-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/sq-full-01",
    "fuzz_tooling_project_name": "sqlite3",
    "duration": 18000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_head_ref": "challenges/tk-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-full-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 18000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
    "challenge_repo_url": "git@github.com:aixcc-finals/afc-libxml2.git",
    "challenge_repo_head_ref": "challenges/lx-full-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lx-full-01",
    "fuzz_tooling_project_name": "libxml2",
    "duration": 18000
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