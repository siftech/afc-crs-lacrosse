curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "d0e3069a8e51554083c2980974f869337b4d6d39",
  "challenge_repo_head_ref": "challenges/tk-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 14400
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "87c62bccc3a6fd0343df073511fc520a235618b3",
  "challenge_repo_head_ref": "challenges/tk-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-02",
  "fuzz_tooling_project_name": "tika",
  "duration": 14400
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "08dabf212d551b27de70d3be0653a226e85b1b73",
  "challenge_repo_head_ref": "challenges/tk-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-03",
  "fuzz_tooling_project_name": "tika",
  "duration": 14400
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "30284a3eb45eddd5b812eca12254a99551671f32",
  "challenge_repo_head_ref": "challenges/tk-delta-04",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-04",
  "fuzz_tooling_project_name": "tika",
  "duration": 14400
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "4d5194b7d13494f97b89c859282342f5efad9cef",
  "challenge_repo_head_ref": "challenges/tk-delta-05",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-05",
  "fuzz_tooling_project_name": "tika",
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

sleep 60m;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-commons-compress.git",
  "challenge_repo_base_ref": "6e608498013784abb6878cad7906c2ddc41e45f1",
  "challenge_repo_head_ref": "challenges/cc-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cc-delta-03",
  "fuzz_tooling_project_name": "apache-commons-compress",
  "duration": 14400
}'

sleep 30s;

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
    "challenge_repo_url": "https://github.com/aixcc-finals/afc-zookeeper.git",
    "challenge_repo_base_ref": "7f350901823080c5dfa176b37c3f56f121dcd718",
    "challenge_repo_head_ref": "challenges/zk-delta-02",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/zk-delta-02",
    "fuzz_tooling_project_name": "zookeeper",
    "duration": 14400
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-libexif.git",
  "challenge_repo_base_ref": "ffcdfbeb5539c25b1630ba59abf8a22587657adc",
  "challenge_repo_head_ref": "challenges/ex-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/ex-delta-01",
  "fuzz_tooling_project_name": "libexif",
  "duration": 14400
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-curl.git",
  "challenge_repo_base_ref": "a29184fc5f9b1474c08502d1545cd90375fadd51",
  "challenge_repo_head_ref": "challenges/cu-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cu-delta-01",
  "fuzz_tooling_project_name": "curl",
  "duration": 14400
}'

sleep 60m;

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
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "d6a2180510e6fb05277f8325f132605399528505",
  "challenge_repo_head_ref": "challenges/sq-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-02",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 14400
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "35af1ffb5dd21ae47332577c2b6c889da302b497",
  "challenge_repo_head_ref": "challenges/sq-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-03",
  "fuzz_tooling_project_name": "sqlite3",
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

sleep 60m;

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
    "challenge_repo_url": "git@github.com:aixcc-finals/example-libpng.git",
    "challenge_repo_base_ref": "5bf8da2d7953974e5dfbd778429c3affd461f51a",
    "challenge_repo_head_ref": "challenges/lp-delta-01",
    "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
    "fuzz_tooling_ref": "challenge-state/lp-delta-01",
    "fuzz_tooling_project_name": "libpng",
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


