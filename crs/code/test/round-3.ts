curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "35af1ffb5dd21ae47332577c2b6c889da302b497",
  "challenge_repo_head_ref": "challenges/sq-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-03",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "d0e3069a8e51554083c2980974f869337b4d6d39",
  "challenge_repo_head_ref": "challenges/tk-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "87c62bccc3a6fd0343df073511fc520a235618b3",
  "challenge_repo_head_ref": "challenges/tk-delta-02",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-02",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "08dabf212d551b27de70d3be0653a226e85b1b73",
  "challenge_repo_head_ref": "challenges/tk-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-03",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "30284a3eb45eddd5b812eca12254a99551671f32",
  "challenge_repo_head_ref": "challenges/tk-delta-04",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-04",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_base_ref": "4d5194b7d13494f97b89c859282342f5efad9cef",
  "challenge_repo_head_ref": "challenges/tk-delta-05",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-delta-05",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-tika.git",
  "challenge_repo_head_ref": "challenges/tk-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/tk-full-01",
  "fuzz_tooling_project_name": "tika",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-s2n-tls.git",
  "challenge_repo_head_ref": "challenges/s2n_tls-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/s2n_tls-full-01",
  "fuzz_tooling_project_name": "s2n-tls",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-ipf.git",
  "challenge_repo_head_ref": "challenges/ipf-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/ipf-full-01",
  "fuzz_tooling_project_name": "ipf",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-libexif.git",
  "challenge_repo_base_ref": "ffcdfbeb5539c25b1630ba59abf8a22587657adc",
  "challenge_repo_head_ref": "challenges/ex-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/ex-delta-01",
  "fuzz_tooling_project_name": "libexif",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-curl.git",
  "challenge_repo_head_ref": "challenges/cu-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cu-full-01",
  "fuzz_tooling_project_name": "curl",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-libpostal.git",
  "challenge_repo_head_ref": "challenges/libpostal-full-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/libpostal-full-01",
  "fuzz_tooling_project_name": "libpostal",
  "duration": 36000
}'

sleep 30s;

curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-curl.git",
  "challenge_repo_base_ref": "a29184fc5f9b1474c08502d1545cd90375fadd51",
  "challenge_repo_head_ref": "challenges/cu-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/cu-delta-01",
  "fuzz_tooling_project_name": "curl",
  "duration": 36000
}'
