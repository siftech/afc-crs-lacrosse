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