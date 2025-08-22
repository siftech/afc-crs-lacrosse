curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-libexif.git",
  "challenge_repo_base_ref": "ffcdfbeb5539c25b1630ba59abf8a22587657adc",
  "challenge_repo_head_ref": "challenges/ex-delta-01",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/ex-delta-01",
  "fuzz_tooling_project_name": "libexif",
  "duration": 36000
}'