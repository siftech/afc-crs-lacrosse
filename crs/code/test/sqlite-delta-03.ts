curl -X 'POST' $LOCAL_EX_COMP_SERVER_ENDPOINT -H 'Content-Type: application/json' -d '{
  "challenge_repo_url": "https://github.com/aixcc-finals/afc-sqlite3.git",
  "challenge_repo_base_ref": "35af1ffb5dd21ae47332577c2b6c889da302b497",
  "challenge_repo_head_ref": "challenges/sq-delta-03",
  "fuzz_tooling_url": "https://github.com/aixcc-finals/oss-fuzz-aixcc.git",
  "fuzz_tooling_ref": "challenge-state/sq-delta-03",
  "fuzz_tooling_project_name": "sqlite3",
  "duration": 36000
}'