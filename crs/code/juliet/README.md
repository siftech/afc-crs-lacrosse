Scripts to run analysis tools against juliet.

Download the juliet tests
```bash
./java/download-juliet.sh
```

Build the docker images
```bash
./docker/bake.sh
```

Run a tool (infer) against juliet
```bash
./infer/run.sh
```

View results
```bash
./infer/shell.sh
# In docker:
infer explore -o /workspace/output/CWE190_Integer_Overflow/s01
# infer explore -o /workspace/output/CWE190_Integer_Overflow/s02
# infer explore -o /workspace/output/CWE190_Integer_Overflow/s03
# etc
```
