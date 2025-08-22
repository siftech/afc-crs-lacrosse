``` C
char* ptr = (char*)malloc (SIZE);
if (err) {
  abrt = 1;
  free(ptr);
 }
...
if (abrt) {
  logError("operation aborted before commit", ptr);
 }
```
