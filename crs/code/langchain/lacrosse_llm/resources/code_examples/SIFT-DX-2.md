``` C
short int bytesRec = 0;
char buf[SOMEBIGNUM];

while(bytesRec < MAXGET) {
  bytesRec += getFromInput(buf+bytesRec);
 }
```
