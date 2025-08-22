``` java
String path = getInputPath();
if (path.startsWith("/safe_dir/"))
    {
        File f = new File(path);
        f.delete()
            }
```
