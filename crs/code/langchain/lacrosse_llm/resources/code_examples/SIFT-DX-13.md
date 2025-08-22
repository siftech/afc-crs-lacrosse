``` Java
String script = System.getProperty("SCRIPTNAME");
if (script != null)
    System.exec(script);
```
