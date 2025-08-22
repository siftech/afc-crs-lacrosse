# 362
## Example 1: perl
``` perl
$transfer_amount = GetTransferAmount();
$balance = GetBalanceFromDatabase();

if ($transfer_amount < 0) {
  FatalError("Bad Transfer Amount");
}
$newbalance = $balance - $transfer_amount;
if (($balance - $transfer_amount) < 0) {
  FatalError("Insufficient Funds");
}
SendNewBalanceToDatabase($newbalance);
NotifyUser("Transfer of $transfer_amount succeeded.");
NotifyUser("New balance: $newbalance");
```
## Example 2: C: DX-24
``` C
void f(pthread_mutex_t *mutex) {
  pthread_mutex_lock(mutex);

  /* access shared resource */


  pthread_mutex_unlock(mutex);
}
```
# 352
# 502
## Example 1: Java: SIFT-DX-10
``` Java
try {
    File file = new File("object.obj");
    ObjectInputStream in = new ObjectInputStream(new FileInputStream(file));
    javax.swing.JButton button = (javax.swing.JButton) in.readObject();
    in.close();
}
```
# 287
## Example 1: perl
``` perl
my $q = new CGI;

if ($q->cookie('loggedin') ne "true") {
  if (! AuthenticateUser($q->param('username'), $q->param('password'))) {
    ExitError("Error: you need to log in first");
  }
  else {
    # Set loggedin and user cookies.
    $q->cookie(
      -name => 'loggedin',
      -value => 'true'
        );

    $q->cookie(
      -name => 'user',
      -value => $q->param('username')
        );
  }
}

if ($q->cookie('user') eq "Administrator") {
  DoAdministratorTasks();
}
```
# 94
## Example 2: perl
``` perl
use CGI qw(:standard);

sub config_file_add_key {
  my ($fname, $key, $arg) = @_;

  # code to add a field/key to a file goes here
}

sub config_file_set_key {
  my ($fname, $key, $arg) = @_;

  # code to set key to a particular file goes here
}

sub config_file_delete_key {
  my ($fname, $key, $arg) = @_;

  # code to delete key from a particular file goes here
}

sub handleConfigAction {
  my ($fname, $action) = @_;
  my $key = param('key');
  my $val = param('val');

  # this is super-efficient code, especially if you have to invoke


  # any one of dozens of different functions!

  my $code = "config_file_$action_key(\$fname, \$key, \$val);";
  eval($code);
}

$configfile = "/home/cwe/config.txt";
print header;
if (defined(param('action'))) {
  handleConfigAction($configfile, param('action'));
}
else {
  print "No action specified!\n";
}
```
## Example 3: Python
### Bad
``` python
def main():
    sum = 0
    numbers = eval(input("Enter a space-separated list of numbers: "))
    for num in numbers:
        sum = sum + num
    print(f"Sum of {numbers} = {sum}")

main()
```
### Good
``` python
def main():
    sum = 0
    numbers = input("Enter a space-separated list of numbers: ").split(" ")
    try:
        for num in numbers:
            sum = sum + int(num)
        print(f"Sum of {numbers} = {sum}")
    except ValueError:
        print("Error: invalid input")
main()
```
# 20
## Example 1: Java: DX-135
``` java
...
public static final double price = 20.00;
int quantity = currentUser.getAttribute("quantity");
double total = price * quantity;
chargeUser(total);
...
```
## Example 2: C: DX-136
``` C
...
#define MAX_DIM 100
...
/* board dimensions */

int m,n, error;
board_square_t *board;
printf("Please specify the board height: \n");
error = scanf("%d", &m);
if ( EOF == error ){
  die("No integer passed: Die evil hacker!\n");
 }
printf("Please specify the board width: \n");
error = scanf("%d", &n);
if ( EOF == error ){
  die("No integer passed: Die evil hacker!\n");
 }
if ( m > MAX_DIM || n > MAX_DIM ) {
  die("Value too large: Die evil hacker!\n");
 }
board = (board_square_t*) malloc( m * n * sizeof(board_square_t));
...
```
## Example 4: Java: DX-34
``` java
private void buildList ( int untrustedListSize ){
    if ( 0 > untrustedListSize ){
        die("Negative value supplied for list size, die evil hacker!");
    }
    Widget[] list = new Widget [ untrustedListSize ];
    list[0] = new Widget();
}
```
## Example 5: Java: DX-110
``` java
...
    IntentFilter filter = new IntentFilter("com.example.URLHandler.openURL");
MyReceiver receiver = new MyReceiver();
registerReceiver(receiver, filter);
...

    public class UrlHandlerReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            if("com.example.URLHandler.openURL".equals(intent.getAction())) {
                String URL = intent.getStringExtra("URLToOpen");
                int length = URL.length();

                ...
                    }
        }
    }
```
# 22
## Example 1: Perl: DX-27
``` perl
my $dataPath = "/users/cwe/profiles";
my $username = param("user");
my $profilePath = $dataPath . "/" . $username;

open(my $fh, "<", $profilePath) || ExitError("profile read error: $profilePath");
print "<ul>\n";
while (<$fh>) {
  print "<li>$_</li>\n";
}
print "</ul>\n";
```
## Example 2: Java: DX-18
``` java
String filename = System.getProperty("com.domain.application.dictionaryFile");
File dictionaryFile = new File(filename);
```
## Example 3: Perl
``` perl
my $Username = GetUntrustedInput();
$Username =~ s/\.\.\///;
my $filename = "/home/user/" . $Username;
ReadAndSendFile($filename);
```
## Example 4: Java: SIFT-DX-4
``` java
String path = getInputPath();
if (path.startsWith("/safe_dir/"))
    {
        File f = new File(path);
        f.delete()
            }
```
## Example 6: Python
### Bad
``` python
import os
import sys
def main():
    filename = sys.argv[1]
    path = os.path.join(os.getcwd(), filename)
    try:
    with open(path, 'r') as f:
        file_data = f.read()
    except FileNotFoundError as e:
        print("Error - file not found")
main()
```
### Good
``` python
import os
import sys
def main():
    filename = sys.argv[1]
    path = os.path.normpath(f"{os.getcwd()}{os.sep}{filename}")
    try:
    with open(path, 'r') as f:
        file_data = f.read()
    except FileNotFoundError as e:
        print("Error - file not found")
main()
```
# 79
# 77
## Example 1: C: DX-30
``` C
int main(int argc, char** argv) {
  char cmd[CMD_MAX] = "/usr/bin/cat ";
  strcat(cmd, argv[1]);
  system(cmd);
}
```
## Example 2: Java: DX-28
``` java
...
    String btype = request.getParameter("backuptype");
String cmd = new String("cmd.exe /K \"
  c:\\util\\rmanDB.bat "
   +btype+
   "&&c:\\utl\\cleanup.bat\"")

System.Runtime.getRuntime().exec(cmd);
...
```
## Example 3: Java: SIFT-DX-11
``` java
...
String home = System.getProperty("APPHOME");
String cmd = home + INITCMD;
java.lang.Runtime.getRuntime().exec(cmd);
...
```
## Example 4: C: SIFT-DX-12
``` C
#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv) {

  char cat[] = "cat ";
  char *command;
  size_t commandLength;

  commandLength = strlen(cat) + strlen(argv[1]) + 1;
  command = (char *) malloc(commandLength);
  strncpy(command, cat, commandLength);
  strncat(command, argv[1], (commandLength - strlen(cat)) );

  system(command);
  return (0);
}
```
# 78
## Example 2: C
``` C
int main(int argc, char** argv) {
  char cmd[CMD_MAX] = "/usr/bin/cat ";
  strcat(cmd, argv[1]);
  system(cmd);
}
```
## Example 3: Perl
``` perl
use CGI qw(:standard);
$name = param('name');
$nslookup = "/path/to/nslookup";
print header;
if (open($fh, "$nslookup $name|")) {
  while (<$fh>) {
    print escapeHTML($_);
    print "<br>\n";
  }
  close($fh);
}
```
## Example 4: Java: SIFT-DX-13
``` Java
String script = System.getProperty("SCRIPTNAME");
if (script != null)
    System.exec(script);
```
## Example 5: Java: SIFT-DX-14
``` Java
public String coordinateTransformLatLonToUTM(String coordinates)
{
    String utmCoords = null;
    try {
        String latlonCoords = coordinates;
        Runtime rt = Runtime.getRuntime();
        Process exec = rt.exec("cmd.exe /C latlon2utm.exe -" + latlonCoords);
        // process results of coordinate transform

        // ...
    }
    catch(Exception e) {...}
    return utmCoords;
}
```
## Example 6: Java: DX-28
``` Java
...
    String btype = request.getParameter("backuptype");
String cmd = new String("cmd.exe /K c:\\util\\rmanDB.bat "
                        +btype+
                        "&&c:\\utl\\cleanup.bat\"")

System.Runtime.getRuntime().exec(cmd);
...
```
# 89
## Example 6: perl
``` perl
$userKey = getUserID();
$name = getUserInput();

# ensure only letters, hyphens and apostrophe are allowed
$name = allowList($name, "^a-zA-z'-$");
$query = "INSERT INTO last_names VALUES('$userKey', '$name')";
```
# 269
## Example 1: Python
``` Python
def makeNewUserDir(username):
    if invalidUsername(username):

    #avoid CWE-22 and CWE-78
    print('Usernames cannot contain invalid characters')
    return False

    try:
        raisePrivileges()
        os.mkdir('/home/' + username)
        lowerPrivileges()
    except OSError:
        print('Unable to create new user directory for user:' + username)
        return False

    return True
```
## Example 2: C: DX-97
``` C
seteuid(0);
/* do some stuff */

seteuid(getuid());
```
## Example 3: Java: DX-142
``` Java
AccessController.doPrivileged(new PrivilegedAction() {
        public Object run() {
            // privileged code goes here, for example:
            System.loadLibrary("awt");
            return null;
            // nothing to return
        }
```
## Example 4: Java: DX-127
``` Java
public enum Roles {
    ADMIN,USER,GUEST
}

public void printDebugInfo(User requestingUser){
    if(isAuthenticated(requestingUser)){
        switch(requestingUser.role){
        case GUEST:
            System.out.println("You are not authorized to perform this command");
            break;

        default:
            System.out.println(currentDebugState());
            break;
        }
    }
    else{
        System.out.println("You must be logged in to perform this command");
    }
}
```

## Example 5: Java: DX-128
``` Java
public enum Roles {
    ADMIN,OPERATOR,USER,GUEST
}

public void resetPassword(User requestingUser, User user, String password ){
    if(isAuthenticated(requestingUser)){
        switch(requestingUser.role){
        case GUEST:
            System.out.println("You are not authorized to perform this command");
            break;

        case USER:
            System.out.println("You are not authorized to perform this command");
            break;

        default:
            setPassword(user,password);
            break;
        }
    }

    else{
        System.out.println("You must be logged in to perform this command");
    }
}
```
# 119
## Example 1: C
``` C
void host_lookup(char *user_supplied_addr){
  struct hostent *hp;
  in_addr_t *addr;
  char hostname[64];
  in_addr_t inet_addr(const char *cp);

  /*routine that ensures user_supplied_addr is in the right format for conversion */

  validate_addr_form(user_supplied_addr);
  addr = inet_addr(user_supplied_addr);
  hp = gethostbyaddr( addr, sizeof(struct in_addr), AF_INET);
  strcpy(hostname, hp->h_name);
}
```
## Example 2: C
``` C
char * copy_input(char *user_supplied_string){
  int i, dst_index;
  char *dst_buf = (char*)malloc(4*sizeof(char) * MAX_SIZE);
  if ( MAX_SIZE <= strlen(user_supplied_string) ){
    die("user string too long, die evil hacker!");
  }
  dst_index = 0;
  for ( i = 0; i < strlen(user_supplied_string); i++ ){
    if( '&' == user_supplied_string[i] ){
      dst_buf[dst_index++] = '&';
      dst_buf[dst_index++] = 'a';
      dst_buf[dst_index++] = 'm';
      dst_buf[dst_index++] = 'p';
      dst_buf[dst_index++] = ';';
    }
    else if ('<' == user_supplied_string[i] ){
      /* encode to &lt; */
    }
    else dst_buf[dst_index++] = user_supplied_string[i];
  }
  return dst_buf;
}
```
## Example 3: C
``` C
int main (int argc, char **argv) {
  char *items[] = {"boat", "car", "truck", "train"};
  int index = GetUntrustedOffset();
  printf("You selected %s\n", items[index-1]);
}
```
## Example 4: C
### Bad
``` C
int getValueFromArray(int *array, int len, int index) {

  int value;

  // check that the array index is less than the maximum

  // length of the array
  if (index < len) {
    // get the value at the specified index of the array
    value = array[index];
  }
  // if array index is invalid then output error message
  // and return value indicating error
  else {
    printf("Value is: %d\n", array[index]);
    value = -1;
  }

  return value;
}
```
### Good
``` C
...

// check that the array index is within the correct

// range of values for the array
if (index >= 0 && index < len) {

...
```
# 863
# 276
# 190
## Example 1: C
``` C
img_t table_ptr; /*struct containing img data, 10kB each*/
int num_imgs;
...
num_imgs = get_num_imgs();
table_ptr = (img_t*)malloc(sizeof(img_t)*num_imgs);
...
```
## Example 2: C
``` C
nresp = packet_get_int();
if (nresp > 0) {
  response = xmalloc(nresp*sizeof(char*));
  for (i = 0; i < nresp; i++) response[i] = packet_get_string(NULL);
 }
```
## Example 3: C
``` C
short int bytesRec = 0;
char buf[SOMEBIGNUM];

while(bytesRec < MAXGET) {
  bytesRec += getFromInput(buf+bytesRec);
 }
```
## Example 4: C
### Bad
``` C
#define JAN 1
#define FEB 2
#define MAR 3

short getMonthlySales(int month) {...}

float calculateRevenueForQuarter(short quarterSold) {...}

int determineFirstQuarterRevenue() {

  // Variable for sales revenue for the quarter
  float quarterRevenue = 0.0f;

  short JanSold = getMonthlySales(JAN); /* Get sales in January */
  short FebSold = getMonthlySales(FEB); /* Get sales in February */
  short MarSold = getMonthlySales(MAR); /* Get sales in March */

  // Calculate quarterly total
  short quarterSold = JanSold + FebSold + MarSold;

  // Calculate the total revenue for the quarter
  quarterRevenue = calculateRevenueForQuarter(quarterSold);

  saveFirstQuarterRevenue(quarterRevenue);

  return 0;
}
```
### Good
``` C
...
float calculateRevenueForQuarter(long quarterSold) {...}

int determineFirstQuarterRevenue() {
  ...
    // Calculate quarterly total
    long quarterSold = JanSold + FebSold + MarSold;

  // Calculate the total revenue for the quarter
  quarterRevenue = calculateRevenueForQuarter(quarterSold);

  ...
    }
```
# 306
## Example 1: Java: SIFT-DX-5
### Bad
``` Java
public BankAccount createBankAccount(String accountNumber, String accountType,
                                     String accountName, String accountSSN, double balance) {
    BankAccount account = new BankAccount();
    account.setAccountNumber(accountNumber);
    account.setAccountType(accountType);
    account.setAccountOwnerName(accountName);
    account.setAccountOwnerSSN(accountSSN);
    account.setBalance(balance);

    return account;
}
```
### Good
``` Java
private boolean isUserAuthentic = false;

// authenticate user,

// if user is authenticated then set variable to true

// otherwise set variable to false
public boolean authenticateUser(String username, String password) {
    ...
        }

public BankAccount createNewBankAccount(String accountNumber, String accountType,
                                        String accountName, String accountSSN, double balance) {
    BankAccount account = null;

    if (isUserAuthentic) {
        account = new BankAccount();
        account.setAccountNumber(accountNumber);
        account.setAccountType(accountType);
        account.setAccountOwnerName(accountName);
        account.setAccountOwnerSSN(accountSSN);
        account.setBalance(balance);
    }
    return account;
}
```
# 862
## Example 2: Perl
``` Perl
sub DisplayPrivateMessage {
  my($id) = @_;
  my $Message = LookupMessageObject($id);
  print "From: " . encodeHTML($Message->{from}) . "<br>\n";
  print "Subject: " . encodeHTML($Message->{subject}) . "\n";
  print "<hr>\n";
  print "Body: " . encodeHTML($Message->{body}) . "\n";
}

my $q = new CGI;
# For purposes of this example, assume that CWE-309 and


# CWE-523 do not apply.
if (! AuthenticateUser($q->param('username'), $q->param('password'))) {
  ExitError("invalid username or password");
}

my $id = $q->param('id');
DisplayPrivateMessage($id);
```
# 476
## Example 1: C: SIFT-DX-8
### Good
``` C
if (pointer1 != NULL) {

  /* make use of pointer1 */

  /* ... */
 }
```
## Example 2: C: DX-1
``` C
void host_lookup(char *user_supplied_addr){
  struct hostent *hp;
  in_addr_t *addr;
  char hostname[64];
  in_addr_t inet_addr(const char *cp);

  /*routine that ensures user_supplied_addr is in the right format for conversion */

  validate_addr_form(user_supplied_addr);
  addr = inet_addr(user_supplied_addr);
  hp = gethostbyaddr( addr, sizeof(struct in_addr), AF_INET);
  strcpy(hostname, hp->h_name);
}
```
## Example 3: Java: SIFT-DX-9
``` Java
String cmd = System.getProperty("cmd");
cmd = cmd.trim();
```
# 125
## Example 1: C
### Bad
``` C
int getValueFromArray(int *array, int len, int index) {

  int value;

  // check that the array index is less than the maximum

  // length of the array
  if (index < len) {

    // get the value at the specified index of the array
    value = array[index];
  }
  // if array index is invalid then output error message

  // and return value indicating error
  else {
    printf("Value is: %d\n", array[index]);
    value = -1;
  }

  return value;
}
```
### Good
``` C
...

// check that the array index is within the correct

// range of values for the array
if (index >= 0 && index < len) {
    ...
    }
```
# 787
## Example 1: C: SIFT-DX-15
``` C
int id_sequence[3];

/* Populate the id array. */

id_sequence[0] = 123;
id_sequence[1] = 234;
id_sequence[2] = 345;
id_sequence[3] = 456;
```
## Example 2: C: DX-114
``` C
int returnChunkSize(void *) {

  /* if chunk info is valid, return the size of usable memory,

   * else, return -1 to indicate an error

   */
  ...
    }
int main() {
  ...
    memcpy(destBuf, srcBuf, (returnChunkSize(destBuf)-1));
  ...
    }
```
## Example 3: C: DX-1
``` C
void host_lookup(char *user_supplied_addr){
  struct hostent *hp;
  in_addr_t *addr;
  char hostname[64];
  in_addr_t inet_addr(const char *cp);

  /*routine that ensures user_supplied_addr is in the right format for conversion */

  validate_addr_form(user_supplied_addr);
  addr = inet_addr(user_supplied_addr);
  hp = gethostbyaddr( addr, sizeof(struct in_addr), AF_INET);
  strcpy(hostname, hp->h_name);
}
```
## Example 4: C: DX-19
``` C
char * copy_input(char *user_supplied_string){
  int i, dst_index;
  char *dst_buf = (char*)malloc(4*sizeof(char) * MAX_SIZE);
  if ( MAX_SIZE <= strlen(user_supplied_string) ){
    die("user string too long, die evil hacker!");
  }
  dst_index = 0;
  for ( i = 0; i < strlen(user_supplied_string); i++ ){
    if( '&' == user_supplied_string[i] ){
      dst_buf[dst_index++] = '&';
      dst_buf[dst_index++] = 'a';
      dst_buf[dst_index++] = 'm';
      dst_buf[dst_index++] = 'p';
      dst_buf[dst_index++] = ';';
    }
    else if ('<' == user_supplied_string[i] ){

      /* encode to &lt; */
    }
    else dst_buf[dst_index++] = user_supplied_string[i];
  }
  return dst_buf;
}
```
## Example 5: C: DX-87
``` C
char* trimTrailingWhitespace(char *strMessage, int length) {
  char *retMessage;
  char *message = malloc(sizeof(char)*(length+1));

  // copy input string to a temporary string
  char message[length+1];
  int index;
  for (index = 0; index < length; index++) {
    message[index] = strMessage[index];
  }
  message[index] = '\0';

  // trim trailing whitespace
  int len = index-1;
  while (isspace(message[len])) {
    message[len] = '\0';
    len--;
  }

  // return string without trailing whitespace
  retMessage = message;
  return retMessage;
}
```
## Example 6: C: DX-20
``` C
int i;
unsigned int numWidgets;
Widget **WidgetList;

numWidgets = GetUntrustedSizeValue();
if ((numWidgets == 0) || (numWidgets > MAX_NUM_WIDGETS)) {
  ExitError("Incorrect number of widgets requested!");
 }
WidgetList = (Widget **)malloc(numWidgets * sizeof(Widget *));
printf("WidgetList ptr=%p\n", WidgetList);
for(i=0; i<numWidgets; i++) {
  WidgetList[i] = InitializeWidget();
 }
WidgetList[numWidgets] = NULL;
showWidgets(WidgetList);
```
## Example 7: C: DX-88
``` C
int main() {
  ...
    char *result = strstr(destBuf, "Replace Me");
  int idx = result - destBuf;
  strcpy(&destBuf[idx], srcBuf);
  ...
    }
```
# 918
# 434
## Example 1: Java: DX-22
``` Java
public class FileUploadServlet extends HttpServlet {
    ...

        protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("text/html");
        PrintWriter out = response.getWriter();
        String contentType = request.getContentType();

        // the starting position of the boundary header
        int ind = contentType.indexOf("boundary=");
        String boundary = contentType.substring(ind+9);

        String pLine = new String();
        String uploadLocation = new String(UPLOAD_DIRECTORY_STRING); //Constant value

        // verify that content type is multipart form data
        if (contentType != null && contentType.indexOf("multipart/form-data") != -1) {
            // extract the filename from the Http header
            BufferedReader br = new BufferedReader(new InputStreamReader(request.getInputStream()));
            ...
                pLine = br.readLine();
            String filename = pLine.substring(pLine.lastIndexOf("\\"), pLine.lastIndexOf("\""));
            ...

                // output the file to the local upload directory
                try {
                    BufferedWriter bw = new BufferedWriter(new FileWriter(uploadLocation+filename, true));
                    for (String line; (line=br.readLine())!=null; ) {
                        if (line.indexOf(boundary) == -1) {
                            bw.write(line);
                            bw.newLine();
                            bw.flush();
                        }
                    } //end of for loop
                    bw.close();


                } catch (IOException ex) {...}
            // output successful upload response HTML page
        }
        // output unsuccessful upload response HTML page
        else
            {...}
    }
    ...
}
```
# 416
## Example 1: C: SIFT-DX-6
``` C
#include <stdio.h>
#include <unistd.h>
#define BUFSIZER1 512
#define BUFSIZER2 ((BUFSIZER1/2) - 8)
int main(int argc, char **argv) {
  char *buf1R1;
  char *buf2R1;
  char *buf2R2;
  char *buf3R2;
  buf1R1 = (char *) malloc(BUFSIZER1);
  buf2R1 = (char *) malloc(BUFSIZER1);
  free(buf2R1);
  buf2R2 = (char *) malloc(BUFSIZER2);
  buf3R2 = (char *) malloc(BUFSIZER2);
  strncpy(buf2R1, argv[1], BUFSIZER1-1);
  free(buf1R1);
  free(buf2R2);
  free(buf3R2);
}
```
## Example 2: C: SIFT-DX-7
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
# 798
## Example 2a: C: DX-14
``` C
int VerifyAdmin(char *password) {
  if (strcmp(password, "Mew!")) {
    printf("Incorrect Password!\n");
    return(0)
      }
  printf("Entering Diagnostic Mode...\n");
  return(1);
}
```
## Example 2b: Java: SIFT-DX-16
``` Java
int VerifyAdmin(String password) {
    if (!password.equals("Mew!")) {
        return(0)
            }
    //Diagnostic Mode
    return(1);
}
```
## Example 3a: C: DX-92
``` C
int VerifyAdmin(char *password) {
  if (strcmp(password,"68af404b513073584c4b6f22b6c63e6b")) {

    printf("Incorrect Password!\n");
    return(0);
  }
  printf("Entering Diagnostic Mode...\n");
  return(1);
}
```
## Example 3b: Java: SIFT-DX-17
``` Java
public boolean VerifyAdmin(String password) {
    if (password.equals("68af404b513073584c4b6f22b6c63e6b")) {
        System.out.println("Entering Diagnostic Mode...");
        return true;
    }
    System.out.println("Incorrect Password!");
    return false;
```

# Generating these items used the following template:
``` elisp
(tempo-define-template "cwe-example"
                       '(
                         "## Example "
                         '(p "Example number> ")
                         ": "
                         '(p "Programming Language> " lang)
                         'n>
                         "``` " (s lang)
                         'n>
                         "```"
                         'n))
```
