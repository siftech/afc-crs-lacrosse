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
