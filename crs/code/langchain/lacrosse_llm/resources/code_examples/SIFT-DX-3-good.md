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
