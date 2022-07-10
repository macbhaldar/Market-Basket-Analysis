# Market-Basket-Analysis

The Association Rules algorithm is also called Market Basket Analysis as has been applied
mostly to learn about purchasing patterns, even if it can be applied to find
patterns in many other cases. 

## Objective :
The objective of the project is to find items that are frequently bought together to be able:
- Place frequently bought together items nearby to increase sales on-site
- Increase online revenue by offering related items to the user
- Creating promotional campaigns with those items which were
bought together

## Data Cleaning :
The `Groceries.csv` file is a comma-separated csv file with 1499 transactions and a maximum
of 34 items per transaction.
Some data cleaning was performed :
- The white spaces have been trimmed, the empty rows ignored and the NA values
marked as NA when importing to R.
- The date, which was concatenated with the first item, was split and removed as is not
needed (date range: January 2000 to February 2002, 28 months).
- The transactionID was added for each transaction.

The Apriori algorithm needs transactional data which can be shaped in two formats: basket
or single. The basket format contains a transaction per line with the items separated by
commas in one column. The single format contains two columns, the first column is the
transaction ID and the second is an item. In both cases, the duplicates were removed to
ensure the same item is not twice in a transaction. Both formats have been tested, with the
same results. The basket format can also use a binary matrix, but has
not been tested.

## Transactional Data Exploration :
The data frame has been successfully transformed into transactional, the number of
transactions (1,499) and items (38) is exactly the same as before the transformation as seen
in the summary. 38.36% of the cells from the sparse matrix are filled in with products and the rest are empty.

The items within the transactions seem to be evenly distributed and the size of the transactions normally distributed. 50% of the transactions contain between 10 and 19 items, the minimum number of unique items within a transaction are 4 and the
maximum 27.

The most popular items are vegetables which appear in 1,089 transactions (72.64%),
followed by poultry which appears in 613 transactions (40.89%).