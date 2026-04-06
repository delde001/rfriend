# Remove Outliers from Data

\`f_remove_outliers()\` removes specific rows from a dataframe based on
a list of identifiers. It is designed to work seamlessly with the output
of
[`f_outliers`](https://delde001.github.io/rfriend/reference/f_outliers.md),
but can also accept a custom vector of IDs.

## Usage

``` r
f_remove_outliers(data, outliers, by = "row_id", verbose = TRUE)
```

## Arguments

- data:

  A data.frame, tibble, or data.table containing the original data.

- outliers:

  Either:

  - A dataframe returned by
    [`f_outliers`](https://delde001.github.io/rfriend/reference/f_outliers.md).

  - A vector of IDs/row numbers to remove.

- by:

  A character string specifying the column to match on. Default is
  `"row_id"`. If the source `data` does not have a `row_id` column, the
  function effectively uses the row numbers (1, 2, 3...) to ensure safe
  deletion.

- verbose:

  Logical. If `TRUE` (default), prints a summary of how many rows were
  removed.

## Value

An object of the same class as the input `data` (data.frame, tibble, or
data.table) with the specified outlier rows removed.

## Details

**Safe Deletion Logic:** This function performs a "anti-join" style
filtering. It keeps rows where the identifier in `by` is **not** found
in the `outliers` list.

**Handling Row IDs:** If you use the default `by = "row_id"` and your
original `data` does not have a column named `"row_id"`, the function
assumes you are referring to the intrinsic row numbers of the
data.frame, tibble, or data.table. It will temporarily generate IDs to
perform the deletion and then return the clean data with the original
structure (without adding a permanent `row_id` column to the result).

## See also

[`f_outliers`](https://delde001.github.io/rfriend/reference/f_outliers.md)
to identify the rows to be removed.

## Examples

``` r
# --- Setup: Create Dummy Data ---
set.seed(42)
df <- data.frame(
  Team       = rep(c("A", "B"), each = 20),
  Department = rep(c("Sales", "IT"), each = 10, times = 2),
  Salary     = c(rnorm(19, 50000, 500), 100000,
                 rnorm(18, 50000, 500), 57000, 1000),
  Age        = c(rnorm(38, 35, 2), 90, 35),
  EmployeeID = paste0("E", sprintf("%03d", 1:40)),
  stringsAsFactors = FALSE
)
# row 20:  extreme high Salary (Team A)
# row 39:  mild Salary outlier at coef = 1.5 only
# row 40:  extreme low  Salary (Team B)
# row 39:  extreme high Age

# --- Example 1: Basic two-step workflow (data.frame notation) ---
# The most common use case: find then remove in two lines.
bad_rows <- f_outliers(df, columns = "Salary")
clean_df <- f_remove_outliers(df, bad_rows)
#> rfriend: Removed 3 outliers.
#> rfriend: Rows: 40 -> 37
nrow(df)       # 40
#> [1] 40
nrow(clean_df) # 40 minus flagged rows
#> [1] 37

# --- Example 2: Basic two-step workflow (formula notation) ---
# Identical result to Example 1 using the formula interface.
bad_rows <- f_outliers(Salary ~ 1, data = df)
clean_df <- f_remove_outliers(df, bad_rows)
#> rfriend: Removed 3 outliers.
#> rfriend: Rows: 40 -> 37
nrow(clean_df)
#> [1] 37

# --- Example 3: Grouped detection then removal (both notations) ---
# Outliers are identified *within* each Team separately before removal.

# data.frame notation:
bad_rows <- f_outliers(df, columns = "Salary", group_vars = "Team")
clean_df <- f_remove_outliers(df, bad_rows)
#> rfriend: Removed 3 outliers.
#> rfriend: Rows: 40 -> 37

# Formula notation (identical result):
bad_rows <- f_outliers(Salary ~ Team, data = df)
clean_df <- f_remove_outliers(df, bad_rows)
#> rfriend: Removed 3 outliers.
#> rfriend: Rows: 40 -> 37
nrow(clean_df)
#> [1] 37

# --- Example 4: Selective removal — only act on a subset of outliers ---
# Find all flagged rows, but only remove the extreme high salaries.
# Step 1: Identify all Salary outliers grouped by Team
bad_rows    <- f_outliers(Salary ~ Team, data = df)
all_flagged <- bad_rows$output_df

# Step 2: Filter to keep only the rows where Salary > 90000
really_bad  <- all_flagged[all_flagged$Salary > 90000, ]

# Step 3: Remove only those rows — low outlier (row 40) is preserved
clean_df <- f_remove_outliers(df, really_bad)
#> rfriend: Removed 1 outliers.
#> rfriend: Rows: 40 -> 39
range(clean_df$Salary)  # low outlier still present, high one is gone
#> [1]  1000 57000

# --- Example 5: Multi-column outlier removal ---
# f_outliers scans both Salary and Age; f_remove_outliers removes
# every row flagged by either column in one call.

# Formula notation:
bad_rows <- f_outliers(Salary + Age ~ Team, data = df)
clean_df <- f_remove_outliers(df, bad_rows)
#> rfriend: Removed 4 outliers.
#> rfriend: Rows: 40 -> 36

# data.frame notation (identical result):
bad_rows <- f_outliers(df, columns = c("Salary", "Age"), group_vars = "Team")
clean_df <- f_remove_outliers(df, bad_rows)
#> rfriend: Removed 4 outliers.
#> rfriend: Rows: 40 -> 36
nrow(clean_df)  # rows flagged by Salary OR Age are removed
#> [1] 36

# --- Example 6: Strict detection + custom ID column ---
# coef = 3.0 flags only extreme outliers. EmployeeID is used
# as the matching key instead of the default row_id.

# Formula notation:
bad_rows <- f_outliers(Salary ~ Team, data = df,
                       id_var = "EmployeeID", coef = 3.0)

# data.frame notation (identical result):
bad_rows <- f_outliers(df, columns = "Salary", group_vars = "Team",
                       id_var = "EmployeeID", coef = 3.0)

# Remove by EmployeeID rather than row position
clean_df <- f_remove_outliers(df, bad_rows$output_df, by = "EmployeeID")
#> rfriend: Removed 3 outliers.
#> rfriend: Rows: 40 -> 37

# Confirm the flagged employees are no longer in the clean data
bad_ids  <- bad_rows$output_df$EmployeeID
any(clean_df$EmployeeID %in% bad_ids)  # FALSE
#> [1] FALSE
```
