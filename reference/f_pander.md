# Fancy Pander Table Output

Is a wrapper around the `pander` function from the 'pander' package,
designed to produce a fancy table output with specific formatting
options.

## Usage

``` r
f_pander(
  table,
  col_width = 10,
  table_width = NULL,
  limit_columns = NULL,
  style = "multiline",
  console = TRUE,
  ...
)
```

## Arguments

- table:

  A data frame, matrix, or other table-like structure to be rendered.

- col_width:

  Integer. Specifies the maximum number of characters allowed in table
  header columns before a line break is inserted. Defaults to `10`. Note
  that latex will not break the table header if the col with is longer
  due to long names below the header.

- table_width:

  Integer or `NULL`. Defines the number of characters after which the
  table is split into separate sections. Defaults to `NULL`, meaning no
  break is applied.

- limit_columns:

  Integer or `NULL`. Defines the number of columns shown in a table.

- style:

  Character. Pander table style. Defaults to `"multiline"`.

- console:

  Logical. Whether to process headers for console output. Defaults to
  `TRUE`.

- ...:

  Additional arguments passed to the `pander` function.

## Value

None. The function is called for its side effects of setting 'pander'
options and creates a pander formatted table in R Markdown.

## Details

This function sets several `pander` options to ensure that the table
output is formatted in a visually appealing manner. The options set
include:

- `table.alignment.default`: Aligns all columns to the left.

- `table.alignment.rownames`: Aligns row names to the left.

- `keep.trailing.zeros`: Keeps trailing zeros in numeric values.

- `knitr.auto.asis`: Ensures output is not automatically treated as
  'asis'.

- `table.caption.prefix`: Removes the default "Table" prefix in
  captions.

- `keep.line.breaks`: Preserves line breaks in cell content.

- `table.split.table`: Controls table splitting (set to `Inf` if
  `table_width` is `NULL` or `FALSE`).

- `table.split.cells`: Inserts line breaks in headers every `col_width`
  characters.

This function requires
\[Pandoc\](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3
or higher), a universal document converter.

- **Windows:** Install Pandoc and ensure the installation folder  
  (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your
  system PATH.

- **macOS:** If using Homebrew, Pandoc is typically installed in
  "/usr/local/bin". Alternatively, download the .pkg installer and
  verify that the binary's location is in your PATH.

- **Linux:** Install Pandoc through your distribution's package manager
  (commonly installed in "/usr/bin" or "/usr/local/bin") or manually,
  and ensure the directory containing Pandoc is in your PATH.

- If Pandoc is not found, this function may not work as intended.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Example usage of f_pander
df <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Score = c(88.5, 92.3, 85.0)
)

# Render the data frame as a fancy table
f_pander(df)
#> 
#> -----------------------
#> Name      Age   Score  
#> --------- ----- -------
#> Alice     25    88.5   
#> 
#> Bob       30    92.3   
#> 
#> Charlie   35    85.0   
#> -----------------------
#> 
```
