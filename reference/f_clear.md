# f_clear: Clear Various Aspects of the R Environment

Provides a convenient way to clear different components of the R
environment, including the console, memory, graphics, and more. It also
offers the option to restart the R session. This can come in handy at
the start of an R script.

## Usage

``` r
f_clear(env = TRUE, gc = TRUE, console = TRUE, graph = TRUE, restart = FALSE)
```

## Arguments

- env:

  Logical. If `TRUE`, all objects in the global environment are removed.
  Default is `TRUE`.

- gc:

  Logical. If `TRUE`, garbage collection is performed to free up memory.
  Default is `TRUE`.

- console:

  Logical. If `TRUE`, the R console is cleared. Default is `TRUE`.

- graph:

  Logical. If `TRUE`, all open graphics devices are closed. Default is
  `TRUE`.

- restart:

  Logical. If `TRUE`, the R session is restarted using 'RStudio's' API.
  Default is `FALSE`.

## Value

No return value, called for side effects, see details.

## Details

- Console Clearing: Clears the console output.

- Garbage Collection: Performs garbage collection to free memory from
  unreferenced objects.

- Graph Clearing: Closes all open graphics devices.

- Environment Clearing: Removes all objects from the global environment.

- Session Restart: Restarts the R session (only available in 'RStudio').

## Note

The `restart` parameter requires 'RStudio' and its API package
('rstudioapi') to be installed and available.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Clear console, memory, graphs, and for example NOT the environment.
f_clear(env = FALSE)
#> Cleaned up console...
#> Freed up memory...
#> Closed all graphs...
#> To also restart R use: f_clear(restart = TRUE)

```
