# Open a File with the Default Application

Opens a specified file using the default application associated with its
file type. It automatically detects the operating system (Windows,
Linux, or macOS) and uses the appropriate command to open the file.

## Usage

``` r
f_open_file(filepath)
```

## Arguments

- filepath:

  A character string specifying the path to the file to be opened. The
  path can be absolute or relative.

## Value

Does not return a value; it is called for its side effect of opening a
file.

## Details

\- On **Windows**, the `f_open_file()` function uses `shell.exec()` to
open the file. - On **Linux**, it uses `xdg-open` via the
[`system()`](https://rdrr.io/r/base/system.html) function. - On
**macOS**, it uses `open` via the
[`system()`](https://rdrr.io/r/base/system.html) function.

If an unsupported operating system is detected, the function will throw
a message.

## See also

\[shell.exec()\], \[system()\]

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# NOTE: The use of "if(interactive())" prevents this example from running
# during automated CRAN checks. This is necessary because the example
# opens a file, a behavior restricted by CRAN policies for automated
# testing.You don't need to use "if(interactive())" in your own scripts.
if(interactive()) {
# Open a PDF file.
f_open_file("example.pdf")

# Open an image file.
f_open_file("image.png")

# Open a text file.
f_open_file("document.txt")
}
```
