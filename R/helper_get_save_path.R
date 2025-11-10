# Generate a Valid Save File Path
#
# Constructs an absolute file path using user input with sensible defaults and robust path validation.
# It normalizes paths by replacing backslashes, handles directory inputs, verifies existence of specified directories,
# and ensures the resulting file path includes a file extension.
#
# @param save_as Optional character string specifying the desired save path or directory.
#   If a directory is specified (ends with a slash or exists as a directory), the returned path includes the default file name.
#   If a full file path is provided, it validates the directory part.
#   If a file name only is provided, the file is placed in the \code{default_dir}.
#   If \code{NULL}, returns path combining \code{default_dir} and \code{default_name}.
# @param default_name Character string specifying the default file name to use when \code{save_as} is \code{NULL} or a directory.
#   Defaults to \code{"output"}. If no extension is present, \code{file.ext} will be appended.
# @param default_dir Character string specifying the default directory to save files in when \code{save_as} is \code{NULL} or only a file name.
#   Defaults to \code{tempdir()}.
# @param file.ext Character string specifying the file extension to append if the filename does not already have one.
#   Defaults to \code{".pdf"}. Should include the leading dot (e.g., \code{".csv"}, \code{".txt"}).
#   If the filename already contains an extension, this parameter is ignored.
#
# @return A character string of a normalized save path with forward slashes and guaranteed file extension.
#
# @details
# - If \code{save_as} is \code{NULL}, the function returns the default directory combined with the default file name.
# - If \code{save_as} is a directory path (either ends with slash or exists), it returns the directory path combined with the default file name.
# - If \code{save_as} is a full file path, it verifies that the directory exists and returns a normalized full path.
# - If a file name only is given, the file will be placed in the \code{default_dir}.
# - The function checks if the final filename has an extension (pattern: one or more characters after a dot).
#   If no extension is detected, \code{file.ext} is automatically appended.
# - Extension checking works on the final filename component, not on directory names containing dots.
#
# throws Error if the specified directory in \code{save_as} does not exist.
#
# @examples
# # Default path with .pdf extension
# get_save_path()
#
# # Directory only - uses default name with extension
# get_save_path("mydata/")
#
# # Full path with extension specified - keeps original extension
# get_save_path("mydata/output.csv")
#
# # Filename without extension - adds default .pdf
# get_save_path("my_file")
#
# # Custom extension
# get_save_path("report", file.ext = ".pdf")
#
# # Filename with extension in custom directory
# get_save_path("my_file.txt", default_dir = "~/Documents")
#
get_save_path <- function(save_as = NULL,
                          default_name = "output",
                          default_dir = tempdir(),
                          file.ext = ".pdf") {

  # remove \\ and use / instead
  default_dir <- gsub("\\\\", "/", default_dir)

  # Function to ensure file extension
  ensure_extension <- function(filename, ext) {
    if (grepl("\\.[^\\.]+$", filename)) {
      # Has some extension, return as is
      filename
    } else {
      # Append given extension
      paste0(filename, ext)
    }
  }

  # 1. Default Case: If no input, use default path and name.
  if (is.null(save_as)) {
    file_with_ext <- ensure_extension(default_name, file.ext)
    return(file.path(default_dir, file_with_ext))
  }

  # Normalize input by removing trailing slashes for cleaner path resolution later
  save_as_normalized <- sub("/+$|\\\\+$", "", save_as)

  # Determine if the user intended a directory:
  # A. Input ends with a slash (e.g., "mydata/").
  # B. Input is the path to an existing directory (e.g., "mydata" where 'mydata' exists).
  is_directory_only_intent <- grepl("/$|\\\\$", save_as) || dir.exists(save_as_normalized)

  # --- Case Handling ---

  # 2. Case: Directory Only Intent (e.g., "mydata/" or "C:/reports")
  # Output: 'mydata/default_name'
  if (is_directory_only_intent) {

    # Check if the directory exists.
    if (!dir.exists(save_as_normalized)) {
      stop(paste0("Error: The specified directory does not exist: '", save_as_normalized, "'"))
    }

    # If it exists, normalize the path and apply the default name with extension.
    base_dir <- normalizePath(save_as_normalized, winslash = "/", mustWork = FALSE)
    file_with_ext <- ensure_extension(default_name, file.ext)
    return(file.path(base_dir, file_with_ext))
  }

  # 3. Case: Full Path or Filename

  # Extract the directory component
  dir_part <- dirname(save_as)

  # Check if the input is a full path (i.e., not just a filename, where dir_part == ".")
  if (dir_part != ".") {

    # A: Full Path Provided (e.g., "nonexistent/output.csv")

    # Check if the directory part of the path exists.
    if (!dir.exists(dir_part)) {

      # Stop execution with a clear error if the directory is missing.
      stop(paste0("Error: The directory part of the path does not exist: '", dir_part, "'"))
    }

    # Ensure the filename has the extension
    filename <- basename(save_as)
    file_with_ext <- ensure_extension(filename, file.ext)
    full_path <- file.path(dir_part, file_with_ext)

    # Return normalized full path
    return(normalizePath(full_path, winslash = "/", mustWork = FALSE))

  } else {
    # B: Only a Filename provided (e.g., "my_file" or "my_file.txt").
    file_with_ext <- ensure_extension(save_as, file.ext)
    # Output: 'default_dir/filename'
    return(file.path(default_dir, file_with_ext))
  }
}
