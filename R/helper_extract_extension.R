#Helper function to extract the extension provided by the user.
extract_extension <- function(filename) {

  # --- 1. Define Valid Extensions and their File Types (Lookup Table) ---
  ext_map <- c(
    "pdf"  = "pdf",
    "doc"  = "word",
    "docx" = "word",
    "xls"  = "excel",
    "xlsx" = "excel",
    "rmd"  = "rmd",
    "png"  = "png"
  )

  # Create the regex pattern dynamically from the map keys
  # This pattern MUST start with a literal dot: \. (ext1|ext2) $
  valid_ext_list <- paste(names(ext_map), collapse = "|")
  valid_regex <- paste0("\\.(", valid_ext_list, ")$")

  # --- 2. Check for Dot in the LAST 5 Characters ---

  # Check 2a: Get the last 5 characters of the filename
  last_5_chars <- substr(filename, nchar(filename) - 4, nchar(filename))

  # Check 2b: See if a dot exists in those last 5 characters
  dot_in_last_5 <- grepl("\\.", last_5_chars)

  # --- 3. Determine Status and Return Result ---

  # Check 1: Valid Extension Found (The filename ends with one of the valid extensions)
  if (grepl(valid_regex, filename, ignore.case = TRUE)) {

    # Extract the extension *without* the dot for the lookup
    extracted_ext_no_dot <- sub(paste0(".*\\."), "", filename, ignore.case = TRUE)
    ext_key <- tolower(extracted_ext_no_dot)
    file_type <- ext_map[ext_key]

    # Construct the output vector
    result <- c(
      "Extension" = paste0(".", ext_key),
      "FileType" = file_type
    )

    return(result)
  }

  # Check 2: Invalid Extension Found (A dot is present in the last 5 chars, but not a valid one)
  # NOTE: This only captures extensions 1-4 characters long (e.g., .f, .bak, .txt, .html)
  # that are NOT in the valid list. Longer invalid ones will be caught by the next check.
  else if (dot_in_last_5) {
    # Extract the full, invalid extension for the error message
    invalid_ext <- sub(paste0(".*\\."), ".", filename, ignore.case = TRUE)

    # Halt the function with a detailed error message
    stop(paste0("Invalid Extension '", invalid_ext,
                "' provided (or valid extension is > 5 chars). Must be one of: ",
                paste(names(ext_map), collapse = ", ")))
  }

  # Check 3: No Extension Found
  # This covers strings with no dot, or strings where the dot is more than 5 characters from the end.
  else {
    return(FALSE)
  }
}

 # foo <- unname(extract_extension("./hoh/kljkpf.lllll/xls"))
 # bar <- foo[1]
 # bar
