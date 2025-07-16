# Insert newline every X characters
insert_newline <- function(string, every_x_chars) {
  gsub(paste0("(.{", every_x_chars, "})"), "\\1\n", string)
}
