construct_args <- function(x, indent = 4L, max_width = 60L, always_linebreak = NULL, backtick = TRUE) {
  if (backtick) x <- maybe_backtick(x)
  out <- paste(x, collapse = ", ")
  always_linebreak <- always_linebreak %||% (nchar(out) > max_width || grepl("\n", out))
  always_linebreak <- always_linebreak && length(x) > 1
  if (always_linebreak) {
    x   <- gsub("\n", paste0("\n", strrep(" ", indent)), x) 
    out <- paste(x, collapse = paste0(",\n", strrep(" ", indent)))
    out <- paste0("\n", strrep(" ", indent), out, "\n", strrep(" ", indent - 2L))
  }
  out
}

construct_vec <- function(x, indent = 4L, max_width = 60L, backtick = TRUE) {
  if (length(x) == 1L) {
    return(if (backtick) maybe_backtick(x) else x)
  }
  paste0("c(", construct_args(x, indent, max_width, backtick = backtick), ")")
}

maybe_backtick <- function(x) {
  
  ifelse(
    grepl("^[_.a-zA-Z][_.a-zA-Z0-9]*$", x) | grepl("^`.+`$", x), 
    x, paste0("`", x, "`")
  )
  
}
