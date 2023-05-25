construct_args <- function(x, indent = 4L, max_width = 60L, always_linebreak = NULL) {
  out <- paste(x, collapse = ", ")
  always_linebreak <- always_linebreak %||% nchar(out) > max_width || grepl("\n", out)
  if (always_linebreak) {
    x   <- gsub("\n", paste0("\n", strrep(" ", indent)), x) 
    out <- paste(x, collapse = paste0(",\n", strrep(" ", indent)))
    out <- paste0("\n", strrep(" ", indent), out, "\n", strrep(" ", indent - 2L))
  }
  out
}

construct_vec <- function(x, indent = 4L, max_width = 60L) {
  if (length(x) == 1L) {
    return(x)
  }
  paste0("c(", construct_args(x, indent, max_width), ")")
}
