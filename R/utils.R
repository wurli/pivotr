random_id <- function(length = 16L) {
  chars <- sample(c(letters, LETTERS, 0:9), size = length, replace = TRUE)
  paste(chars, collapse = "")
}

strip_id <- function(x) sub(".+__", "", x)
