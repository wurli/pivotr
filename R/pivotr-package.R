# The R CMD check linter doesn't pick up that these are assigned by zeallot
utils::globalVariables(c(
  "columns", 
  "dataset", 
  "dataset_code", 
  "dataset_pkg",
  "dataset_name", 
  "rows", 
  "values"
))


#' @import rlang cli shiny dplyr tidyr bslib
#' @importFrom purrr map map_chr map_lgl map_int map_dbl imap
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @importFrom utils data
#' @importFrom zeallot "%<-%"

NULL
