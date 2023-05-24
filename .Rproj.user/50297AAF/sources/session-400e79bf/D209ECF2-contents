#' Insert expressions into a string using rlang injection syntax
#'
#' @param ...,.open,.close,.envir Passed to [glue::glue()]
#'
#' @return A character vector
#' @export
#'
#' @examples
#' a <- expr(1 + 1)
#' glue_inject("c(<< !!a >>)")
#' glue_inject("<< c(!!!letters[1:3]) >>")
glue_inject <- function(..., .open = "<<", .close = ">>", .envir = parent.frame()) {
  
  glue::glue(
    ...,
    .open = .open, .close = .close, .envir = .envir, 
    .transformer = inject_transformer
  )
}

inject_transformer <- function(text, envir) {
  inject <- expr_deparse(inject(expr(!!parse_expr(text)), envir))
  gsub("\\s+", " ", paste(inject, collapse = " "))
}
