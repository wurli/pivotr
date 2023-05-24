expr_concat <- function(...) {
  exprs(...) |> 
    lapply(expr_deparse) |> 
    lapply(paste, collapse = " ") |> 
    Filter(x = _, \(x) x != "NULL") |> 
    paste(collapse = " %>%\n") |> 
    parse_expr()
}
