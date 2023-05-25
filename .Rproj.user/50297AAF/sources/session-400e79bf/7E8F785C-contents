#' Title
#'
#' @param x 
#' @param filters 
#' @param columns,rows Character vector of column names, one of which must 
#'   include the special `".value"`
#' @param values 
#'
#' @return
#' @export
#'
#' @examples
get_code <- function(x, filters = NULL, columns = ".value", rows = NULL, values = NULL) {
  
  stopifnot(".value" %in% c(columns, rows))
  
  long_format <- ".value" %in% rows
  
  grouping_cols_chr <- setdiff(c(rows, columns), ".value")
  grouping_cols     <- syms(grouping_cols_chr)
  columns           <- syms(setdiff(columns, ".value"))
  rows              <- syms(setdiff(rows, ".value"))
  
  filters <- filters %||% list(list(cols = NULL, values = NULL))
  values  <- values  %||% list(list(cols = NULL, funs = NULL))
  
  df_name <- ensym(x)
  
  abort_cols_dont_exist(x, map(filters, 1), map(values, 1), grouping_cols_chr)
  
  values <- make_summary_exprs(values)
  summary_exprs <- values$exprs
  new_col_names <- values$new_col_names
  
  step_summary <- if (length(grouping_cols) == 0L) {
    expr(!!df_name %>% summarise(!!!summary_exprs))
  } else {
    expr(
      !!df_name %>% 
        relocate(!!!grouping_cols) %>%
        arrange(!!!grouping_cols) %>%
        summarise(
          .by = c(!!!grouping_cols),
          !!!summary_exprs
        )
    )
  } 
  
  step_pivot_longer <- if (long_format) {
    expr(
      pivot_longer(
        c(!!!syms(new_col_names)),
        names_to = ".measure",
        values_to = ".value"
      )
    )
  }
  
  step_pivot_wider <- if (length(columns) > 0) {
    pivot_wider_vals_from <- if (long_format) list(expr(.value)) else syms(new_col_names)
    
    expr(
      pivot_wider(
        names_from = c(!!!columns),
        values_from = c(!!!pivot_wider_vals_from)
      )
    )
  } 
  
  expr <- expr_concat(!!step_summary, !!step_pivot_longer, !!step_pivot_wider)
  
  suppressWarnings(styler::style_text(expr_deparse(expr)))
  
}

abort_cols_dont_exist <- function(df, ...) {
  all_cols <- unique(unlist(c(...)))
  bad_cols <- setdiff(all_cols, c(colnames(df), ".value"))
  
  if (length(bad_cols) > 0) {
    cli_abort(
      c(
        "Columns specified don't exist",
        i = "Check {.field {bad_cols}}"
      ),
      call = caller_call()
    )
  }
  
}




# Naming rules for summaries:
# 1. If only one function, use x = f(x) naming scheme
# 2. If multiple functions, use x_f = f(x) naming schema

# Syntax rules for summaries:
# 1. If 1:1 column/function relationship, use `=`
# 2. If 1:many column/function relationship, use `across()`

# spec <- list(
#   list("x1", "f2"),
#   list("x2", "f2"),
#   list("x1", "f1"),
#   list("x2", "f3"),
#   list("x2", "f1"),
#   list("x3", "f3"),
#   list("x4", "f4")
# )
make_summary_exprs <- function(spec, use_function_names = NULL) {
  
  stopifnot(is.null(use_function_names) || is.logical(use_function_names))
  
  spec <- spec |>
    compress_summary_spec()
  
  use_function_names <- use_function_names %||% length(unique(unlist(map(spec, 2)))) > 1
  
  new_col_names <- spec |> 
    map(\(x) {
      if (use_function_names) return(x[[1]])
      do.call(paste, c(sep = "_", expand.grid(x[[1]], x[[2]])))
    }) |> 
    unlist(use.names = FALSE) |> 
    unique() |> 
    sort()
  
  expr_names <- map_chr(spec, \(x) {
    if (!all(lengths(x) == 1L)) {
      return("")
    } 
    if (use_function_names) {
      paste(x, collapse = "_")
    } else {
      x[[1]]
    }
  })
  
  exprs <- spec |>
    set_names(expr_names) |>
    map(function(x) {
      
      if (all(lengths(x) == 1L)) {
        f <- sym(x[[2]])
        x <- sym(x[[1]])
        return(expr((!!f)(!!x)))
      }
      
      cols <- if (length(x[[1]]) == 1L) {
        sym(x[[1]])
      } else {
        expr(c(!!!syms(x[[1]]))) 
      }
      
      funs <- if (length(x[[2]]) == 1L && !use_function_names) {
        sym(x[[2]])
      } else {
        names(x[[2]]) <- x[[2]]
        expr(list(!!!syms(x[[2]])))
      }
      
      expr(across(!!cols, !!funs))
      
    })
  
  list(exprs = exprs, new_col_names = new_col_names)
  
}

#' @examples
#' spec <- list(
#'   list("x1", "f2"),
#'   list("x2", "f2"),
#'   list("x1", "f1"),
#'   list("x2", "f3"),
#'   list("x2", "f1"),
#'   list("x3", "f3")
#' )
#' compress_summary_spec(spec)
compress_summary_spec <- function(spec) {
  spec_by_fun <- split(spec, map_chr(spec, 2)) |> 
    map(~ list(sort(map_chr(., 1)), .[[1]][[2]])) |> 
    unname() |> 
    c() 
  
  spec_by_col <- split(spec_by_fun, map_chr(spec_by_fun, ~ paste(.[[1]], collapse = "."))) |> 
    map(~ list(.[[1]][[1]], sort(map_chr(., 2)))) |> 
    unname() |> 
    c()
  
  spec_by_col
}
