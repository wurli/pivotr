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
  
  # stopifnot(".value" %in% c(columns, rows))
  
  long_format <- ".value" %in% rows
  
  columns       <- setdiff(columns, ".value")
  rows          <- setdiff(rows, ".value")
  grouping_cols <- union(rows, columns)
  
  filters <- filters %||% list(list(cols = NULL, values = NULL))
  values  <- values  %||% list(list(cols = NULL, funs = NULL))
  
  df_name <- expr_deparse(enexpr(x))
  
  abort_cols_dont_exist(x, map(filters, 1), map(values, 1), grouping_cols)
  
  values        <- make_summary_exprs(values)
  summary_exprs <- values$exprs
  new_col_names <- values$new_col_names
  
  step_summary <- if (length(grouping_cols) == 0L) {
    glue(
      "
      {df_name} |>
        summarise({summary_exprs})
      ",
      summary_exprs = construct_args(summary_exprs, always_linebreak = TRUE)
    )
  } else {
    glue(
      "
      {df_name} |>
        summarise({summary_exprs}) |> 
        arrange({grouping_cols})
      ",
      summary_exprs = construct_args(
        c(
          paste0(".by = ", construct_vec(grouping_cols)),
          summary_exprs
        ),
        always_linebreak = TRUE
      ),
      grouping_cols = construct_args(grouping_cols)
    )
  } 
  
  step_pivot_longer <- if (long_format) {
    glue(
      '
        pivot_longer(
          {new_col_names},
          names_to = ".measure",
          values_to = ".value"
        )
      ',
      new_col_names = construct_vec(new_col_names)
    )
  }
  
  step_pivot_wider <- if (length(columns) > 0) {
    pivot_wider_vals_from <- if (long_format) ".value" else new_col_names
    
    glue(
      "
        pivot_wider(
          names_from = {columns},
          values_from = {pivot_wider_vals_from}
        )
      ",
      columns = construct_vec(columns),
      pivot_wider_vals_from = construct_vec(pivot_wider_vals_from)
    )
  } 
  
  paste(
    c(step_summary, step_pivot_longer, step_pivot_wider), 
    collapse = " |>\n"
  )
  
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
  
  use_function_names <- use_function_names %||% length(unique(unlist(map(spec, 2)))) > 1
  
  new_col_names <- spec |> 
    map_chr(\(x) {
      if (use_function_names) {
        paste(x[[1]], x[[2]], sep = "_")
      } else {
        x[[1]]
      }
    }) |> 
    unique() |> 
    sort()
  
  exprs <- spec |>
    compress_summary_spec() |> 
    map(function(x) {
      
      if (all(lengths(x) == 1L)) {
        f <- x[[2]]
        x <- x[[1]]
        return(glue("{x} = {f}({x})"))
      }
      
      cols <- construct_vec(x[[1]], indent = 4L) 
      funs <- if (!use_function_names) {
        x[[2]]
      } else {
        glue("list({args})", args = construct_args(glue("{x[[2]]} = {x[[2]]}")))
      }
      
      glue(
        "across({args})",
        args = construct_args(c(cols, funs))
      )
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
