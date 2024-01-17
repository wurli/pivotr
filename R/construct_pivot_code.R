#' Construct the dplyr code used for the PivotTable
#'
#' @param x A dataframe
#' @param x_name The name/code that represents `x`
#' @param filters A list of filter expressions
#' @param columns,rows Column names to use for the rows/columns PivotTable fields
#' @param values A list of values expressions
#' @param code_width Number of characters to display per line
#' @param pipe Either `"base"` or `"magrittr"`
#' @param use_function_names Whether to indicate the summary function in the
#'   output column names
#' @param use_across Whether to use `dplyr::across()`
#'
#' @return A string
#' @noRd
construct_pivot_code <- function(x, x_name = NULL, 
                                 filters = NULL, 
                                 columns = ".measure", 
                                 rows = NULL, 
                                 values = NULL,
                                 code_width = 40,
                                 pipe = c("base", "magrittr"),
                                 use_function_names = c("sometimes", "never", "always"),
                                 use_across = c("sometimes", "never", "always")) {
  
  use_across         <- match.arg(use_across)
  use_function_names <- match.arg(use_function_names)
  pipe               <- match.arg(pipe)
  pipe               <- switch(pipe, base = "|>", magrittr = "%>%")
  
  width <- function(code) code_width - nchar(glue(code, envir = parent.frame()))
  
  long_format <- ".measure" %in% rows
  
  columns       <- setdiff(columns, ".measure")
  grouping_cols <- setdiff(union(rows, columns), ".measure")
  
  filters <- filters %||% list(list(cols = NULL, values = NULL))
  values  <- values  %||% list()
  
  use_dummy_col <- length(values) == 0L && length(columns) > 0L
  
  df_name <- x_name %||% expr_deparse(enexpr(x))
  
  abort_cols_dont_exist(x, x_name, map(filters, 1), map(values, 1), grouping_cols)
  
  values        <- make_summary_exprs(values, code_width, use_function_names, use_across)
  summary_exprs <- values$exprs
  new_col_names <- if (use_dummy_col) ".dummy" else values$new_col_names
  
  step_start <- glue("{df_name}")
  
  step_as_tibble <- if (!all(class(x) %in% c("tbl_df", "tbl", "data.frame"))) "  as_tibble()"
  
  step_summary <- if (length(grouping_cols) == 0L) {
    glue(
      "
        summarise({summary_exprs})
      ",
      summary_exprs = construct_args(
        summary_exprs, 
        always_linebreak = TRUE, 
        backtick = FALSE,
        max_width = width("  summarise() {pipe}")
      )
    )
  } else {
    grouping_expr <- construct_vec(grouping_cols, max_width = width("  .by = c(),"), indent = 2L)
    glue(
      "
        summarise({summary_exprs})
      ",
      summary_exprs = construct_args(
        c(paste0(".by = ", grouping_expr), summary_exprs),
        always_linebreak = TRUE,
        backtick = FALSE
      )
    )
  } 
  
  step_dummy_col <- if (use_dummy_col) "  mutate(.dummy = NA)"
  
  step_pivot_longer <- if (long_format) {
    glue(
      '
        pivot_longer(
          {new_col_names},
          names_to = ".measure",
          values_to = ".value"
        )
      ',
      new_col_names = construct_vec(
        new_col_names, 
        max_width = width("    ,"), 
        indent = 6L
      )
    )
  }
  
  step_relocate <- if (length(rows) > 0 && rows[length(rows)] != ".measure") {
    if (rows[1] == ".measure") {
      "  relocate(.measure)"
    } else {
      glue(
        "  relocate(.measure, .after = {prev})", 
        prev = rows[which(rows == ".measure") - 1L],
        .trim = FALSE
      )
    }
  }
  
  step_arrange <- if (length(setdiff(rows, ".measure")) > 0L) {
    glue(
      "
        arrange({rows})
      ",
      rows = construct_args(rows, max_width = width("  arrange() {pipe}"))
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
      columns = construct_vec(
        columns, max_width = width("    names_from = ,")
      ),
      pivot_wider_vals_from = construct_vec(
        pivot_wider_vals_from, max_width = width("    values_from = ,")
      )
    )
  } 
  
  paste(
    c(step_start, 
      step_as_tibble,
      step_summary,
      step_pivot_longer, 
      step_relocate,
      step_arrange, 
      step_dummy_col,
      step_pivot_wider), 
    collapse = paste0(" ", pipe, "\n")
  )
  
}

abort_cols_dont_exist <- function(df, df_name, ...) {
  all_cols <- unique(unlist(c(...)))
  bad_cols <- setdiff(all_cols, c(colnames(df), ".measure"))
  df_name  <- gsub(".+\n", "", df_name)
  
  if (length(bad_cols) > 0) {
    cli_abort(
      c(
        "Columns specified don't exist",
        i = "Table is {.val {df_name}}",
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
make_summary_exprs <- function(spec, code_width = 60L, use_function_names = NULL, use_across = "sometimes") {
  
  width <- function(code) code_width - nchar(code)
  
  use_function_names <- switch(use_function_names,
    always = TRUE,
    never = FALSE,
    sometimes = length(unique(unlist(map(spec, 2)))) > 1
  )
  
  use_across <- switch(use_across,
    always = TRUE,
    never = FALSE,
    sometimes = NULL
  )
  
  new_col_names <- spec |> 
    map_chr(\(x) {
      if (use_function_names) {
        paste(x[[1]], x[[2]], sep = "_")
      } else {
        x[[1]]
      }
    }) |> 
    unique() 
  
  exprs <- spec |>
    compress_summary_spec() |> 
    map(function(x) {
      
      fun <- x[[2]]
      col <- x[[1]]
      
      use_across <- use_across %||% any(lengths(x) > 1L)
      
      if (!use_across) {
        out <- if (use_function_names) {
          name <- maybe_backtick(paste0(col, "_", fun))
          col <- maybe_backtick(col)
          glue("{name} = {fun}({col})")
        } else {
          col <- maybe_backtick(col)
          glue("{col} = {fun}({col})")
        }
        return(out)
      }
      
      cols_exprs <- construct_vec(col, indent = 2L, max_width = width("      ,")) 
      funs_exprs <- if (!use_function_names) {
        fun
      } else {
        glue("list({args})", args = construct_args(
          glue("{fun} = {fun}"), 
          backtick = FALSE,
          max_width = width("    ,")
        ))
      }
      
      glue(
        "across({args})",
        args = construct_args(
          c(cols_exprs, funs_exprs), 
          backtick = FALSE,
          max_width = width("    across()"),
          indent = 2L
        )
      )
    }) |> 
    unlist(use.names = FALSE)
  
  list(exprs = exprs, new_col_names = new_col_names)
  
}

# @examples
# spec <- list(
#   list("x1", "f2"),
#   list("x2", "f2"),
#   list("x1", "f1"),
#   list("x2", "f3"),
#   list("x2", "f1"),
#   list("x3", "f3")
# )
# compress_summary_spec(spec)
compress_summary_spec <- function(spec) {
  spec_by_fun <- split(spec, map_chr(spec, 2)) |> 
    map(~ list(map_chr(., 1), .[[1]][[2]])) |> 
    unname() |> 
    c() 
  
  spec_by_col <- split(spec_by_fun, map_chr(spec_by_fun, ~ paste(.[[1]], collapse = "."))) |> 
    map(~ list(.[[1]][[1]], sort(map_chr(., 2)))) |> 
    unname() |> 
    c()
  
  spec_by_col
}
