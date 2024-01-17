pivotTableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(7, style = "padding:5px;",
      card(
        full_screen = TRUE,
        reactable::reactableOutput(ns("data"))
      )
    ),
    column(5, style = "padding:5px;",
      h4("PivotTable Fields"),
      fluidRow(column(12,
        uiOutput(ns("fields_bucket"))
      )),
      uiOutput(ns("pivot_table_fields_ui"))
    )
  )
}

#' Generate the PivotTable and fields
#'
#' @param id The `input` slot that will be used to access the value
#' @param dataset A reactive holding the unprocessed data frame
#' @param dataset_name A reactive giving the name of the `dataset()`, e.g.
#'   'diamonds' for `ggplot2::diamonds`
#' @param dataset_pkg A reactive giving the name of the package which provides
#'   `dataset()`
#' @param code A reactive giving the code used to perform pivoting operations on
#'   `dataset()`
#' @param pkg_data_env The environment in which to evaluate `code()`
#'
#' @return A vector of reactives giving the selected rows, columns, and values
#' @noRd
#' 
pivotTableServer <- function(id, dataset, dataset_name, dataset_pkg, code, pkg_data_env) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # -- Re-render pivot table opts whenever a new dataset is selected ---------
    bindEvent(dataset(), x = observe({
      output$pivot_table_fields_ui <- renderUI({
        opts_panel <- function(name, id, ...) {
          sortable::bucket_list(NULL,
            group_name = "opts", # Means items can be dragged between buckets
            sortable::add_rank_list(name, input_id = id, ...),
            class = "default-sortable pivot-table-options-list"
          )
        }
        
        list(
          fluidRow(
            column(6, style = "padding-right:0px;", opts_panel("Filters", ns("filters"))),
            column(6, style = "padding-left:0px;",
              opts_panel("Columns", ns("columns"), list(.measure = "\U03A3 Value"))
            )
          ),
          fluidRow(
            column(6, style = "padding-right:0px;", opts_panel("Rows", ns("rows"))),
            column(6, style = "padding-left:0px;", with_corner_buttons(
              corner_button("values_settings", "cog", style = "right:-20px;"),
              opts_panel("Values", ns("values"))
            ))
          )
        )
      })
    }))
   
    # -- 'Pivot Table Fields' bucket ------------------------------------------- 
    bindEvent(input$fields, ignoreNULL = FALSE, x = observe({
      # Whenever a field is dragged to a new panel, re-render so the field
      # gets re-added to the 'Pivot Table Fields' bucket
      if (length(input$fields) != ncol(dataset())) {
        output$fields_bucket <- renderUI({
          sortable::bucket_list(NULL, 
            group_name = "opts",
            sortable::add_rank_list(
              input_id = ns("fields"),
              text = NULL,
              labels = dataset() |> 
                imap(function(x, y) {
                  ptype <- paste0("&lt", vctrs::vec_ptype_abbr(x), "&gt")
                  HTML(glue::glue('<span style="color:grey;">{ptype}</span>{y}'))
                }) |>
                # Long story short, it's hard to track where different cols get
                # dragged to without using unique identifiers for each item
                set_names(~ paste0(random_id(), "__", .)),
            ),
            class = "default-sortable fields-list"
          )
        })
      }
    }))
    
    values <- bindEvent(input$values, input$update_functions, x = reactive({
      input$values |>
        map(function(id) {
          colname <- strip_id(id)
          col <- dataset()[[colname]]
          fun <- funs()[[id]] %||% (if (is.numeric(col)) "sum" else "length")
          list(cols = colname, funs = fun)
        })
    }))
    
    # Summary function used for each column
    funs <- reactiveVal(list())
    
    # Update summary function used for each column when the user
    # specifies by closing the modal
    bindEvent(input$update_functions, x = observe({
      removeModal()
      
      new_funs <- input$values |> 
        set_names() |> 
        map(~ input[[.]]) 
      
      funs(new_funs)
    }))
    
    
    output$data <- reactable::renderReactable({
      
      # Load the data into an app-specific environment to avoid polluting the global envir
      data(list = dataset_name(), package = dataset_pkg(), envir = pkg_data_env)
      
      df <- eval(parse(text = code()), envir = pkg_data_env)
      
      if (ncol(df) == 0L) {
        df <- tibble(`-` = integer())
      }
      
      reactable::reactable(
        df,
        bordered = TRUE,
        striped = TRUE,
        outlined = TRUE,
        compact = TRUE,
        pagination = FALSE,
        sortable = FALSE,
        height = 680,
        resizable = TRUE
      )
      
    })
    
    bindEvent(input$values_settings, x = observe({
      summary_fns <- list( 
        continuous = c("sum", "mean", "median", "min", "max", "length", "n_distinct"),
        discrete = c("length", "n_distinct", "first", "last")
      )
      
      showModal(modalDialog(
        
        title = "Value settings",
        size = "xl",
        easyClose = length(input$values) == 0L,
        
        fluidRow(column(12,
          if (length(input$values) == 0L) {
            span("No summary columns specified")
          } else {
            map(input$values, function(val) {
              val_type <- if (is.numeric(dataset()[[strip_id(val)]])) "continuous" else "discrete"
              div(
                h4(shiny::code(strip_id(val))),
                radioButtons(
                  inputId = ns(val),
                  label = "Summary function",
                  choices = summary_fns[[val_type]],
                  selected = funs()[[val]],
                  inline = TRUE
                )
              )
            })
          }
        )),
        
        footer = div(
          modalButton("Cancel"),
          actionButton(
            ns("update_functions"), "Apply changes", 
            style = "padding-left:10px"
          )
        )
      ))
    }))
    
    c(reactive(input$rows), reactive(input$columns), values)
    
  })
}



    

    
