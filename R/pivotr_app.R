pivotr_app <- function(x = NULL) {
  
  pkg_data <- data(package = c("dplyr", "tidyr"))$results[, 3]
  pkg_data_env <- new.env()
  data(list = pkg_data, package = c("dplyr", "tidyr"), envir = pkg_data_env)
  
  ui <- fluidPage(
    column(2,
      selectInput("package", "Package", list_packages(), selected = "datasets"),
      selectInput("dataset", "Dataset", dataset_choices(list_datasets()), selected = "iris")
    ),
    column(6,
      verbatimTextOutput("code"),
      reactable::reactableOutput("data")
    ),
    column(4,
      fluidRow(
        column(12,
          uiOutput("fields_bucket")
        )
      ),
      fluidRow(
        column(6, style = "padding-right:0px;",
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            sortable::add_rank_list(
              "Filters",
              input_id = "filters"
            )
          )
        ),
        column(6, style = "padding-left:0px;",
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            sortable::add_rank_list(
              "Columns",
              input_id = "columns",
              list(.measure = "\U03A3 Value")
            )
          )
        )
      ),
      fluidRow(
        column(6, style = "padding-right:0px;",
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            sortable::add_rank_list(
              "Rows",
              input_id = "rows"
            )
          )
        ),
        column(6, style = "padding-left:0px;",
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            sortable::add_rank_list(
              span(
                "Values",
                actionLink(
                  "values_settings", icon("cog"),
                  style = "float:right;padding-right:10px"
                )
              ),
              input_id = "values"
            )
          )
        )
      )
    )
    
  )
  
  server <- function(input, output, session) {
    
    summary_spec <- reactiveVal(list())
    
    datasets <- reactive(list_datasets(input$package))
    
    bindEvent(input$package, ignoreInit = TRUE, x = observe({
      updateSelectInput(inputId = "dataset", choices = dataset_choices(datasets()))
    }))
    
    dataset <- reactive(datasets()[[input$dataset]])
    
    bindEvent(input$fields, ignoreNULL = FALSE, x = observe({
      if (length(input$fields) != ncol(dataset())) {
        output$fields_bucket <- renderUI(
          sortable::bucket_list(
            "PivotTable Fields",
            group_name = "opts",
            sortable::add_rank_list(
              input_id = "fields",
              text = "PivotTable Fields",
              labels = dataset() |> 
                imap(~ glue::glue("{.y}<{vctrs::vec_ptype_abbr(.x)[1]}>")) |>
                set_names(~ paste0(random_id(), "__", .))
            )
          )
        )
      }
    }))
    
    values <- reactive({
      input$values |>
        strip_id() |> 
        map(function(colname) {
          col <- dataset()[[colname]]
          fun <- if (is.numeric(col)) "sum" else "length" 
          list(cols = colname, funs = fun)
        })
    })
    
    code <- reactive({
      paste0(
        "# library(dplyr)\n",
        "# library(tidyr)\n",
        "\n",
        get_code(
          dataset(),
          input$dataset,
          filters = NULL,
          rows = input$rows |> strip_id(),
          columns = input$columns |> strip_id(),
          values = values()
        )
      )
    })
    
    output$code <- renderText(code())
    output$data <- reactable::renderReactable({
      
      df <- eval(parse(text = code()), envir = pkg_data_env)
      
      if (ncol(df) == 0L) {
        df <- tibble::tibble(`-` = integer())
      }
      
      reactable::reactable(
        df,
        bordered = TRUE,
        striped = TRUE,
        outlined = TRUE,
        compact = TRUE,
        pagination = FALSE,
        sortable = FALSE,
        height = 600,
        resizable = TRUE
      )
      
    })
    
    
    observeEvent(input$values_settings, {
      showModal(modalDialog(
        
      ))
    })
    
  }
  
  shinyApp(ui, server)
  
}
