# TODO:
# *  Restrict what you can drag and to where. E.g. can currently remove the 'values'
#    item and not get it back
# *  More options for summary types 
# *  Data import
# *  No errors in the code (e.g. pivot wider no values)
# *  Rename '.measure' to '.value'?? 
# *  Code highlighting/copy to clipboard
# *  Turn off function names in summary?
# *  Allow abritrary renaming of rows/columns?
# *  Allow arbitrary sorting of rows/columns?
# *  Filter??

catch_warnings <- function(..., .msg = "Warning here!") {
  tryCatch(
    ..., 
    warning = function(w) {
      abort("Warning was here", call = caller_env())
    }
  )
}

pivotr <- function(x = NULL) {
  
  user_pkg_datasets <- package_datasets()
  
  pkg_data <- data(package = c("dplyr", "tidyr"))$results[, 3]
  pkg_data_env <- new.env()
  data(list = pkg_data, package = c("dplyr", "tidyr", "datasets"), envir = pkg_data_env)
  
  # User code will use data(), but in the app we'll just load data into 
  # `pkg_data_env`. So, overload data() to avoid creating unnecessary objects
  assign("data", function(...) {}, envir = pkg_data_env)
  
  ui <- page_sidebar(
    
    theme = bs_theme(5, bootswatch = "cerulean"),
    
    title = "{pivotr}",
    
    sidebar = sidebar(
      width = 350,
      navset_card_pill(
        title = "Data",
        height = "280px",
        nav_panel("Package", value = "panel_package_data", icon = icon("cube"), 
          selectInput("package", "Package", names(user_pkg_datasets), selected = "datasets"),
          selectInput("dataset", "Dataset", names(user_pkg_datasets$datasets), selected = "iris")
        ),
        nav_panel("Upload", value = "panel_user_data", icon = icon("upload"),
          fileInput("user_data", "Upload data")
        )
      ),
      div(
        style = "font-size: 0.85em;",
        card(
          full_screen = TRUE,
          htmlOutput("code")
        )
      )
    ),
    
    tags$head(
      
      tags$script(src = "prism.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "prism.css"),
    
      tags$style(HTML("
        .bslib-card, .tab-content, .tab-pane, .card-body {
          overflow: visible !important;
        }
      
        .bucket-list-container.fields-list {
          font-size: 0.95em;
          padding: 0px !important;
          margin: 0px !important;
        }
        
        .rank-list-container.fields-list {
          height: 350px;
          overflow-y: auto;
        }
        
        .bucket-list-container.pivot-table-options-list {
          font-size:0.95em;
          padding: 0px !important;
          margin: 0px !important;
        }
        
        .rank-list-container.pivot-table-options-list {
          min-height:150px;
        }
        
        .default-sortable .rank-list-item {
          padding: 2px 5px !important;
        }
      "))
    ),
    
    fluidRow(
      column(7, style = "padding:5px;",
        reactable::reactableOutput("data")
      ),
      column(5, style = "padding:5px;",
        fluidRow(column(12,
          uiOutput("fields_bucket")
        )),
        uiOutput("pivot_table_fields_ui")
      )
    )
    
    
  )
  
  server <- function(input, output, session) {
    
    summary_spec <- reactiveVal(list())
    datasets     <- reactive(list_datasets(input$package))
    
    freeze_inputs <- function(...) purrr::walk(c(...), ~ freezeReactiveValue(input, .))
    
    bindEvent(input$package, ignoreInit = TRUE, x = observe({
      freeze_inputs("dataset", "fields", "filters", "columns", "rows", "values")
      updateSelectInput(
        inputId = "dataset", 
        choices = names(user_pkg_datasets[[input$package]])
      )
    }))
    
    bindEvent(input$dataset, ignoreInit = TRUE, x = observe({
      freeze_inputs("fields", "filters", "columns", "rows", "values")
    }))
    
    dataset      <- reactive(user_pkg_datasets[[input$package]][[input$dataset]]$value)
    dataset_code <- reactive(user_pkg_datasets[[input$package]][[input$dataset]]$code)
    
    bindEvent(dataset(), x = observe({
      output$pivot_table_fields_ui <- renderUI(pivot_table_fields_ui())
    }))
    
    bindEvent(input$fields, ignoreNULL = FALSE, x = observe({
      if (length(input$fields) != ncol(dataset())) {
        output$fields_bucket <- renderUI({
          
          sortable::bucket_list(
            "PivotTable Fields",
            group_name = "opts",
            sortable::add_rank_list(
              input_id = "fields",
              text = NULL,
              labels = dataset() |> 
                imap(function(x, y) {
                  ptype <- paste0("&lt", vctrs::vec_ptype_abbr(x), "&gt")
                  HTML(glue::glue('<span style="color:grey;">{ptype}</span>{y}'))
                }) |>
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
    
    code <- reactive({
      paste0(
        "# library(dplyr)\n",
        "# library(tidyr)\n",
        "\n",
        construct_pivot_code(
          dataset(),
          dataset_code(),
          filters = NULL,
          rows = input$rows |> strip_id(),
          columns = input$columns |> strip_id(),
          values = values(),
          code_width = 40
        )
      )
    })
    
    output$code <- renderUI(highlight_code(code()))
    output$data <- reactable::renderReactable({
      
      # Load the data into an app-specific environment to avoid polluting
      # global envir
      data(list = input$dataset, package = input$package, envir = pkg_data_env)
      
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
        height = 550,
        resizable = TRUE
      )
      
    })
    
    bindEvent(input$values_settings, x = observe({
      showModal(modalDialog(
        
        easyClose = length(input$values) == 0L,
        
        fluidRow(column(12,
          if (length(input$values) == 0L) {
            span("No summary columns specified")
          } else {
            map(input$values, function(val) {
              div(
                hr(),
                h4(shiny::code(strip_id(val))),
                radioButtons(
                  inputId = val,
                  label = "Summary function",
                  choices = if (is.numeric(dataset()[[strip_id(val)]])) {
                    c("sum", "mean", "median", "length", "n_distinct")
                  } else {
                    c("length", "n_distinct", "first", "last")
                  },
                  selected = funs()[[val]],
                  inline = TRUE
                )
              )
            })
          }
        )),
        
        footer = div(
          modalButton("Cancel"),
          actionButton("update_functions", "Apply changes", 
            style = "padding-left:10px"
          )
        )
      ))
    }))
  }
  
  shinyApp(ui, server)
  
}

pivot_table_fields_ui <- function() {
  list(
    fluidRow(
      column(6, style = "padding-right:0px;",
        sortable::bucket_list(
          NULL,
          group_name = "opts",
          sortable::add_rank_list(
            "Filters",
            input_id = "filters"
          ),
          class = "default-sortable pivot-table-options-list"
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
          ),
          class = "default-sortable pivot-table-options-list"
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
          ),
          class = "default-sortable pivot-table-options-list"
        )
      ),
      column(6, style = "padding-left:0px;",
        sortable::bucket_list(
          NULL,
          group_name = "opts",
          sortable::add_rank_list(
            # as.character |> HTML because title should be character
            HTML(as.character(span(
              "Values",
              actionLink(
                "values_settings", icon("cog"),
                style = "float:right;padding-right:10px"
              )
            ))),
            input_id = "values"
          ),
          class = "default-sortable pivot-table-options-list"
        ) 
      )
    )
  )
}
