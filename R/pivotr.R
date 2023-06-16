# TODO:
# *  Restrict what you can drag and to where. E.g. can currently remove the 'values'
#    item and not get it back
# *  More options for summary types 
# *  Allow abritrary renaming of rows/columns?
# *  Allow arbitrary sorting of rows/columns?
# *  Filter??

#' Run the {pivotr} app
#'
#' This brings up a GUI similar to Excel's PivotTable interface. Data 
#' manipulations will be performed and the dplyr/tidyr code used will be
#' displayed.
#'
#' @return An object that represents the app
#' @export
pivotr <- function() {
  
  resources <- system.file("app/www", package = "pivotr")
  addResourcePath("www", resources)
  
  user_pkg_datasets <- package_datasets()
  
  pkg_data <- data(package = c("dplyr", "tidyr"))$results[, 3]
  pkg_data_env <- new.env()
  data(list = pkg_data, package = c("dplyr", "tidyr", "datasets"), envir = pkg_data_env)
  
  # User code will use data(), but in the app we'll just load data into 
  # `pkg_data_env`. So, overload data() to avoid creating unnecessary objects
  assign("data", function(...) NULL, envir = pkg_data_env)
  
  ui <- page_sidebar(
    theme = bs_theme(5, bootswatch = "cerulean"),
    title = "{pivotr} - Excel's PivotTables in R",
    
    rclipboard::rclipboardSetup(),
    
    tags$head(

      htmltools::htmlDependency(
        name = "resources",
        version = "0.0.1",
        src = resources,
        script = list.files(resources, pattern = "\\.js$", recursive = TRUE),
        package = NULL,
        all_files = TRUE
      ),

      map(
        list.files(resources, pattern = "\\.css$", recursive = TRUE),
        function(x) tags$link(href = file.path("www", x), rel = "stylesheet")
      )
      
    ),
    
    sidebar = sidebar(
      width = 350,
      navset_card_pill(
        title = "Data",
        id = "data_selection_tabs",
        height = "280px",
        nav_panel("Package", value = "panel_package_data", icon = icon("cube"), 
          selectInput("package", "Package", names(user_pkg_datasets), selected = "datasets"),
          selectInput("dataset", "Dataset", names(user_pkg_datasets$datasets), selected = "infert")
        ),
        nav_menu(
          title = "Import",
          icon = icon("upload"),
          nav_panel("From CSV", value = "panel_file_data", icon = icon("upload"),
            fileInput("user_data.upload", NULL, accept = ".csv")
          ),
          nav_panel("From global environment", value = "panel_env_data", icon = icon("table"),
            selectizeInput(
              "user_data.from_env",
              "Select an object to import",
              options = list(
                placeholder = 'Select a dataset',
                onInitialize = I('function() { this.setValue(""); }')
              ),
              choices = mget(ls(envir = .GlobalEnv), .GlobalEnv) |> 
                purrr::keep(is.data.frame) |> 
                names()
            )
          )
        )
      ),
      htmlOutput("code")
    ),
    
    fluidRow(
      column(7, style = "padding:5px;",
        card(
          full_screen = TRUE,
          reactable::reactableOutput("data")
        )
      ),
      column(5, style = "padding:5px;",
        h4("PivotTable Fields"),
        fluidRow(column(12,
          uiOutput("fields_bucket")
        )),
        uiOutput("pivot_table_fields_ui")
      )
    )
    
    
  )
  
  server <- function(input, output, session) {
    
    bindEvent(input$code_clipboard, ignoreInit = TRUE, x = observe({
      shiny::showNotification("Code added to clipboard", duration = 3)
    }))
    bindEvent(input$code_clipboard_modal, ignoreInit = TRUE, x = observe({
      shiny::showNotification("Code added to clipboard", duration = 3)
    }))
    
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
    
    dataset           <- reactiveVal(isolate(user_pkg_datasets[[input$package]][[input$dataset]]$value))
    dataset_code      <- reactiveVal(isolate(user_pkg_datasets[[input$package]][[input$dataset]]$code))
    file_dataset      <- reactiveVal()
    file_dataset_code <- reactiveVal()
    env_dataset       <- reactiveVal()
    env_dataset_code  <- reactiveVal() 
    
    bindEvent(input$package, input$dataset, input$data_selection_tabs, ignoreInit = TRUE, x = observe({
      if (input$data_selection_tabs == "panel_package_data") {
        freeze_inputs("fields", "filters", "columns", "rows", "values")
        dataset(user_pkg_datasets[[input$package]][[input$dataset]]$value)
        dataset_code(user_pkg_datasets[[input$package]][[input$dataset]]$code) 
      }
      
      if (input$data_selection_tabs == "panel_file_data" && !is.null(file_dataset())) {
        freeze_inputs("fields", "filters", "columns", "rows", "values")
        dataset(file_dataset())
        dataset_code(file_dataset_code())
      } 
      
      if (input$data_selection_tabs == "panel_env_data" && !is.null(env_dataset())) {
        freeze_inputs("fields", "filters", "columns", "rows", "values")
        dataset(env_dataset())
        dataset_code(env_dataset_code())
      }
      
    }))
    
    bindEvent(input$user_data.upload, x = observe({
      freeze_inputs("fields", "filters", "columns", "rows", "values")
      imported_dataset <- readr::read_csv(
        input$user_data.upload$datapath, 
        progress = FALSE, show_col_types = FALSE
      )
      assign("dataset", imported_dataset, envir = pkg_data_env)
      file_dataset(imported_dataset)
      file_dataset_code(glue(
        '# dataset <- readr::read_csv("{name}")\n\ndataset',
        name = input$user_data.upload$name
      ))
      dataset(file_dataset())
      dataset_code(file_dataset_code())
    }))
    
    bindEvent(input$user_data.from_env, ignoreInit = TRUE, x = observe({
      freeze_inputs("fields", "filters", "columns", "rows", "values")
      dataset_name <- input$user_data.from_env
      data_from_env <- get(dataset_name, envir = .GlobalEnv)
      assign(dataset_name, data_from_env, envir = pkg_data_env)
      env_dataset(data_from_env)
      env_dataset_code(dataset_name)
      dataset(env_dataset())
      dataset_code(env_dataset_code())
    }))
    
    bindEvent(dataset(), x = observe({
      output$pivot_table_fields_ui <- renderUI(pivot_table_fields_ui())
    }))
    
    bindEvent(input$fields, ignoreNULL = FALSE, x = observe({
      if (length(input$fields) != ncol(dataset())) {
        output$fields_bucket <- renderUI({
          
          sortable::bucket_list(
            NULL, #"PivotTable Fields",
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
          code_width         = input$code_controls.code_width %||% 80,
          pipe               = input$code_controls.pipe %||% "base",
          use_function_names = input$code_controls.use_function_names %||% "sometimes",
          use_across         = input$code_controls.use_across %||% "sometimes"
        )
      )
    })
    
    # Need two code outputs - one for sidebar and one for settings modal
    code_ui           <- reactive(highlight_code(code()))
    output$code       <- renderUI(
      with_corner_buttons(
        code_ui(),
        corner_button_clipboard("code_clipboard", code()),
        corner_button("code_settings", "cog", "Code settings")
      )
    )
    output$model_code <- renderUI(
      with_corner_buttons(
        code_ui(),
        corner_button_clipboard("code_clipboard_modal", code(), modal = TRUE)
      )
    )
    
    output$data <- reactable::renderReactable({
      
      # Load the data into an app-specific environment to avoid polluting
      # global envir
      data(list = input$dataset, package = input$package, envir = pkg_data_env)
      
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
    
    bindEvent(input$code_settings, x = observe({
      showModal(modalDialog(
        easyClose = TRUE,
        title = "PivotTable Code",
        size = "xl",
        fluidRow(
          column(3, sliderInput(
            inputId = "code_controls.code_width",
            label = "Code width",
            value = input$code_controls.code_width %||% 40,
            min = 20,
            max = 250,
            step = 20
          )),
          column(3, radioButtons(
            "code_controls.use_across",
            label = span("Use", shiny::code("across()")),
            selected = input$code_controls.use_across %||% "sometimes",
            choices = c(Never = "never", Sometimes = "sometimes", Always = "always"),
            inline = TRUE
          )),
          column(3, radioButtons(
            "code_controls.pipe",
            label = "Pipe version",
            selected = input$code_controls.pipe %||% "base",
            choiceNames = list(HTML("Base <code>|></code>"), HTML("{magrittr} <code>%>%</code>")),
            choiceValues = c("base", "magrittr"),
            inline = TRUE
          )),
          column(3, radioButtons(
            "code_controls.use_function_names",
            label = "Include summary function in column names",
            selected = input$code_controls.use_function_names %||% "sometimes",
            choices = c(`When necessary` = "sometimes", Always = "always"),
            inline = TRUE
          ))
        ),
        htmlOutput("model_code")
      ))
    }))
    
    bindEvent(input$values_settings, x = observe({
      showModal(modalDialog(
        
        title = "Value settings",
        size = "xl",
        easyClose = length(input$values) == 0L,
        
        fluidRow(column(12,
          if (length(input$values) == 0L) {
            span("No summary columns specified")
          } else {
            map(input$values, function(val) {
              div(
                h4(shiny::code(strip_id(val))),
                radioButtons(
                  inputId = val,
                  label = "Summary function",
                  choices = if (is.numeric(dataset()[[strip_id(val)]])) {
                    c("sum", "mean", "median", "min", "max", "length", "n_distinct")
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
        with_corner_buttons(
          corner_button("values_settings", "cog", style = "right:-20px;"),
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            sortable::add_rank_list(
              "Values",
              input_id = "values"
            ),
            class = "default-sortable pivot-table-options-list"
          ) 
        )
      )
    )
  )
}
