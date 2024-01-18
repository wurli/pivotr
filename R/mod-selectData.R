
selectDataUI <- function(id, user_pkg_datasets) {
  ns <- NS(id)
  navset_card_pill(
    title = "Data",
    id = ns("data_selection_tabs"),
    height = "280px",
    nav_panel("Package", value = "panel_package_data", icon = icon("cube"), 
      selectInput(ns("package"), "Package", names(user_pkg_datasets), selected = "datasets"),
      selectInput(ns("dataset"), "Dataset", names(user_pkg_datasets$datasets), selected = "infert")
    ),
    nav_menu(
      title = "Import",
      icon = icon("upload"),
      nav_panel("From CSV", value = "panel_file_data", icon = icon("upload"),
        fileInput(ns("user_data.upload"), NULL, accept = ".csv")
      ),
      nav_panel("From global environment", value = "panel_env_data", icon = icon("table"),
        selectizeInput(
          ns("user_data.from_env"),
          "Select an object to import",
          options = list(
            placeholder = 'Select a dataset',
            onInitialize = I('function() { this.setValue(""); }') # Set no initial value
          ),
          # Data frames in the user's workspace
          choices = mget(ls(envir = .GlobalEnv), .GlobalEnv) |> 
            purrr::keep(is.data.frame) |> 
            names()
        )
      )
    )
  )
}

#' Select a dataset in the sidebar
#'
#' @param id The `input` slot that will be used to access the value
#' @param pkg_data_env The environment in which to evaluate `code()`
#' @param user_pkg_datasets A list of datasets provided by the user's installed
#'   packages
#' @param freeze_pivottable A function which can be used to freeze the 
#'   pivot table fields if/when the input dataset changes
#'
#' @return A vector of reactives: the dataset itself, the name of the dataset,
#'   and the code used to access the dataset
#'
#' @noRd
selectDataServer <- function(id, pkg_data_env, user_pkg_datasets, freeze_pivottable) {
  moduleServer(id, function(input, output, session) {
    
    # Update dataset options depending on selected package
    bindEvent(input$package, ignoreInit = TRUE, x = observe({
      freeze_pivottable()
      updateSelectInput(
        inputId = "dataset", 
        choices = names(user_pkg_datasets[[input$package]])
      )
      freezeReactiveValue(input, "dataset")
    }))
    
    bindEvent(input$dataset, ignoreInit = TRUE, x = observe({
      freeze_pivottable()
    }))
    
    dataset           <- reactiveVal(isolate(user_pkg_datasets[[input$package]][[input$dataset]]$value))
    dataset_code      <- reactiveVal(isolate(user_pkg_datasets[[input$package]][[input$dataset]]$code))
    file_dataset      <- reactiveVal()
    file_dataset_code <- reactiveVal()
    env_dataset       <- reactiveVal()
    env_dataset_code  <- reactiveVal() 
    
    bindEvent(input$package, input$dataset, input$data_selection_tabs, ignoreInit = TRUE, x = observe({
      if (input$data_selection_tabs == "panel_package_data") {
        freeze_pivottable()
        dataset(user_pkg_datasets[[input$package]][[input$dataset]]$value)
        dataset_code(user_pkg_datasets[[input$package]][[input$dataset]]$code) 
      }
      
      if (input$data_selection_tabs == "panel_file_data" && !is.null(file_dataset())) {
        freeze_pivottable()
        dataset(file_dataset())
        dataset_code(file_dataset_code())
      } 
      
      if (input$data_selection_tabs == "panel_env_data" && !is.null(env_dataset())) {
        freeze_pivottable()
        dataset(env_dataset())
        dataset_code(env_dataset_code())
      }
    }))
    
    bindEvent(input$user_data.upload, x = observe({
      freeze_pivottable()
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
      freeze_pivottable()
      dataset_name <- input$user_data.from_env
      data_from_env <- get(dataset_name, envir = .GlobalEnv)
      assign(dataset_name, data_from_env, envir = pkg_data_env)
      env_dataset(data_from_env)
      env_dataset_code(dataset_name)
      dataset(env_dataset())
      dataset_code(env_dataset_code())
    }))
    
    
    c(dataset, reactive(input$dataset), dataset_code, reactive(input$package))
  })
}
