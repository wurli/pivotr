# TODO:
# *  Restrict what you can drag and to where. E.g. can currently remove the 'values'
#    item and not get it back
# *  More options for summary types 
# *  Allow abritrary renaming of rows/columns?
# *  Allow arbitrary sorting of rows/columns?
# *  Filter??

#' Run the {pivotr} app
#'
#' This brings up a GUI similar to Excel's PivotTable interface. The 
#' dplyr/tidyr code used to achieve the data transformations can be copied
#' for modification/programmatic use.
#'
#' @return An object that represents the app. Printing the object or passing it 
#'  to [shiny::runApp()] will run the app.
#' @export
pivotr <- function() {
  resources <- system.file("app/www", package = "pivotr")
  user_pkg_datasets <- package_datasets()
  shinyApp(
    pivotr_ui(resources, user_pkg_datasets), 
    pivotr_server(user_pkg_datasets)
  )
}


pivotr_server <- function(user_pkg_datasets) {
  function(input, output, session) {
    # -- Package datasets ------------------------------------------------------
    # Datasets loaded from packages are bound in this environment as the user
    # accesses them. This is to avoid polluting the global namespace.
    pkg_data_env <- new.env()
    
    # Initially, start with these datasets loaded
    pkg_data <- data(package = c("dplyr", "tidyr"))$results[, 3]
    data(
      list = pkg_data, 
      package = c("dplyr", "tidyr", "datasets"),
      envir = pkg_data_env
    )
    
    # User code will use data(), but in the app we'll just load data into 
    # pkg_data_env. So, overload data() to avoid creating unnecessary objects
    assign("data", function(...) NULL, envir = pkg_data_env)
    
    # -- Modules ---------------------------------------------------------------
    # When the dataset changes, need to freeze pivottable controls until they've
    # been updated to reflect the new fields. This function provides a way for
    # one module to reach into another one and do this.
    # TODO: think of a more elegant way of doing this.
    freeze_pivottable <- function() {
      purrr::walk(
        paste0(
          "pivot_table-", 
          c("fields", "filters", "columns", "rows", "values")
        ),
        ~ freezeReactiveValue(input, .)
      )
    }
    
    c(dataset, dataset_name, dataset_code, dataset_pkg) %<-% selectDataServer("select_data",
      pkg_data_env, user_pkg_datasets, freeze_pivottable
    ) 
    
    code <- codeDisplayServer("code_display",
      dataset, dataset_code, rows, columns, values
    )
    
    c(rows, columns, values) %<-% pivotTableServer("pivot_table", 
      dataset, dataset_name, dataset_pkg, code, pkg_data_env
    )
    
  }
}


pivotr_ui <- function(resources, user_pkg_datasets) {
  page_sidebar(
    theme = bs_theme(5, bootswatch = "cerulean"),
    title = "{pivotr} - Excel's PivotTables in R",
    
    rclipboard::rclipboardSetup(),
    
    # -- Add resources ---------------------------------------------------------
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
      selectDataUI("select_data", user_pkg_datasets),
      codeDisplayUI("code_display")
    ),
    
    pivotTableUI("pivot_table")
  )
}
