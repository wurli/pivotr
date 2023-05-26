pivotr_app <- function(x = NULL) {
  
  datasets <- list_datasets()
  
  ui <- fluidPage(
    column(1,
      selectInput("dataset", "Dataset", names(datasets), selected = "iris")
    ),
    column(7,
      verbatimTextOutput("code"),
      tableOutput("data")
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
              list(.value = "\U03A3 Value")
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
    
    datasets <- list_datasets()
    
    output$fields_bucket <- renderUI(
      sortable::bucket_list(
        "PivotTable Fields",
        group_name = "opts",
        sortable::add_rank_list(
          input_id = "fields",
          text = "PivotTable Fields",
          labels = datasets[[input$dataset]] |> 
            imap(~ glue::glue("{.y}<{vctrs::vec_ptype_abbr(.x)[1]}>"))
        )
      )
    )
    
    observeEvent(input$fields_bucket, {
      output$fields <- renderUI(
        sortable::bucket_list(
          "PivotTable Fields",
          group_name = "opts",
          sortable::add_rank_list(
            input_id = "fields",
            text = "PivotTable Fields",
            labels = datasets[[input$dataset]] |> 
              imap(~ glue::glue("{.y}<{vctrs::vec_ptype_abbr(.x)[1]}>"))
          )
        )
      )
    })
    
    code <- reactive({
      values <- map(input$values, ~ list(., "sum"))
      
      get_code(
        datasets[[input$dataset]],
        input$dataset,
        filters = NULL,
        rows = input$rows,
        columns = input$columns,
        values = values
      )
    })
    
    output$code <- renderText(code())
    output$data <- renderTable(eval(parse(text = code())))
    
    
    observeEvent(input$values_settings, {
      showModal(modalDialog())
    })
    
  }
  
  shinyApp(ui, server)
  
}
