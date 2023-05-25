pivotr_app <- function(x = NULL) {
  
  ui <- fluidPage(
    column(1),
    column(7,
      verbatimTextOutput("code"),
      tableOutput("data")
    ),
    column(4,
      fluidRow(
        column(12,
          uiOutput("test")
        )
      ),
      fluidRow(
        column(6,
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            add_rank_list(
              "Filters",
              input_id = "filters"
            )
          )
        ),
        column(6,
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            add_rank_list(
              "Columns",
              input_id = "columns",
              list(.value = "\U03A3 Value")
            )
          )
        )
      ),
      fluidRow(
        column(6,
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            add_rank_list(
              "Rows",
              input_id = "rows"
            )
          )
        ),
        column(6,
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            add_rank_list(
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
    
    output$test <- renderUI(
      sortable::bucket_list(
        "PivotTable Fields",
        group_name = "opts",
        add_rank_list(
          input_id = "fields",
          text = "PivotTable Fields",
          labels = ggplot2::mpg |> 
            imap(~ glue::glue("{.y}<{class(.x)[1]}>"))
        )
      )
    )
    
    observeEvent(input$fields, {
      output$test <- renderUI(
        sortable::bucket_list(
          "PivotTable Fields",
          group_name = "opts",
          add_rank_list(
            input_id = "fields",
            text = "PivotTable Fields",
            labels = ggplot2::mpg |> 
              imap(~ glue::glue("{.y}<{class(.x)}>"))
          )
        )
      )
    })
    
    code <- reactive({
      values <- map(input$values, ~ list(., "sum"))
      
      get_code(
        ggplot2::mpg,
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
