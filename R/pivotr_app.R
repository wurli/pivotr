pivotr_app <- function(x = NULL) {
  
  ui <- fluidPage(
    column(1),
    column(8),
    column(3,
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
              "test1"
            )
          )
        ),
        column(6,
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            add_rank_list(
              "Filters",
              "test1"
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
              "Filters",
              "test1"
            )
          )
        ),
        column(6,
          sortable::bucket_list(
            NULL,
            group_name = "opts",
            add_rank_list(
              "Filters",
              "test1"
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
          labels = colnames(iris)
        )
      )
    )
      
    
  }
  
  shinyApp(ui, server)
  
}
