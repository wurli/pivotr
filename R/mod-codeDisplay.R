codeDisplayUI <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("code"))
}

#' Display the code used to generate the PivotTable
#'
#' @param id The `input` slot that will be used to access the value
#' @param dataset A reactive holding the unprocessed data frame
#' @param dataset_code The code used to access the data frame. This may take
#'   various forms, e.g. `ggplot2::diamonds`, or a call to `data()` for some
#'   packages.
#' @param rows,columns,values Reactives giving the rows, columns, and values as
#'   specified in the PivotTable
#'
#' @return A reactive giving code which can be used to produce a
#'   transformed/pivoted version of `dataset()`
#' 
#' @noRd
codeDisplayServer <- function(id, dataset, dataset_code, rows, columns, values) {
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    bindEvent(input$code_clipboard, ignoreInit = TRUE, x = observe({
      shiny::showNotification("Code added to clipboard", duration = 3)
    }))
    
    bindEvent(input$code_clipboard_modal, ignoreInit = TRUE, x = observe({
      shiny::showNotification("Code added to clipboard", duration = 3)
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
          rows = strip_id(rows()),
          columns = strip_id(columns()),
          values = values(),
          code_width         = input$code_controls.code_width %||% 80,
          pipe               = input$code_controls.pipe %||% "base",
          use_function_names = input$code_controls.use_function_names %||% "sometimes",
          use_across         = input$code_controls.use_across %||% "sometimes"
        )
      )
    })
    
    # Need two code outputs - one for sidebar and one for settings modal
    code_ui     <- reactive(highlight_code(code()))
    output$code <- renderUI(
      with_corner_buttons(
        code_ui(),
        corner_button_clipboard(ns("code_clipboard"), code()),
        corner_button(ns("code_settings"), "cog", "Code settings")
      )
    )
    
    output$modal_code <- renderUI(
      with_corner_buttons(
        code_ui(),
        corner_button_clipboard(ns("code_clipboard_modal"), code(), modal = TRUE)
      )
    )
    
    bindEvent(input$code_settings, x = observe({
      showModal(modalDialog(
        easyClose = TRUE,
        title = "PivotTable Code",
        size = "xl",
        fluidRow(
          column(3, sliderInput(
            inputId = ns("code_controls.code_width"),
            label = "Code width",
            value = input$code_controls.code_width %||% 80,
            min = 20,
            max = 250,
            step = 20
          )),
          column(3, radioButtons(
            ns("code_controls.use_across"),
            label = span("Use", shiny::code("across()")),
            selected = input$code_controls.use_across %||% "sometimes",
            choices = c(Never = "never", Sometimes = "sometimes", Always = "always"),
            inline = TRUE
          )),
          column(3, radioButtons(
            ns("code_controls.pipe"),
            label = "Pipe version",
            selected = input$code_controls.pipe %||% "base",
            choiceNames = list(HTML("Base <code>|></code>"), HTML("{magrittr} <code>%>%</code>")),
            choiceValues = c("base", "magrittr"),
            inline = TRUE
          )),
          column(3, radioButtons(
            ns("code_controls.use_function_names"),
            label = "Include summary function in column names",
            selected = input$code_controls.use_function_names %||% "sometimes",
            choices = c(`When necessary` = "sometimes", Always = "always"),
            inline = TRUE
          ))
        ),
        htmlOutput(ns("modal_code"))
      ))
    }))
    
    code
  })
}
