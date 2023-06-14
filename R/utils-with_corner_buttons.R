with_corner_buttons <- function(...) {
  
  dots <- list(...)
  is_button <- vapply(dots, is_corner_button, logical(1))
  buttons <- dots[is_button]
  
  buttons <- div(
    class = "parent",
    
    tags$style(HTML("
      .inline-block-child {
        display: inline-block;
      }
    ")),
    
    !!!map(seq_along(buttons), function(i) {
      right <- paste0("right: ", i * 35, "px; ")
      div(
        class = "corner-button child inline-block-child", 
        style = paste0(right, "margin: 0px;"),
        buttons[[i]]
      )
    })
  )
  
  div(
    class = "corner-buttons-wrapper",
    !!!dots[!is_button],
    buttons
  )
  
}

corner_button <- function(inputId, icon, tooltip = NULL, ...) {
  if (is.character(icon)) {
    icon <- shiny::icon(icon)
  }
  
  out <- tags$button(
    class = "action-button corner-button",
    title = tooltip,
    type = "button",
    id = inputId,
    icon,
    ...
  )
  
  class(out) <- c("corner_button", class(out))
  out
}

is_corner_button <- function(x) {
  inherits(x, "corner_button")
}
