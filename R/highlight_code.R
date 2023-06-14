highlight_code <- function(code) {
  
  code <- paste0(
    # Not sure why the -4px indent is needed - first line is out of whack
    # otherwise. Maybe a bug in prism?
    "<pre class='line-numbers' style='text-indent:-4px !important; font-size: 0.85em;'>",
    "<code class='language-r'>",
    paste(code, collapse = "\n"),
    "</code></pre>"
  )
  
  tagList(
    tags$script("Prism.highlightAll()"),
    HTML(code)
  )
}
