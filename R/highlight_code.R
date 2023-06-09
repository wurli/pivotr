highlight_code <- function(code) {
  
  code <- paste(code, collapse = "\n")
  code <- sprintf(
    "<pre class='line-numbers'><code class ='language-R'>%s</code></pre>", 
    code
  )
  
  tagList(
    HTML(code),
    tags$script("Prism.highlightAll()")
  )
  
}
