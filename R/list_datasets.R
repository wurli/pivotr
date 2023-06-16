package_datasets <- function(pkg = NULL) {
  pkg <- pkg %||% .packages(TRUE)
  
  pkg |> 
    set_names() |> 
    map(function(pkg) {
      pkg |> 
        tibble_data() |>
        imap(\(df, name) list(value = df, code = get_dataset_code(pkg, name)))
    }) |> 
    purrr::compact()
  
}

tibble_data <- function(pkg) {
  datasets <- data(package = pkg)$results[, "Item"]
  datasets <- datasets[!grepl("\\s", datasets)] 
  
  env <- new.env()
  data(list = datasets, package = pkg, envir = env)
  out <- purrr::keep(as.list(env), can_be_tibble)
  if (length(out) == 0L) out else out[order(names(out))]
}

can_be_tibble <- function(x) {
  tryCatch(
    {
      as_tibble(x)
      is.data.frame(x)
    },
    error = function(e) FALSE,
    warning = function(e) FALSE
  )
}

get_dataset_code <- function(pkg, dataset) {
  purrr::map2_chr(pkg, dataset, function(pkg, dataset) {
    if (pkg %in% c("datasets", "dplyr", "tidyr")) {
      return(dataset)
    }
    tryCatch(
      {
        do.call("::", list(pkg, dataset))
        paste0(pkg, "::", dataset)
      },
      error = function(e) {
        glue('
        data({dataset}, package = "{pkg}")
      
        {dataset}
        ')
      }
    )
  })
}
