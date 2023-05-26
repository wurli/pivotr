list_datasets <- function(package = NULL) {
  env <- new_environment()
  dataset_names <- data(package = package)$results[,3]
  dataset_names <- dataset_names[!grepl("\\s", dataset_names)]
  data(list = dataset_names, envir = env, package = package)
  
  as.list(env) |> 
    purrr::keep(is.data.frame) 
}
