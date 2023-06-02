list_datasets <- function(package = "datasets") {
  env <- new.env()
  datasets <- data(package = package)$results[, 3]
  datasets <- datasets[!grepl("\\s", datasets)]
  data(package = package, list = datasets, envir = env)
  out <- purrr::keep(as.list(env), is.data.frame)
  out <- out[sort(names(out))]
  if (package %in% c("datasets", "dplyr", "tidyr")) return(out) 
  set_names(out, ~ paste0(package, "::", .))
}

list_packages <- function() {
  sort(unique(list.files(.libPaths())))
}

dataset_choices <- function(x) {
  choices <- as.list(names(x))
  names(choices) <- sub(".+::", "", choices)
  choices
}
