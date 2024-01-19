.onLoad <- function(libname, pkgname) {
  addResourcePath("www", system.file("app/www", package = "pivotr"))
}
