test_that("construct_pivot_code() works", {
  
  expect_snapshot({
    test <- function(width) {
      construct_pivot_code(
        x = dplyr::storms,
        x_name = "dplyr::storms",
        columns = "status",
        rows = ".measure",
        values = list(
          list("pressure", "sum"),
          list("wind", "sum"),
          list("tropicalstorm_force_diameter", "sum")
        ),
        code_width = width
      )
    }
    
    cat(test(5))
    cat(test(40))
    cat(test(120))
    
  })
})
