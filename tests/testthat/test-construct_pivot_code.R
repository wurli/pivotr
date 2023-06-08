test_that("construct_pivot_code() works", {
  
  expect_snapshot({
    test <- function(width) {
      cat(construct_pivot_code(
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
      ))
    }
    
    test(5)
    test(40)
    test(120)
    
  })
})
