# construct_pivot_code() works

    Code
      test <- (function(width) {
        cat(construct_pivot_code(x = dplyr::storms, x_name = "dplyr::storms",
        columns = "status", rows = ".measure", values = list(list("pressure", "sum"),
        list("wind", "sum"), list("tropicalstorm_force_diameter", "sum")),
        code_width = width))
      })
      test(5)
    Output
      dplyr::storms |>
        summarise(
          .by = status,
          across(
            c(
              pressure,
              wind,
              tropicalstorm_force_diameter
            ),
            sum
          )
        ) |>
        pivot_longer(
          c(
            pressure,
            wind,
            tropicalstorm_force_diameter
          ),
          names_to = ".measure",
          values_to = ".value"
        ) |>
        pivot_wider(
          names_from = status,
          values_from = .value
        )
    Code
      test(40)
    Output
      dplyr::storms |>
        summarise(
          .by = status,
          across(
            c(
              pressure,
              wind,
              tropicalstorm_force_diameter
            ),
            sum
          )
        ) |>
        pivot_longer(
          c(
            pressure,
            wind,
            tropicalstorm_force_diameter
          ),
          names_to = ".measure",
          values_to = ".value"
        ) |>
        pivot_wider(
          names_from = status,
          values_from = .value
        )
    Code
      test(120)
    Output
      dplyr::storms |>
        summarise(
          .by = status,
          across(c(pressure, wind, tropicalstorm_force_diameter), sum)
        ) |>
        pivot_longer(
          c(pressure, wind, tropicalstorm_force_diameter),
          names_to = ".measure",
          values_to = ".value"
        ) |>
        pivot_wider(
          names_from = status,
          values_from = .value
        )

