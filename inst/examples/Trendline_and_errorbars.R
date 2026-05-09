# Bar chart with two per-series adornments demonstrated together:
# percentage error bars (10%, dark gray 404040) and a linear trendline (red
# FF0000) without R-squared shown. Chart at D2:L20.

trendline_and_errorbars <- function() {
  require(openxlsx2)
  require(encharter)

  df <- data.frame(
    Month   = month.abb[1:6],
    Revenue = c(100, 120, 110, 150, 140, 170)
  )

  ch <- encharter(type = "barChart")

  ch$add_series(
    name  = "Monthly Revenue",
    data  = "Sheet1!$B$2:$B$7",
    label = "Sheet1!$A$2:$A$7",
    type  = "barChart",
    error_bars = list(
      type  = "percentage",
      value = 10,
      color = "404040"
    ),
    trendline = list(
      type    = "linear",
      color   = "FF0000",
      show_r2 = FALSE
    )
  )

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = df) |>
    wb_add_encharter(dims = "D2:L20", graph = ch)

  if (interactive()) wb_open(wb)
  invisible(wb)
}

trendline_and_errorbars()
