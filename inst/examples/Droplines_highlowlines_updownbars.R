# Line chart with two series (Open, Close) showing the three optional adornments
# that the Chart class exposes as logical fields: drop_lines (vertical lines
# from points down to the X-axis), high_low_lines (vertical lines connecting
# the two series at each category) and up_down_bars (shaded bars between the
# two series). Most useful when comparing two paired series, e.g. for stock
# Open/Close behaviour. Chart at E2:M20.

droplines_etc <- function() {
  require(openxlsx2)
  require(encharter)

  df <- data.frame(
    Day   = paste("Day", 1:5),
    Open  = c(100, 110, 105, 120, 115),
    Close = c(115, 105, 125, 110, 130)
  )

  ch <- encharter(type = "lineChart")

  ch$add_series(
    name  = "Sheet1!$B$1",
    data  = "Sheet1!$B$2:$B$6",
    label = "Sheet1!$A$2:$A$6",
    color = "4F81BD"
  )

  ch$add_series(
    name  = "Sheet1!$C$1",
    data  = "Sheet1!$C$2:$C$6",
    color = "C0504D"
  )

  ch$drop_lines     <- TRUE
  ch$high_low_lines <- TRUE
  ch$up_down_bars   <- TRUE

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = df) |>
    wb_add_encharter(dims = "E2:M20", graph = ch)

  if (interactive()) wb_open(wb)
  invisible(wb)
}

droplines_etc()
