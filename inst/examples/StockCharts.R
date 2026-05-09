# nolint start: object_usage_linter.
# Stock chart pieced together by hand: three scatter-style series (High, Low,
# Close) without connecting lines, an X-axis based on dates, plus the optional
# adornments turned on directly via fields - high_low_lines = TRUE for the
# vertical wick between High/Low and up_down_bars = TRUE for the body. Uses
# wb_data() so series are added by column name rather than cell range.

stock_charts <- function() {
  require(openxlsx2)
  require(encharter)

  stock_data <- data.frame(
    Date  = seq(as.Date("2020-01-21"), as.Date("2020-02-04"), by = "day"),
    High  = c(42.30, 42.70, 42.97, 41.53, 42.03, 42.78, 43.08, 43.78, 44.11,
              44.98, 45.09, 45.45, 45.23, 45.62, 45.31),
    Low   = c(40.29, 41.37, 41.00, 40.65, 41.33, 42.11, 42.73, 43.28, 43.86,
              44.36, 44.75, 45.03, 44.86, 45.21, 44.91),
    Close = c(41.23, 42.21, 41.29, 41.03, 41.86, 42.53, 42.99, 43.62, 44.03,
              44.75, 45.02, 45.33, 44.98, 45.48, 45.03)
  )

  wb <- wb_workbook()$add_worksheet("Sheet1")$add_data(x = stock_data)
  df <- wb_data(wb)

  stock_chart <- ec("stockChart")

  stock_chart$add_series(
    data      = df,
    label     = Date,
    name      = High,
    show_line = FALSE,
    marker    = "circle"
  )

  stock_chart$add_series(
    data      = df,
    label     = Date,
    name      = Low,
    show_line = FALSE
  )

  stock_chart$add_series(
    data      = df,
    label     = Date,
    name      = Close,
    show_line = FALSE
  )

  stock_chart$set_x_axis(base_time = "days")

  stock_chart$high_low_lines <- TRUE
  stock_chart$drop_lines     <- FALSE
  stock_chart$up_down_bars   <- TRUE

  wb$add_encharter(sheet = "Sheet1", graph = stock_chart)

  if (interactive()) wb$open()
  invisible(wb)
}

stock_charts()

# nolint end
