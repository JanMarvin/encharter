test_that("validation helper edge cases", {
  ch <- Chart$new("barChart")
  # non-numeric / non-integer values
  expect_error(ch$add_series(data = "Sheet1!B2:B5", gap_width = "wide"), "gap_width")
  expect_error(ch$add_series(data = "Sheet1!B2:B5", overlap = 10.5), "whole number")
  # non-character color, NA color
  expect_error(ch$add_series(data = "Sheet1!B2:B5", color = 42), "color")
  expect_error(ch$add_series(data = "Sheet1!B2:B5", color = NA_character_), "NA")
  # empty format string
  expect_error(ch$set_y_axis(format = ""), "format")
  # movingAvg without period
  expect_error(
    Chart$new("lineChart")$add_series(data = "Sheet1!B2:B5",
                                      trendline = list(type = "movingAvg")),
    "period"
  )
  # wbColour passes through untouched
  expect_silent(ch$add_series(data = "Sheet1!B2:B5", color = openxlsx2::wb_color("green")))
})

test_that("schema range validation rejects invalid values at input time", {
  ch <- Chart$new("barChart")

  # ST_Overlap: -100..100
  expect_error(ch$add_series(data = "Sheet1!B2:B5", overlap = 150), "overlap")
  # ST_GapAmount: 0..500
  expect_error(ch$add_series(data = "Sheet1!B2:B5", gap_width = 501), "gap_width")
  # ST_MarkerSize: 2..72
  expect_error(ch$add_series(data = "Sheet1!B2:B5", marker_size = 1), "marker_size")
  expect_error(ch$add_series(data = "Sheet1!B2:B5", marker_size = 73), "marker_size")
  # negative widths
  expect_error(ch$add_series(data = "Sheet1!B2:B5", line_width = -1), "line_width")

  # in-range values pass
  expect_silent(ch$add_series(data = "Sheet1!B2:B5", overlap = -100, gap_width = 500, marker_size = 72))
})

test_that("trendline and error bar lists are validated", {
  ch <- Chart$new("scatterChart")

  expect_error(
    ch$add_series(data = "Sheet1!B2:B5", trendline = list(type = "quadratic")),
    "trendline\\$type"
  )
  # ST_Order: 2..6, required for poly
  expect_error(
    ch$add_series(data = "Sheet1!B2:B5", trendline = list(type = "poly")),
    "order"
  )
  expect_error(
    ch$add_series(data = "Sheet1!B2:B5", trendline = list(type = "poly", order = 7)),
    "order"
  )
  # ST_Period: >= 2, required for movingAvg
  expect_error(
    ch$add_series(data = "Sheet1!B2:B5", trendline = list(type = "movingAvg", period = 1)),
    "period"
  )
  expect_error(
    ch$add_series(data = "Sheet1!B2:B5", error_bars = list(type = "bogus")),
    "error_bars\\$type"
  )
  expect_error(
    ch$add_series(data = "Sheet1!B2:B5", error_bars = list(type = "cust")),
    "cust"
  )
  expect_error(
    ch$add_series(data = "Sheet1!B2:B5", error_bars = list(type = "stdDev", direction = "up")),
    "direction"
  )
  expect_silent(
    ch$add_series(data = "Sheet1!B2:B5",
                  trendline = list(type = "poly", order = 3),
                  error_bars = list(type = "percentage", value = 10, direction = "plus"))
  )
})

test_that("error_bars direction is written to errBarType", {
  ch <- Chart$new("lineChart")
  ch$add_series(data = "Sheet1!B2:B5",
                error_bars = list(type = "stdDev", value = 1, direction = "minus"))
  expect_true(grepl('<c:errBarType val="minus"/>', ch$render()))
})

test_that("axis parameter validation follows the schema", {
  ch <- Chart$new("lineChart")

  # ST_LogBase: 2..1000
  expect_error(ch$set_y_axis(log_base = 1), "log_base")
  expect_error(ch$set_y_axis(log_base = 1001), "log_base")
  expect_error(ch$set_y_axis(min = 10, max = 5), "max")
  expect_error(ch$set_y_axis(major = 0), "major")
  expect_error(ch$set_x_axis(major_time = "weeks"), "major_time")
  # ST_Skip: >= 1
  expect_error(ch$set_x_axis(tick_lbl_skip = 0), "tick_lbl_skip")
  expect_error(ch$set_y_axis(disp_units = "dozens"), "disp_units")
  expect_error(ch$set_y_axis(disp_units = -5), "disp_units")
  expect_error(ch$set_x_axis(cross_between = "center"), "cross_between")

  expect_silent(ch$set_y_axis(min = 1, max = 100, major = 10, log_base = 10))
})

test_that("colors are validated and R color names are converted", {
  ch <- Chart$new("barChart")
  expect_error(ch$add_series(data = "Sheet1!B2:B5", color = "not-a-color"), "color")

  ch$add_series(data = "Sheet1!B2:B5", color = "red")
  expect_true(grepl('val="FF0000"', ch$render()))

  ch2 <- Chart$new("barChart")
  expect_silent(ch2$add_series(data = "Sheet1!B2:B5", color = "#00ff00"))
  expect_silent(ch2$add_series(data = "Sheet1!B2:B5", color = "80FF0000"))
  expect_silent(ch2$add_series(data = "Sheet1!B2:B5", color = "auto"))
  expect_silent(ch2$set_x_axis(color = "steelblue", grid_color = "grey80"))
})

test_that("pie, bubble, and legend setters validate", {
  ch <- Chart$new("pieChart")
  expect_error(ch$set_pie_options(rotation = 400), "rotation")
  expect_error(ch$set_pie_options(expansion = -1), "expansion")
  # hole_size is intentionally not range checked: values outside the schema
  # range (e.g. 0) render fine in Excel
  expect_silent(ch$set_pie_options(hole_size = 0))
  expect_silent(ch$set_pie_options(rotation = 360, hole_size = 90))

  chb <- Chart$new("bubbleChart")
  expect_error(chb$set_bubble_options(scale = 301), "scale")
  expect_error(chb$set_bubble_options(show_neg = "yes"), "show_neg")

  expect_error(Chart$new("lineChart")$set_legend_style(pos = "middle"), "pos")
  expect_silent(Chart$new("lineChart")$set_legend_style(pos = "tr"))
})

test_that("render warns about combinations Excel refuses to display", {
  ch <- Chart$new("stockChart")
  ch$add_series(data = "Sheet1!B2:B5")
  ch$add_series(data = "Sheet1!C2:C5")
  expect_warning(ch$render(), "3 or 4 series")

  ch2 <- Chart$new("pieChart")
  ch2$add_series(data = "Sheet1!B2:B5")
  ch2$add_series(data = "Sheet1!C2:C5", type = "barChart")
  expect_warning(ch2$render(), "pie-type")

  ch3 <- Chart$new("lineChart")
  ch3$add_series(data = "Sheet1!B2:B5")
  ch3$set_y_axis(min = 0, log_base = 10)
  expect_warning(ch3$render(), "logarithmic")
})

test_that("ChartEx add_series validates its inputs", {
  cx <- ChartEx$new("boxWhisker")
  expect_error(cx$add_series(data = "Sheet1!B2:B5", statistics = "median"), "statistics")
  expect_error(
    ChartEx$new("treemap")$add_series(data = "Sheet1!B2:B5", parent_label = "big"),
    "parent_label"
  )
  expect_error(
    ChartEx$new("waterfall")$add_series(data = "Sheet1!B2:B5", subtotals = c(-1, 2)),
    "subtotals"
  )
  expect_silent(
    ChartEx$new("boxWhisker")$add_series(data = "Sheet1!B2:B5", statistics = "exclusive")
  )
})
