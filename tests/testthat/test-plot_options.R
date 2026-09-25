# plot() options and helpers not covered by the type tests

plot_png <- function(chart, wb = NULL, width = 600, height = 400) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width, height)
  plot(chart, wb = wb)
  grDevices::dev.off()
  file.info(f)$size
}

test_that("plot helpers cover formats, line types, trend equations and error extents", {
  expect_equal(plot_lty("sysDot"), "dotted")
  expect_equal(plot_lty("dashDot"), "dotdash")
  expect_equal(plot_lty("lgDash"), "longdash")
  expect_equal(plot_lty("lgDashDot"), "twodash")
  expect_equal(plot_lty("sysDash"), "22")
  expect_equal(plot_lty(NULL), "solid")
  expect_equal(plot_format(1234.5, "#,##0.00"), "1,234.50")
  expect_equal(plot_format(0.256, "0.0%"), "25.6%")
  expect_equal(plot_format(-3, "[Red]0.00"), "-3.00")
  expect_equal(plot_format(1234, "#,##0_);(#,##0)"), "1,234 ")
  expect_equal(plot_format(12, "\"Total\" 0"), "Total 12")
  expect_equal(plot_format(12, "0 \\k"), "12 k")
  expect_equal(plot_format(as.Date("2024-03-05"), "dd.mm.yyyy"), "05.03.2024")
  expect_equal(plot_format(as.Date("2024-03-05"), "mmm yy"), "Mar 24")
  expect_equal(plot_format(as.Date("2024-03-05"), "mmmm"), "March")
  expect_equal(plot_format(NA_real_), "")
  expect_equal(plot_format(c(1, 2.5)), c("1", "2.5"))

  x <- 1:8
  y <- c(3, 5, 6, 8, 9, 12, 13, 15)
  expect_match(plot_trend_equation(x, y, list(type = "poly", order = 3))$eq, "^y = .*x³ .*x² .*x .*$")
  expect_match(plot_trend_equation(x, y, list(type = "log"))$eq, "ln\\(x\\)")
  expect_match(plot_trend_equation(x, y, list(type = "power"))$eq, "x\\^")
  expect_match(plot_trend_equation(x, y, list(type = "exp"))$eq, "e")
  expect_null(plot_trend_equation(x, y, list(type = "movingAvg")))
  expect_null(plot_trend_equation(x, c(-1, y[-1]), list(type = "exp")))
  expect_null(plot_trend_equation(1, 2, list(type = "linear")))
  expect_equal(length(plot_trend_curve(x, y, list(type = "movingAvg", period = 3))$x), 6)
  expect_length(plot_trend_curve(x, y, list(type = "log"))$y, 100)
  expect_length(plot_trend_curve(x, y, list(type = "power", forward = 1, backward = 1))$y, 100)
  expect_length(plot_trend_curve(x, y, list(type = "poly", order = 2, intercept = 2))$y, 100)
  expect_null(plot_trend_curve(x, y, list(type = "unknown")))
  expect_null(plot_trend_curve(1, 2, list(type = "linear")))
  expect_equal(plot_trend_name(list(type = "exp"), "S"), "Expon. (S)")
  expect_equal(plot_trend_name(list(type = "log"), "S"), "Log. (S)")
  expect_equal(plot_trend_name(list(type = "power"), "S"), "Power (S)")
  expect_equal(plot_trend_name(list(type = "poly"), "S"), "Poly. (S)")
  expect_equal(plot_trend_name(list(type = "movingAvg", period = 3), "S"), "3 per. Mov. Avg. (S)")
  expect_equal(plot_trend_name(list(name = "Fit"), "S"), "Fit")

  expect_equal(plot_error_extent(c(1, 2, 3), list(type = "percentage", value = 10)), c(0.1, 0.2, 0.3))
  expect_equal(plot_error_extent(c(1, 2, 3), list(type = "stdDev", value = 1)), rep(1, 3))
  expect_equal(plot_error_extent(c(1, 2, 3), list(type = "stdErr")), rep(1 / sqrt(3), 3))
  expect_equal(plot_error_extent(c(1, 2, 3), list(type = "cust", value = 2)), rep(2, 3))
  expect_equal(plot_error_extent(c(1, 2, 3), list()), rep(5, 3))

  expect_equal(plot_pch("square"), 22)
  expect_equal(plot_pch("star"), 8)
  expect_true(is.na(plot_pch("none")))
})

test_that("the theme of a saved workbook is used for colors", {
  f <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_workbook()$add_worksheet("S")$save(f)
  wb <- openxlsx2::wb_load(f)
  plot_set_theme(wb)
  on.exit(plot_set_theme(NULL))
  expect_equal(unname(plot_state$theme["accent1"]), "156082")
  expect_equal(plot_color(openxlsx2::wb_color(theme = 4)), "#156082")
  expect_equal(plot_auto_color(7, character()), "#8599AA")
  expect_equal(plot_auto_color(13, character()), plot_auto_color(13, character()))
  plot_set_theme(NULL)
  expect_equal(unname(plot_state$theme["accent1"]), "4472C4")
  expect_match(plot_auto_color(8, c("FF0000")), "^#")
  expect_equal(plot_color("none"), NA_character_)
  expect_equal(plot_color(NULL, "#123456"), "#123456")
})

test_that("axis options are drawn: dates, skips, display units, crossings, positions", {
  months <- data.frame(
    Month = seq(as.Date("2021-01-01"), by = "month", length.out = 30),
    Year = seq(as.Date("2010-01-01"), by = "year", length.out = 30),
    Sales = round(100 + cumsum(rnorm(30, 2, 5))), Cost = round(80 + cumsum(rnorm(30, 1, 4)))
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = months)
  d <- openxlsx2::wb_data(wb, sheet = "Data")

  ch <- ec("line")$add_series(name = Sales, data = d, label = Month)$set_x_axis(major = 6, major_time = "months", base_time = "months")
  expect_gt(plot_png(ch), 1000)
  ch <- ec("line")$add_series(name = Sales, data = d, label = Year)$set_x_axis(base_time = "years", format = "yyyy")
  expect_gt(plot_png(ch), 1000)
  ch <- ec("line")$add_series(name = Sales, data = d, label = Month)$set_x_axis(major = 1, major_time = "years")
  expect_gt(plot_png(ch, width = 300, height = 200), 1000)

  ch <- ec("bar")$add_series(name = Sales, data = d, label = Month, color = "4472C4")$
    set_x_axis(tick_lbl_skip = 3, tick_mark_skip = 2, rotation = -45, label_pos = "high")$
    set_y_axis(disp_units = "millions", format = "0.000", crosses = "max", minor_grid_lines = "dash", minor_tick = "out")
  ch$axis_params$x$auto <- FALSE
  expect_gt(plot_png(ch), 1000)
  ch <- ec("bar")$add_series(name = Sales, data = d, label = Month, color = "4472C4", dir = "bar")$
    set_x_axis(rev = TRUE, label_pos = "low")$set_y_axis(crosses_at = 50, disp_units = 10, label_pos = "high")$
    set_data_label_style(show_val = TRUE, pos = "b")
  expect_gt(plot_png(ch), 1000)
  for (pos in c("inEnd", "ctr", "b", "outEnd")) {
    ch <- ec("bar")$add_series(name = Sales, data = d, label = Month, color = "4472C4")$
      add_series(name = Cost, data = d, label = Month, color = "ED7D31", secondary = TRUE, type = "lineChart",
                 trendline = list(type = "movingAvg", period = 3))$
      set_y_axis(crosses = "min", log_base = 10)$set_y2_axis(crosses = "max", rev = TRUE)$
      set_data_label_style(show_val = TRUE, pos = pos)$set_legend_style(pos = if (pos == "ctr") "l" else "t")
    expect_gt(plot_png(ch), 1000)
  }
  ch <- ec("scatter")$add_series(name = Sales, data = d, label = Cost, color = "4472C4", secondary = "x",
                                 error_bars = list(type = "fixedVal", value = 3, axis = "x"))$
    add_series(name = Cost, data = d, label = Sales, color = "ED7D31", show_line = FALSE, marker = "circle")$
    set_x2_axis(minor_tick = "in", grid_lines = TRUE)$set_x2_title("top")$set_x_title("bottom")$set_y_title("left")
  expect_gt(plot_png(ch), 1000)
  ch <- ec("radar")$add_series(name = Sales, data = d, label = Month, color = "4472C4", marker = "circle", filled = TRUE)$
    add_series(name = Cost, data = d, label = Month, color = "ED7D31", filled = TRUE)$set_legend_style(pos = "tr")
  expect_gt(plot_png(ch), 1000)
})

test_that("wrapped labels with alignment, multi-level categories and bar-of-pie labels are drawn", {
  df <- data.frame(
    Group = c("Alpha group", NA, "Beta group", NA, "Gamma group", NA),
    Item = c("one item with a long name", "two", "three", "four", "five", "six"),
    Sales = c(5, 3, 6, 2, 7, 4)
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = df)
  ch <- ec("bar")$add_series(name = "Data!$C$1", data = "Data!$C$2:$C$7", label = "Data!$A$2:$B$7")$
    set_data_label_style(show_val = TRUE, show_cat = TRUE, pos = "outEnd", align = "l")
  ch$label_params$style$align <- "l"
  expect_gt(plot_png(ch, wb, width = 300, height = 300), 1000)
  ch$label_params$style$align <- "r"
  expect_gt(plot_png(ch, wb, width = 300, height = 300), 1000)
  ch$label_params$style$align <- "ctr"
  ch$series_data[[1]]$dir <- "bar"
  expect_gt(plot_png(ch, wb, width = 300, height = 300), 1000)
  d <- openxlsx2::wb_data(wb, sheet = "Data")
  pie <- ec("barOfPie")$add_series(name = Sales, data = d, label = Item)$
    set_of_pie_options(split_type = "val", split_pos = 4)$set_data_label_style(show_val = TRUE, show_cat = TRUE)
  expect_gt(plot_png(pie), 1000)
  pie <- ec("pieOfPie")$add_series(name = Sales, data = d, label = Item)$set_data_label_style(show_percent = TRUE)
  expect_gt(plot_png(pie), 1000)
  line <- ec("line")$add_series(name = Sales, data = d, label = Item)$add_series(name = Sales, data = d, label = Item, color = "ED7D31")
  line$drop_lines <- TRUE
  line$high_low_lines <- TRUE
  line$disp_blanks_as <- "span"
  expect_gt(plot_png(line), 1000)
})
