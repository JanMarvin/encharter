plot_to_png <- function(chart, wb = NULL) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 600, 400)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(chart, wb = wb)
  f
}

sales_wb <- function() {
  openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Month = month.abb[1:6],
    Sales = c(120, 135, 128, 160, 175, 190),
    Cost  = c(80, -90, 85, 100, -110, 120),
    Day   = as.Date("2024-01-01") + 0:5
  ))
}

test_that("plot_scale follows Excel's automatic axis rules", {
  expect_equal(unlist(plot_scale(0, 190)[c("min", "max", "major")]), c(min = 0, max = 200, major = 20))
  expect_equal(unlist(plot_scale(1150, 1400)[c("min", "max", "major")]), c(min = 0, max = 1600, major = 200))
  expect_equal(unlist(plot_scale(11500, 14000)[c("min", "max", "major")]), c(min = 0, max = 16000, major = 2000))
  expect_equal(unlist(plot_scale(-6, 12)[c("min", "max", "major")]), c(min = -8, max = 14, major = 2))
  expect_equal(unlist(plot_scale(0, 1, pad = FALSE)[c("min", "max", "major")]), c(min = 0, max = 1, major = 0.1))
  expect_equal(plot_scale(0, 10, list(min = 2, max = 8, major = 3))[c("min", "max", "major")], list(min = 2, max = 8, major = 3))
  expect_equal(plot_scale(1, 500, list(log_base = 10))$max, 1000)
})

test_that("plot_format handles common Excel format codes", {
  expect_equal(plot_format(c(12000, 0.5, 1234.5678)), c("12000", "0.5", "1234.5678"))
  expect_equal(plot_format(12345.678, "#,##0"), "12,346")
  expect_equal(plot_format(12345.678, "#,##0.00"), "12,345.68")
  expect_equal(plot_format(0.256, "0.0%"), "25.6%")
  expect_equal(plot_format(3, "0"), "3")
  expect_equal(plot_format(as.Date("2024-03-05"), "dd.mm.yyyy"), "05.03.2024")
  expect_equal(plot_format(as.Date("2024-03-05"), "YYYY"), "2024")
  expect_equal(plot_format(c(1, NA), "0"), c("1", ""))
})

test_that("plot_color converts encharter colors", {
  expect_equal(plot_color("ff0000"), "#FF0000")
  expect_equal(plot_color("80FF0000"), "#FF000080")
  expect_equal(plot_color("auto"), "#4472C4")
  expect_true(is.na(plot_color("none")))
  expect_equal(plot_color(NULL, "#123456"), "#123456")
  expect_equal(plot_color(openxlsx2::wb_color(theme = 5)), "#ED7D31")
  expect_equal(plot_color(openxlsx2::wb_color("red")), "#FF0000")
})

test_that("plot() validates its input", {
  expect_error(plot(ec("bar")), "no series")
  expect_error(plot_to_png(ec("bar3DChart")$add_series(data = "Data!B2:B7")), "not supported")
  expect_error(plot_to_png(ec("bar")$add_series(data = "Data!B2:B7")), "pass 'wb'")
})

test_that("plot() draws the supported chart types", {
  wb <- sales_wb()
  wd <- openxlsx2::wb_data(wb)
  charts <- list(
    ec("bar")$set_chart_title("Combo", bold = TRUE)$set_x_title("Month")$set_y_title("EUR")$
      add_series(name = Sales, data = wd, label = Month, gap_width = 80, overlap = -10)$
      add_series(name = Cost, data = wd, label = Month, type = "line", secondary = TRUE, color = "ED7D31",
                 line_type = "dashed", marker = "diamond", trendline = list(type = "linear"),
                 error_bars = list(type = "percentage", value = 10))$
      set_y2_title("Cost")$set_legend_style(pos = "b")$set_data_label_style(show_val = TRUE, format = "#,##0"),
    ec("bar")$add_series(name = Sales, data = wd, label = Month, dir = "bar", grouping = "stacked")$
      add_series(name = Cost, data = wd, label = Month, dir = "bar", grouping = "stacked")$set_x_axis(rev = TRUE),
    ec("bar")$add_series(name = Sales, data = wd, label = Month, grouping = "percentStacked")$
      add_series(name = Cost, data = wd, label = Month, grouping = "percentStacked")$set_x_axis(label_pos = "low", rotation = -45),
    ec("line")$add_series(name = Sales, data = wd, label = Day, smooth = TRUE, marker = "circle")$
      set_x_axis(format = "dd.mm.yyyy", base_time = "days")$set_y_axis(log_base = 10, min = 10, minor_grid_lines = TRUE),
    ec("area")$add_series(name = Sales, data = wd, label = Month, grouping = "stacked")$
      add_series(name = Cost, data = wd, label = Month, grouping = "stacked"),
    ec("scatter")$add_series(name = Sales, data = wd, label = Cost, show_line = FALSE)$
      add_series(name = Cost, data = wd, label = Sales, secondary = "x")$set_x2_title("top"),
    ec("bubble")$set_bubble_options(scale = 60)$add_series(name = Sales, data = wd, label = Cost, weight = Sales),
    ec("pie")$set_pie_options(rotation = 45, expansion = 10)$set_data_label_style(show_percent = TRUE, show_cat = TRUE)$
      add_series(name = Sales, data = wd, label = Month, color = c("C00000", "4472C4", "70AD47")),
    ec("doughnut")$set_pie_options(hole_size = 40)$add_series(name = Sales, data = wd, label = Month)$set_legend_style(pos = "none"),
    ec("radar")$add_series(name = Sales, data = wd, label = Month, marker = "circle")$
      add_series(name = Cost, data = wd, label = Month, filled = TRUE, color = "ED7D31")
  )
  for (i in seq_along(charts)) {
    f <- plot_to_png(charts[[i]])
    expect_gt(file.info(f)$size, 2000, label = paste("chart", i))
  }
})

test_that("plot() reads values from the workbook when there are no caches", {
  wb <- sales_wb()
  chart <- ec("line")$set_chart_title(openxlsx2::fmt_txt("Two", bold = TRUE) + openxlsx2::fmt_txt("\nlines"))$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7")
  f <- plot_to_png(chart, wb = wb)
  expect_gt(file.info(f)$size, 2000)
  s <- plot_collect(chart, wb)[[1]]
  expect_equal(s$label_text, "Sales")
  expect_equal(s$values, c(120, 135, 128, 160, 175, 190))
  expect_equal(s$cats, month.abb[1:6])
})

test_that("plot() returns the chart invisibly", {
  wb <- sales_wb()
  chart <- ec("bar")$add_series(name = Sales, data = openxlsx2::wb_data(wb), label = Month)
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 300, 300)
  expect_identical(plot(chart), chart)
  grDevices::dev.off()
  expect_gt(file.info(f)$size, 1000)
})

test_that("plot() draws the extended chart types", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Group = rep(c("A", "B", "C"), each = 10),
    Sub   = rep(c("x", "y"), 15),
    Value = c(1:10, seq(5, 50, 5), c(3, 8, 2, 9, 4, 7, 40, 5, 6, 8))
  ))
  charts <- list(
    ec("waterfall")$set_chart_title("Bridge")$
      add_series(name = "Data!$C$1", data = "Data!$C$2:$C$7", label = "Data!$B$2:$B$7", subtotals = c(0, 5)),
    ec("boxWhisker")$set_y_title("Value")$
      add_series(name = "Data!$C$1", data = "Data!$C$2:$C$31", label = "Data!$A$2:$A$31", statistics = "exclusive",
                 visibility = list(meanMarker = TRUE, meanLine = TRUE))$set_legend_style(pos = "r"),
    ec("boxWhisker")$add_series(data = "Data!$C$2:$C$31"),
    ec("clusteredColumn")$add_series(name = "Data!$C$1", data = "Data!$C$2:$C$31",
                                     binning = list(binSize = 10, underflow = 5, overflow = 40)),
    ec("paretoLine")$add_series(name = "Data!$C$1", data = "Data!$C$2:$C$31", binning = list(binCount = 4)),
    ec("funnel")$add_series(name = "Data!$C$1", data = "Data!$C$12:$C$16", label = "Data!$A$12:$A$16"),
    ec("treemap")$add_series(name = "Data!$C$1", data = "Data!$C$2:$C$31", label = "Data!$A$2:$B$31", parent_label = "banner")$
      set_data_label_style(show_val = TRUE, format = "0.0"),
    ec("sunburst")$add_series(name = "Data!$C$1", data = "Data!$C$2:$C$31", label = "Data!$A$2:$B$31")
  )
  for (i in seq_along(charts)) {
    f <- plot_to_png(charts[[i]], wb = wb)
    expect_gt(file.info(f)$size, 2000, label = paste("chartEx", i))
  }
  expect_error(plot(charts[[1]]), "pass the workbook")
  expect_error(plot(ec("regionMap")$add_series(data = "Data!$C$2:$C$4", label = "Data!$A$2:$A$4"), wb = wb), "regionMap")
})

test_that("plot_ex_bins builds Excel-style histogram bins", {
  v <- c(1, 5, 10, 12, 20, 21, 35)
  b <- plot_ex_bins(v, list(binSize = 10))
  expect_equal(b$counts, c(3L, 3L, 0L, 1L))
  expect_equal(b$labels, c("[1, 11]", "(11, 21]", "(21, 31]", "(31, 41]"))
  b <- plot_ex_bins(v, list(binSize = 10, underflow = 10, overflow = 30, intervalClosed = "l"))
  expect_equal(b$counts, c(2L, 2L, 2L, 1L))
  expect_equal(b$labels, c("< 10", "[10, 20)", "[20, 30)", "\u2265 30"))
  b <- plot_ex_bins(v, list(binSize = 10, underflow = 10, overflow = 30))
  expect_equal(b$counts, c(3L, 2L, 1L, 1L))
  expect_equal(b$labels, c("\u2264 10", "(10, 20]", "(20, 30]", "> 30"))
  expect_equal(plot_ex_quartiles(1:8, "inclusive"), c(2.75, 4.5, 6.25))
  expect_equal(plot_ex_quartiles(1:8, "exclusive"), c(2.25, 4.5, 6.75))
})

test_that("plot_ex_hierarchy fills blank outer cells", {
  lv <- data.frame(a = c("A", "", "B"), b = c("x", "y", "z"))
  nodes <- plot_ex_hierarchy(lv, c(1, 2, 3))
  expect_equal(nodes$label[nodes$level == 1], c("A", "B"))
  expect_equal(nodes$value[nodes$level == 1], c(3, 3))
  expect_equal(nodes$parent[nodes$label == "y"], nodes$id[nodes$label == "A"])
})
