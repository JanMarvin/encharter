test_that("update_series() changes every field of a standard series", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    k = c("a", "b", "c"), v = c(3, 4, 2), w = c(5, 6, 4), z = c(1, 2, 3)
  ))
  d <- openxlsx2::wb_data(wb, sheet = "Data")
  ch <- ec("line")$add_series(name = v, data = d, label = k)
  expect_error(ec("line")$update_series(color = "FF0000"), "no series")
  expect_error(ch$update_series(index = 2, color = "FF0000"), "between 1 and 1")
  expect_error(ch$update_series(data = "B2:B4"), "sheet reference")
  ch$update_series(
    name = "Data!$C$1", data = "Data!$C$2:$C$4", label = "Data!$A$2:$A$4", weight = "Data!$D$2:$D$4",
    color = "FF0000", type = "scatterChart", secondary = TRUE, dir = "bar", grouping = "stacked",
    overlap = 50, gap_width = 80, smooth = TRUE, show_line = FALSE, marker = "diamond", marker_size = 9,
    marker_fill = "00FF00", marker_line = "0000FF", marker_line_width = 2, line_type = "dash", line_width = 3,
    line_color = "123456", filled = TRUE, error_bars = list(type = "percentage", value = 5),
    trendline = list(type = "linear"), invert_if_negative = TRUE
  )
  s <- ch$series_data[[1]]
  expect_equal(s$name, "'Data'!$C$1")
  expect_null(s$data_cache)
  expect_equal(s$weight, "'Data'!$D$2:$D$4")
  expect_equal(s$line$color, "123456")
  expect_equal(s$marker$symbol, "diamond")
  expect_equal(s$marker$fill, "00FF00")
  expect_equal(s$marker$line$width, 2)
  expect_equal(s$type, "scatterChart")
  expect_equal(s$sec_type, "y")
  expect_equal(s$grouping, "stacked")
  expect_true(s$smooth)
  expect_false(s$line$show)
  expect_true(s$invert_if_negative)
  # data from wb_data() replaces the caches
  ch$update_series(name = w, data = d, label = k, weight = z, color = "00FF00")
  s <- ch$series_data[[1]]
  expect_equal(s$data_cache, c(5, 6, 4))
  expect_equal(s$z_cache, c(1, 2, 3))
  expect_equal(s$marker$fill, "00FF00")
  expect_match(ch$render(), "<c:bubbleSize>|<c:xVal>")
  expect_error(ch$update_series(marker = "blob"), "marker")
  expect_error(ch$update_series(dir = "sideways"), "dir")
  expect_error(ch$update_series(grouping = "heap"), "grouping")
  expect_error(ch$update_series(type = "pizzaChart"), "series type")
})

test_that("update_series() changes the fields of an extended series", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(k = c("a", "b", "c"), v = c(3, -4, 2)))
  d <- openxlsx2::wb_data(wb, sheet = "Data")
  ch <- ec("waterfall")$add_series(name = v, data = d, label = k)
  expect_error(ec("waterfall")$update_series(color = "FF0000"), "no series")
  expect_error(ch$update_series(index = 3, color = "FF0000"), "between 1 and 1")
  expect_error(ch$update_series(data = "B2:B4"), "sheet reference")
  ch$update_series(
    name = "Data!$B$1", data = "Data!$B$2:$B$4", label = "Data!$A$2:$A$4", color = "FF0000",
    line_color = "0000FF", line_width = 2, gap_width = 50, subtotals = c(0, 2),
    visibility = list(connectorLines = FALSE), parent_label = "banner"
  )
  s <- ch$series_data[[1]]
  expect_equal(s$name, "'Data'!$B$1")
  expect_equal(s$color, "FF0000")
  expect_equal(s$line_color, "0000FF")
  expect_equal(s$subtotals, c(0, 2))
  expect_equal(s$parent_label, "banner")
  expect_match(ch$render(), "<cx:subtotals>")
  ch2 <- ec("clusteredColumn")$add_series(name = v, data = d)
  ch2$update_series(binning = list(binCount = 3), statistics = NULL)
  expect_equal(ch2$series_data[[1]]$binning$binCount, 3)
  ch3 <- ec("boxWhisker")$add_series(name = v, data = d, label = k)
  ch3$update_series(statistics = "exclusive")
  expect_equal(ch3$series_data[[1]]$statistics, "exclusive")
})

test_that("extended charts write grid line styles and quote sheet names", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("My Data")$add_data(x = data.frame(k = c("a", "b", "c"), v = c(3, 4, 2)))
  ch <- ec("waterfall")$add_series(name = "My Data!$B$1", data = "My Data!$B$2:$B$4", label = "My Data!$A$2:$A$4")$
    set_y_axis(grid_lines = "dotted", minor_grid_lines = "dash", grid_color = "FF0000", minor_grid_width = 0.5)$
    set_chart_title("T", font_size = 12, bold = TRUE, italic = TRUE, font_color = "FF0000", font_name = "Arial")
  xml <- ch$render()
  expect_equal(ch$series_data[[1]]$data, "'My Data'!$B$2:$B$4")
  expect_match(xml, '<cx:majorGridlines><cx:spPr><a:ln w="12700"><a:solidFill><a:srgbClr val="FF0000"/></a:solidFill><a:prstDash val="dot"/>', fixed = TRUE)
  expect_match(xml, '<a:rPr sz="1200" b="1" i="1"><a:solidFill><a:srgbClr val="FF0000"/></a:solidFill><a:latin typeface="Arial"/>', fixed = TRUE)
  ch$set_x_axis(font_size = 9, bold = TRUE, italic = FALSE, font_color = "112233", font_name = "Arial")
  expect_match(ch$render(), "<a:endParaRPr sz=\"900\" b=\"1\"><a:solidFill><a:srgbClr val=\"112233\"/></a:solidFill><a:latin typeface=\"Arial\"/>", fixed = TRUE)
})

test_that("single points with markers and positioned labels are written", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(k = c("a", "b", "c"), v = c(3, 4, 2)))
  d <- openxlsx2::wb_data(wb, sheet = "Data")
  ch <- ec("line")$add_series(name = v, data = d, label = k, marker = "circle")
  ch$series_data[[1]]$points <- list(list(idx = 1L, color = "FF0000", marker = list(symbol = "square", size = 9L, fill = "00FF00")))
  ch$series_data[[1]]$point_labels <- list(list(idx = 1L, show_val = TRUE, pos = "t", format = "0.0", fill = "FFFF00", style = list(bold = TRUE)))
  xml <- ch$render()
  expect_match(xml, '<c:dPt><c:idx val="1"/><c:bubble3D val="0"/><c:marker><c:symbol val="square"/><c:size val="9"/><c:spPr><a:solidFill><a:srgbClr val="00FF00"/>', fixed = TRUE)
  expect_match(xml, '<c:dLbl><c:idx val="1"/><c:numFmt formatCode="0.0" sourceLinked="0"/><c:spPr><a:solidFill><a:srgbClr val="FFFF00"/></a:solidFill></c:spPr><c:txPr>', fixed = TRUE)
  expect_match(xml, '<c:dLblPos val="t"/>', fixed = TRUE)
  bar <- ec("bar")$add_series(name = v, data = d, label = k)
  bar$series_data[[1]]$point_labels <- list(list(idx = 0L, show_val = TRUE, pos = "t"), list(idx = 2L, show_val = TRUE, pos = "b"))
  xml <- bar$render()
  expect_match(xml, '<c:dLblPos val="outEnd"/>', fixed = TRUE)
  expect_match(xml, '<c:dLblPos val="inBase"/>', fixed = TRUE)
  back <- encharter:::load_chart(xml)
  expect_equal(back$series_data[[1]]$point_labels[[1]]$pos, "t")
  expect_equal(back$series_data[[1]]$point_labels[[2]]$pos, "b")
  expect_error(encharter:::load_chart("<c:chartSpace xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\"><c:chart/></c:chartSpace>"), "plot area")
  expect_error(encharter:::load_chart("<c:chartSpace xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\"><c:chart><c:plotArea/></c:chart></c:chartSpace>"), "chart type")
})

test_that("literal x values, referenced bubble sizes and palette colors are written", {
  ch <- ec("scatter")$add_series(name = "S", data = "Data!$B$2:$B$4")
  ch$series_data[[1]]$cat_cache <- c("a", "b", "c")
  expect_match(ch$render(), "<c:xVal><c:strLit>", fixed = TRUE)
  ch$series_data[[1]]$cat_cache <- c(1, 2, 3)
  expect_match(ch$render(), "<c:xVal><c:numLit>", fixed = TRUE)
  bub <- ec("bubble")$add_series(name = "S", data = "Data!$B$2:$B$4", label = "Data!$A$2:$A$4", weight = "Data!$C$2:$C$4")
  expect_match(bub$render(), "<c:bubbleSize><c:numRef><c:f>'Data'!$C$2:$C$4</c:f>", fixed = TRUE)
  ch <- ec("bar")$add_series(name = "S", data = "Data!$B$2:$B$4", color = NULL)$add_series(name = "T", data = "Data!$C$2:$C$4", color = NULL)
  expect_equal(ch$series_data[[2]]$line$color, ch$palette[2])
  ch <- ec("bar")$add_series(name = "S", data = "Data!$B$2:$B$4")$set_x_axis(crosses = "max", crosses_at = NULL)$set_y_axis(crosses_at = 2)
  xml <- ch$render()
  expect_match(xml, '<c:crossesAt val="2"/>', fixed = TRUE)
  expect_match(xml, '<c:crosses val="max"/>', fixed = TRUE)
})
