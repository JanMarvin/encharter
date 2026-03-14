library(testthat)
library(openxlsx2)

test_that("Chart: Styling, Markers, and Labels", {
  chart <- Chart$new("lineChart")

  # Test Data Label Style (from Line.R)
  chart$set_data_label_style(
    show_val = TRUE, show_cat = FALSE, pos = "t", bold = TRUE, font_size = 9
  )

  # Test Marker Styling
  chart$add_series(
    header = "S1!$B$1", data = "S1!$B$2:$B$5",
    marker = "circle", marker_size = 7, marker_fill = "#FFFFFF"
  )

  xml <- as.character(chart$render())

  expect_match(xml, "<c:showVal val=\"1\"/>")
  expect_match(xml, "<c:dLblPos val=\"t\"/>")
  expect_match(xml, "<c:marker>")
  expect_match(xml, "<c:symbol val=\"circle\"/>")
  expect_match(xml, "<c:size val=\"7\"/>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)$
    add_chart_xml(xml = xml)
})

test_that("Chart: Legend and Title styles", {
  chart <- Chart$new()
  chart$set_chart_title("Main Title", bold = TRUE, sz = 14)
  chart$set_legend_style(pos = "b", font_size = 10)
  expect_error(chart$render(), "The chart contains no data. You must add at least one series")

  chart$add_series(
    header = "S1!$B$1", data = "S1!$B$2:$B$5",
    marker = "circle", marker_size = 7, marker_fill = "#FFFFFF"
  )

  xml <- as.character(chart$render())
  expect_match(xml, "<c:title>")
  expect_match(xml, "Main Title")
  expect_match(xml, "<c:legendPos val=\"b\"/>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)$
    add_chart_xml(xml = xml)
})

test_that("Chart and Plot area styling works", {
  # 1. Create a styled chart
  chart <- Chart$new("lineChart")
  chart$add_series(header = "S1!$B$1", data = "S1!$B$2:$B$5")

  # Set Chart Background and Border (ChartSpace)
  chart$set_chart_style(
    fill = "EDF2F7",   # Light grey
    line = "2D3748",   # Dark border
    line_width = 2.25  # Should result in 28575 EMUs
  )

  # Set Plot Area Background (PlotArea)
  chart$set_plot_style(
    fill = "FFEE00",   # Yellow plot area
    line = "000000",   # Black plot border
    line_width = 1     # Should result in 12700 EMUs
  )

  # Render XML
  xml_str <- as.character(chart$render())
  xml <- xml2::read_xml(xml_str)

  # 2. Test ChartSpace Styling (Root level spPr)
  # Usually the last spPr child of chartSpace
  chart_sp_pr <- xml2::xml_find_first(xml, "/c:chartSpace/c:spPr")
  expect_false(is.null(chart_sp_pr))

  # Check Chart Fill
  expect_match(as.character(chart_sp_pr), 'val="EDF2F7"')

  # Check Chart Line Width (2.25 * 12700 = 28575)
  chart_ln <- xml2::xml_find_first(chart_sp_pr, "a:ln")
  expect_equal(xml2::xml_attr(chart_ln, "w"), "28575")

  # 3. Test PlotArea Styling
  plot_sp_pr <- xml2::xml_find_first(xml, "//c:plotArea/c:spPr")
  expect_false(is.null(plot_sp_pr))

  # Check Plot Fill
  expect_match(as.character(plot_sp_pr), 'val="FFEE00"')

  # Check Plot Line Width (1 * 12700 = 12700)
  plot_ln <- xml2::xml_find_first(plot_sp_pr, "a:ln")
  expect_equal(xml2::xml_attr(plot_ln, "w"), "12700")
})

test_that("ChartEx chart and plot styling works", {
  # 1. Create a styled ChartEx (treemap)
  ce <- ChartEx$new()
  ce$add_series(
    data = "Sheet1!$B$2:$B$5",
    cat  = "Sheet1!$A$2:$A$5",
    type = "treemap"
  )

  # Set Background (ChartSpace equivalent)
  ce$set_chart_style(
    fill = "F0F0F0",
    line = "FF0000",
    line_width = 2
  )

  # Set Plot Area Background (The area inside the chart)
  ce$set_plot_style(
    fill = "CCFFCC", # Light green
    line = "0000FF", # Blue border
    line_width = 1
  )

  # Render XML
  xml_str <- as.character(ce$render(1))
  xml <- xml2::read_xml(xml_str)

  wb <- wb_workbook()$add_worksheet("Sheet1")$add_data(x = mtcars)
  wb <- wb_add_chartx(wb, sheet = "Sheet1", dims = "A2:G12", chart_obj = ce)

  # 2. Test Chart Styling (cx:chart/cx:spPr)
  chart_sp_pr <- xml2::xml_find_first(xml, "cx:spPr")
  expect_false(is.null(chart_sp_pr))
  expect_match(as.character(chart_sp_pr), 'val="F0F0F0"')

  # 3. Test Plot Area Styling (cx:plotArea/cx:spPr)
  plot_sp_pr <- xml2::xml_find_first(xml, "//cx:chart/cx:plotArea/cx:plotAreaRegion/cx:plotSurface/cx:spPr")
  expect_false(is.null(plot_sp_pr))
  expect_match(as.character(plot_sp_pr), 'val="CCFFCC"')

  # Check Plot Line Width (1 * 12700 = 12700)
  plot_ln <- xml2::xml_find_first(plot_sp_pr, "a:ln")
  expect_equal(xml2::xml_attr(plot_ln, "w"), "12700")
})
