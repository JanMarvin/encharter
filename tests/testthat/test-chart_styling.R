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
