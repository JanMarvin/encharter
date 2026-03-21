test_that("Chart: Date Axes and Axis Units", {
  chart <- Chart$new("lineChart")
  chart$add_series(
    header = "S1!$B$1", data = "S1!$B$2:$B$5"
  )

  # Date axis configuration
  chart$set_x_axis(
    major = 2, major_time = "months",
    minor = 1, minor_time = "months",
    format = "mmm-yy", label_pos = "low",
    gridlines = "dotted", grid_color = "#707070", # test grid lines
    minor_gridlines = "dotted", minor_grid_color = "#707070", # test grid lines
  )

  chart$set_x_title("Foo", sz = 14, bold = TRUE)
  expect_warning(chart$set_x2_title("Bar", sz = 14, bold = TRUE), "Secondary axis title ignored.")
  chart$set_y_title("Baz", sz = 14, bold = TRUE)
  expect_warning(chart$set_y2_title("Bam", sz = 14, bold = TRUE), "Secondary axis title ignored.")

  xml <- as.character(chart$render())

  expect_match(xml, "<c:majorUnit val=\"2\"/>")
  expect_match(xml, "<c:majorTimeUnit val=\"months\"/>")
  expect_match(xml, "<c:numFmt formatCode=\"mmm-yy\"")
  expect_match(xml, "<c:lblOffset val=\"100\"/>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)
  wb <- wb_add_encharter(wb, dims = "A2:K10", graph = chart)

  chart$add_series(
    header = "S1!C$1", data = "S1!$C$2:$C$5", secondary = TRUE # "y"
  )
  chart$set_y2_title("Bam", sz = 14, bold = TRUE)
  xml <- as.character(chart$render())

  chart$add_series(
    header = "S1!C$1", data = "S1!$C$2:$C$5", secondary = "x"
  )
  chart$set_x2_title("Bar", sz = 14, bold = TRUE)
  xml <- as.character(chart$render())

  wb <- openxlsx2::wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)
  wb <- wb_add_encharter(wb, dims = "A2:K10", graph = chart)
})

test_that("Chart: Combo charts and Secondary Axis", {
  chart <- Chart$new("barChart")
  # Primary
  chart$add_series(header = "B1", data = "dat!B2:B5", color = openxlsx2::wb_color("magenta"))
  # Secondary Area
  chart$add_series(header = "C1", data = "dat!C2:C5", type = "areaChart", secondary = TRUE)

  xml <- as.character(chart$render())

  # Check for two plot areas (primary and secondary)
  expect_match(xml, "<c:barChart>")
  expect_match(xml, "<c:areaChart>")
  # Check for secondary axis ID presence
  expect_match(xml, "<c:valAx>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("dat")$add_data(x = mtcars)$
    add_chart_xml(xml = xml)
})
