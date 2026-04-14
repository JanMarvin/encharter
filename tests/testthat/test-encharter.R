test_that("Chart supports unquoted column names (NSE)", {
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Sheet 1") |>
    openxlsx2::wb_add_data(x = mtcars[1:3, 1:2])

  dat <- openxlsx2::wb_data(wb, 1, dims = "A1:B4")

  chart <- ec("lineChart")

  # Note: No quotation marks used for mpg or cyl
  chart$add_series(data = dat, name = mpg, label = cyl)

  # Verify resolution
  expect_equal(chart$series_data[[1]]$name, "'Sheet 1'!$A$1")
  expect_equal(chart$series_data[[1]]$label,    "'Sheet 1'!$B$2:$B$4")

  # Verify standard string input still works (Backward compatibility)
  chart$add_series(data = dat, name = "cyl", label = "mpg")
  expect_equal(chart$series_data[[2]]$name, "'Sheet 1'!$B$1")


  expect_error(chart$add_series(data = dat, name = mpg, label = foo), "object 'foo' not found")

  wb$add_chart_xml(xml = chart$render())
})

test_that("ChartEx handles unquoted names for Waterfall", {
  df <- data.frame(Category = c("A", "B"), Value = c(10, 20))
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet() |>
    openxlsx2::wb_add_data(x = df)
  dat <- openxlsx2::wb_data(wb)

  chart <- ec("waterfall")
  # Unquoted names
  chart$add_series(data = dat, name = Value, label = Category, type = "waterfall")

  expect_equal(chart$series_data[[1]]$name, "'Sheet 1'!$B$1")
  expect_equal(chart$series_data[[1]]$label,    "'Sheet 1'!$A$2:$A$3")

  expect_error(chart$add_series(data = dat, name = Value, label = foo), "object 'foo' not found")

  wb <- openxlsx2::wb_add_encharter(wb, graph = chart)
})

test_that("validates correctly", {
  expect_error(chart <- ec("waterFall"), "should be one of")
})

test_that("print works", {
  combo_chart <- ec("barplot")$
    set_chart_title("Sales Volume vs Growth", bold = TRUE)$
    add_series(
      name = "Standard!$B$1", data = "Standard!$B$2:$B$13",
      label = "Standard!$A$2:$A$13", color = "4472C4"
    )$
    add_series(
      name = "Standard!$D$1", data = "Standard!$D$2:$D$13",
      type = "lineChart", secondary = TRUE, color = "C0504D", marker = "circle"
    )$
    set_legend_style(pos = "bottom")

  output <- capture.output(print(combo_chart))

  expect_match(output[1], "An encharter object")
  expect_match(output[2], "Number of Series: 2")

  expect_true(any(grepl("Series 2: 'Standard'!\\$D\\$1  \\[Secondary Axis\\]", output)))

  expect_true(any(grepl("Type: barChart", output)))
  expect_true(any(grepl("'Standard'!\\$B\\$2", output)))
})
