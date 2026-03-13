test_that("Chart supports unquoted column names (NSE)", {
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Sheet 1") |>
    openxlsx2::wb_add_data(x = mtcars[1:3, 1:2])

  dat <- openxlsx2::wb_data(wb, 1, dims = "A1:B4")

  chart <- Chart$new("lineChart")

  # Note: No quotation marks used for mpg or cyl
  chart$add_series(data = dat, header = mpg, cat = cyl)

  # Verify resolution
  expect_equal(chart$series_data[[1]]$header, "'Sheet 1'!A1")
  expect_equal(chart$series_data[[1]]$cat,    "'Sheet 1'!B2:B4")

  # Verify standard string input still works (Backward compatibility)
  chart$add_series(data = dat, header = "cyl", cat = "mpg")
  expect_equal(chart$series_data[[2]]$header, "'Sheet 1'!B1")


  expect_error(chart$add_series(data = dat, header = mpg, cat = foo), "object 'foo' not found")

  wb$add_chart_xml(xml = chart$render())
})

test_that("ChartEx handles unquoted names for Waterfall", {
  df <- data.frame(Category = c("A", "B"), Value = c(10, 20))
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet() |>
    openxlsx2::wb_add_data(x = df)
  dat <- openxlsx2::wb_data(wb)

  chart <- ChartEx$new()
  # Unquoted names
  chart$add_series(data = dat, header = Value, cat = Category, type = "waterfall")

  expect_equal(chart$series_data[[1]]$header, "'Sheet 1'!B1")
  expect_equal(chart$series_data[[1]]$cat,    "'Sheet 1'!A2:A3")

  expect_error(chart$add_series(data = dat, header = Value, cat = foo), "object 'foo' not found")

  wb <- wb_add_chartx(wb, chart_obj = chart)
})
