library(testthat)
library(openxlsx2)

test_that("wb_data resolution and NSE support", {
  wb <- wb_workbook() |>
    wb_add_worksheet("DataSheet") |>
    wb_add_data(x = data.frame(Revenue = 10:15, Month = 1:6))

  dat <- wb_data(wb, sheet = "DataSheet", col_names = TRUE)

  chart <- Chart$new("lineChart")

  # Test NSE (unquoted names)
  chart$add_series(data = dat, header = Revenue, cat = Month)

  expect_equal(chart$series_data[[1]]$header, "'DataSheet'!$A$1")
  expect_equal(chart$series_data[[1]]$data,   "'DataSheet'!$A$2:$A$7")
  expect_equal(chart$series_data[[1]]$cat,    "'DataSheet'!$B$2:$B$7")

  # Test helpful error message for typos
  expect_error(chart$add_series(data = dat, header = Revnue), "object 'Revnue' not found")

  wb$add_chart_xml(xml = chart$render())
})


test_that("wb_data resolution and NSE support", {
  wb <- wb_workbook() |>
    wb_add_worksheet("DataSheet") |>
    wb_add_data(x = data.frame(Revenue = 10:15, Month = 1:6), col_names = FALSE)

  dat <- wb_data(wb, sheet = "DataSheet", col_names = FALSE)

  chart <- Chart$new("lineChart")

  # Test NSE (unquoted names)
  chart$add_series(data = dat, header = A)

  expect_equal(chart$series_data[[1]]$header, NULL)
  expect_equal(chart$series_data[[1]]$data,   "'DataSheet'!$A$1:$A$6")
  expect_equal(chart$series_data[[1]]$cat,    NULL)

  # Test helpful error message for typos
  expect_error(chart$add_series(data = dat, header = Revnue), "object 'Revnue' not found")

  wb$add_chart_xml(xml = chart$render())
})

test_that("ChartEx handles wb_data", {
  wb <- wb_workbook() |>
    wb_add_worksheet("WF") |>
    wb_add_data(x = data.frame(Label = c("A", "B"), Val = c(10, 20)))

  dat <- wb_data(wb, sheet = "WF")
  ce <- ChartEx$new()
  ce$add_series(data = dat, header = Val, cat = Label, type = "waterfall")

  expect_equal(ce$series_data[[1]]$header, "'WF'!$B$1")

  wb <- wb_add_chartx(wb, chart_obj = ce)
})
