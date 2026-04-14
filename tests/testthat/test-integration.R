test_that("Integration: wb_add_chart and wb_add_chart", {
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet() |>
    openxlsx2::wb_add_data(x = head(cars))
  chart <- Chart$new()
  chart$add_series(name = "'Sheet 1'!$A$1", data = "'Sheet 1'!$A$2:$A$5", type = "barChart")

  # Test standard chart addition
  expect_no_error(wb <- openxlsx2::wb_add_encharter(wb, graph = chart, dims = "E2:M20"))

  # Test ChartEx addition (which involves more complex rels management)
  ce <- ChartEx$new()
  ce$add_series(name = "'Sheet 1'!B1", data = "'Sheet 1'!B2:B5", type = "waterfall")
  expect_no_error(wb <- openxlsx2::wb_add_encharter(wb, graph = ce, dims = "A10:G20"))

  # Verify drawing exists in workbook internal structure
  expect_true(length(wb$drawings) > 0)
})
