test_that("ChartEx: Waterfall and Subtotals", {
  ce <- ChartEx$new()
  ce$add_series(header = "Data!$B$1", data = "Data!$B$2:$B$10", type = "waterfall", subtotals = c(0, 8))

  xml <- as.character(ce$render(1))
  expect_match(xml, "waterfall")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = mtcars)
  wb <- wb_add_chartx(wb, chart_obj = ce)
  # expect_snapshot(clean_xml(xml))
})

test_that("ChartEx: Treemap and Sunburst", {
  # Histogram
  hs <- ChartEx$new()
  ## TODO let me use histogram
  hs$add_series(header = NULL, data = "dat!B2:B10", type = "clusteredColumn")
  expect_match(as.character(hs$render(1)), "clusteredColumn")

  wb <- openxlsx2::wb_workbook()$add_worksheet("dat")$add_data(x = mtcars)
  wb <- wb_add_chartx(wb, chart_obj = hs)

  hs$
    add_series("Dat!$B$1", "Dat!$B$2:$B$10", type = "paretoLine")$
    set_y_axis(gridlines = "dashed")
  wb <- wb_add_chartx(wb, chart_obj = hs)

  # Sunburst
  sb <- ChartEx$new()
  sb$add_series(header = "V", data = "dat!C2:C10", cat = "dat!A2:B10", type = "sunburst")
  ## TODO isnt there some white between the sunburst elements?
  expect_match(as.character(sb$render(1)), "sunburst")
  wb <- wb_add_chartx(wb, chart_obj = sb)
})
