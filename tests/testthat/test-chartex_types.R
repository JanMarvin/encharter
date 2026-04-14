
test_that("ChartEx: Specialized types and subtotals", {
  # Waterfall with subtotals
  wf <- ChartEx$new()
  expect_error(wf$add_series(name = "H", data = "B2:B10", type = "waterfall", subtotals = TRUE), "Series data must be")

  wf$add_series(name = "H", data = "dat!B2:B10", type = "waterfall", subtotals = TRUE)

  wb <- openxlsx2::wb_workbook()$add_worksheet("dat")$add_data(x = mtcars)
  wb <- openxlsx2::wb_add_encharter(wb, graph = wf)

  # Treemap
  tm <- ChartEx$new()
  tm$add_series(name = "H", data = "dat!B2:B10", label = "dat!A2:A10", type = "treemap")
  wb <- openxlsx2::wb_add_encharter(wb, graph = tm)

  # Region Map
  rm <- ChartEx$new()
  rm$add_series(name = "dat!B1", data = "dat!B2:B10", label = "dat!A2:A10", type = "regionMap")
  wb <- openxlsx2::wb_workbook()$add_worksheet("dat")$add_data(x = USArrests, row_names = TRUE)
  wb <- openxlsx2::wb_add_encharter(wb, graph = rm)

  expect_match(as.character(wf$render(id_start = 1)), "waterfall")
  expect_match(as.character(tm$render(1)), "treemap")
  expect_match(as.character(rm$render(1)), "regionMap")
})

test_that("ChartEx: BoxWhisker and Funnel", {
  bw <- ChartEx$new()
  bw$add_series(name = "H", data = "dat!B2:B10", type = "boxWhisker")

  fn <- ChartEx$new()
  fn$add_series(name = "H", data = "dat!B2:B10", type = "funnel")

  expect_match(as.character(bw$render(1)), "boxWhisker")
  wb <- openxlsx2::wb_workbook()$add_worksheet("dat")$add_data(x = mtcars)
  wb <- openxlsx2::wb_add_encharter(wb, graph = bw)

  expect_match(as.character(fn$render(1)), "funnel")
  wb <- openxlsx2::wb_add_encharter(wb, graph = fn)
})
