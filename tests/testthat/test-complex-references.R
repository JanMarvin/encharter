test_that("Chart: Multi-level labels and Bubbles", {
  # Multi-level Category (A2:B5 covers two columns)
  chart <- Chart$new()
  expect_error(chart$add_series(name = "A1", data = "C2:C5"), "Series data must be a sheet reference")
  chart$add_series(name = "A1", data = "dat!C2:C5", label = "dat!A2:B5")

  xml <- as.character(chart$render())
  expect_match(xml, "<c:multiLvlStrRef>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("dat")$add_data(x = mtcars)$
    add_encharter(graph = chart)

  # Bubble Chart with Z-Data
  bb <- Chart$new("bubbleChart")
  bb$add_series(name = "H", data = "dat!B2:B5", label = "dat!A2:A5", weight = "dat!C2:C5")

  xml_bb <- as.character(bb$render())
  expect_match(xml_bb, "<c:bubbleSize>")
  expect_match(xml_bb, "<c:bubbleChart>")
  wb$add_chart_xml(xml = bb$render())
})

test_that("Chart: Doughnut Hole Size", {
  dn <- Chart$new("doughnutChart")
  dn$set_pie_options(hole_size = 75)
  dn$add_series(name = "H", data = "dat!B2:B5")

  expect_match(as.character(dn$render()), "<c:holeSize val=\"75\"/>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("dat")$add_data(x = mtcars)$
    add_chart_xml(xml = dn$render())
})
