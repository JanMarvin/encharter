library(testthat)
library(openxlsx2)

# Helper to remove random IDs for stable snapshots
clean_xml <- function(xml) {
  xml_str <- as.character(xml)
  gsub("val=\"[0-9]{5,10}\"", "val=\"12345\"", xml_str)
}

test_that("Chart: Combo Bar/Area and Secondary Axis", {
  chart <- Chart$new()
  chart$add_series(header = "Sheet1!$B$1", data = "Sheet1!$B$2:$B$6", type = "barChart")
  chart$add_series(header = "Sheet1!$D$1", data = "Sheet1!$D$2:$D$6", type = "areaChart", secondary = TRUE)

  chart$set_y_title("Primary")
  chart$set_y2_title("Secondary")

  res <- chart$render()
  expect_true(any(grepl("areaChart", as.character(res))))
  expect_true(any(grepl("barChart", as.character(res))))
  # expect_snapshot(clean_xml(res))
  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$add_chart_xml(xml = res)
})

test_that("Chart: Date Axes and Major/Minor Units", {
  chart <- Chart$new("lineChart")
  chart$add_series(
    data = "Sheet1!$B$2:$B$6"
  )
  chart$set_x_axis(
    major      = 2,
    major_time = "months",
    minor      = 1,
    minor_time = "months",
    format     = "mmm-yy"
  )

  xml <- chart$render()
  expect_match(xml, "majorUnit")
  expect_match(xml, "minorUnit")
  expect_match(xml, "numFmt")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$add_chart_xml(xml = xml)
})

test_that("Chart: Bubble and Doughnut specific features", {
  # Doughnut hole size
  dn <- Chart$new("doughnutChart")
  dn$add_series(
    data = "Sheet1!$A$2:$A$6"
  )
  dn$set_hole_size(65)
  expect_match(as.character(dn$render()), "holeSize val=\"65\"")

  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$
    add_data(x = head(mtcars[1:3]), row_names = TRUE)$
    add_chart_xml(xml = dn$render())

  # Bubble z_data
  bb <- Chart$new("bubbleChart")
  bb$add_series(header = "H", cat = "Sheet1!$A$1:$A$5",
                data = "Sheet1!$B$1:$B$5", z_data = "Sheet1!$C$1:$C$5")
  expect_match(as.character(bb$render()), "bubbleSize")
  wb$add_chart_xml(xml = bb$render())
})

test_that("Chart: Multi-level Category Grouping", {
  chart <- Chart$new()
  # Testing a range covering two columns for categories
  chart$add_series(header = "Val", data = "Sheet1!$C$2:$C$5", cat = "Sheet1!$A$2:$B$5")

  xml <- as.character(chart$render())
  expect_match(xml, "multiLvlStrRef")

  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$
    add_data(x = head(mtcars[1:3]), row_names = TRUE)$
    add_chart_xml(xml = xml)
})

test_that("Chart series supports Trendlines and Error Bars with correct XSD sequence", {

  ch <- Chart$new()

  ch$add_series(
    header = "Monthly Revenue",
    data = "Sheet1!$B$2:$B$7",
    cat = "Sheet1!$A$2:$A$7",
    type = "barChart",
    error_bars = list(
      type = "percentage",
      value = 10,
      color = "404040"
    ),
    trendline = list(
      type = "linear",
      color = "FF0000",
      show_r2 = FALSE
    )
  )

  xml <- ch$render()

  tl_node <- xml2::xml_find_first(xml2::read_xml(xml), "//c:ser/c:trendline")
  expect_false(is.null(tl_node))
  expect_equal(xml2::xml_attr(xml2::xml_find_first(tl_node, "c:trendlineType"), "val"), "linear")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(tl_node, "c:dispRSqr"), "val"), "0")

  eb_node <- xml2::xml_find_first(xml2::read_xml(xml), "//c:ser/c:errBars")
  expect_false(is.null(eb_node))
  expect_equal(xml2::xml_attr(xml2::xml_find_first(eb_node, "c:errDir"), "val"), "y")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(eb_node, "c:errValType"), "val"), "percentage")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(eb_node, "c:val"), "val"), "10")

  ser_children <- xml2::xml_name(xml2::xml_children(xml2::xml_find_first(xml2::read_xml(xml), "//c:ser")))

  idx_trendline <- which(ser_children == "trendline")
  idx_errbars   <- which(ser_children == "errBars")
  idx_cat       <- which(ser_children == "cat")
  idx_val       <- which(ser_children == "val")

  expect_true(idx_trendline < idx_errbars)

  expect_true(idx_errbars < idx_cat)
  expect_true(idx_cat < idx_val)
})
