# Helper to remove random IDs for stable snapshots
clean_xml <- function(xml) {
  xml_str <- as.character(xml)
  gsub("val=\"[0-9]{5,10}\"", "val=\"12345\"", xml_str)
}

test_that("Chart: Combo Bar/Area and Secondary Axis", {
  chart <- Chart$new()
  chart$add_series(name = "Sheet1!$B$1", data = "Sheet1!$B$2:$B$6", type = "barChart")
  chart$add_series(name = "Sheet1!$D$1", data = "Sheet1!$D$2:$D$6", type = "areaChart", secondary = TRUE)

  chart$set_y_title("Primary")
  chart$set_y2_title("Secondary")

  res <- chart$render()
  expect_true(any(grepl("areaChart", as.character(res))))
  expect_true(any(grepl("barChart", as.character(res))))
  # expect_snapshot(clean_xml(res))
  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$add_encharter(graph = chart)
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
  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$add_encharter(graph = chart)
})

test_that("Chart: Bubble and Doughnut specific features", {
  # Doughnut hole size
  dn <- Chart$new("doughnutChart")
  dn$add_series(
    label = "Sheet1!$A$2:$A$6",
    data = "Sheet1!$B$2:$B$6"
  )
  dn$set_pie_options(hole_size = 65, rotation = 90, expansion = 40)
  expect_match(as.character(dn$render()), "holeSize val=\"65\"")
  expect_match(as.character(dn$render()), "firstSliceAng val=\"90\"")
  expect_match(as.character(dn$render()), "explosion val=\"40\"")

  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$
    add_data(x = head(mtcars[1:3]), row_names = TRUE)$
    add_encharter(graph = dn)

  # Bubble weight
  bb <- Chart$new("bubbleChart")
  bb$add_series(name = "H", label = "Sheet1!$A$1:$A$5",
                data = "Sheet1!$B$1:$B$5", weight = "Sheet1!$C$1:$C$5")
  expect_match(as.character(bb$render()), "bubbleSize")
  wb$add_encharter(graph = bb)
})

test_that("Chart: Multi-level Category Grouping", {
  chart <- Chart$new()
  # Testing a range covering two columns for categories
  chart$add_series(name = "Val", data = "Sheet1!$C$2:$C$5", label = "Sheet1!$A$2:$B$5")

  xml <- as.character(chart$render())
  expect_match(xml, "multiLvlStrRef")

  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$
    add_data(x = head(mtcars[1:3]), row_names = TRUE)$
    add_encharter(graph = chart)
})

test_that("Chart series supports Trendlines and Error Bars with correct XSD sequence", {

  ch <- Chart$new()

  ch$add_series(
    name = "Monthly Revenue",
    data = "Sheet1!$B$2:$B$7",
    label = "Sheet1!$A$2:$A$7",
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

  tl_node <- xml_find_first(read_xml(xml), "//c:ser/c:trendline")
  expect_false(is.null(tl_node))
  expect_equal(xml_attr(xml_find_first(tl_node, "c:trendlineType"), "val"), "linear")
  expect_equal(xml_attr(xml_find_first(tl_node, "c:dispRSqr"), "val"), "0")

  eb_node <- xml_find_first(read_xml(xml), "//c:ser/c:errBars")
  expect_false(is.null(eb_node))
  expect_equal(xml_attr(xml_find_first(eb_node, "c:errDir"), "val"), "y")
  expect_equal(xml_attr(xml_find_first(eb_node, "c:errValType"), "val"), "percentage")
  expect_equal(xml_attr(xml_find_first(eb_node, "c:val"), "val"), "10")

  ser_children <- xml_name(xml_children(xml_find_first(read_xml(xml), "//c:ser")))

  idx_trendline <- which(ser_children == "c:trendline")
  idx_errbars   <- which(ser_children == "c:errBars")
  idx_cat       <- which(ser_children == "c:cat")
  idx_val       <- which(ser_children == "c:val")

  expect_true(idx_trendline < idx_errbars)

  expect_true(idx_errbars < idx_cat)
  expect_true(idx_cat < idx_val)
})

test_that("surfaceChart rendering works", {
  # 1. Minimal setup
  surface_data <- data.frame(
    Y = paste0("Y", 1:3),
    X1 = c(10, 20, 10),
    X2 = c(20, 40, 20),
    X3 = c(10, 20, 10)
  )

  # 2. Initialize Chart and add series
  chart <- Chart$new()
  for (i in 2:4) {
    chart$add_series(
      name = paste0("Sheet1!$A$", i),
      data   = paste0("Sheet1!$B$", i, ":$D$", i),
      label  = "Sheet1!$B$1:$D$1",
      type   = "surfaceChart"
    )
  }

  # 3. Build workbook and get XML
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Sheet1") |>
    openxlsx2::wb_add_data(x = surface_data) |>
    openxlsx2::wb_add_encharter(graph = chart)

  xml_content <- read_xml(chart$render())

  # 4. Assertions
  expect_s3_class(chart, "Chart")
  # Verify the specific OOXML tag for surface charts exists
  expect_true(any(grepl("surfaceChart", as.character(xml_content))))
  # Verify we have the correct number of series nodes
  expect_equal(length(xml_find_all(xml_content, ".//c:ser")), 3)
})

test_that("radarChart rendering works for both standard and filled styles", {
  # 1. Minimal setup
  skill_data <- data.frame(
    Metric = c("A", "B", "C"),
    Val = c(10, 20, 30)
  )

  # 2. Test Standard Radar (Line)
  radar_std <- Chart$new()
  radar_std$add_series(
    name = "Std",
    label  = "Sheet1!$A$2:$A$4",
    data   = "Sheet1!$B$2:$B$4",
    type   = "radarChart"
  )

  xml_std <- read_xml(radar_std$render())

  # Assert standard radarStyle is "radar" (default line)
  expect_true(any(grepl('radarStyle val="standard"', as.character(xml_std))))

  # 3. Test Filled Radar
  radar_filled <- Chart$new()
  radar_filled$add_series(
    name = "Fill",
    label  = "Sheet1!$A$2:$A$4",
    data   = "Sheet1!$B$2:$B$4",
    type   = "radarChart",
    filled = TRUE
  )

  xml_filled <- read_xml(radar_filled$render())

  # Assert filled radarStyle is "filled"
  expect_true(any(grepl('radarStyle val="filled"', as.character(xml_filled))))

  # 4. General structure check
  expect_s3_class(radar_std, "Chart")
  expect_equal(length(xml_find_all(xml_std, ".//c:radarChart")), 1)
})

test_that("scatter without label is supported", {

  df_scatter <- data.frame(
    Project = paste("Project", LETTERS[1:5]),
    Risk    = c(10, 40, 30, 70, 90),
    ROI     = c(20, 50, 80, 40, 60)
  )

  sc_chart <- ec("scatterChart")
  sc_chart$set_chart_title("Risk vs ROI Analysis")

  sc_chart$add_series(
    name      = "Portfolio",
    data      = "ScatterData!$C$2:$C$6",
    show_line = FALSE,
    marker    = "circle",
    marker_size = 8
  )

  xml <- sc_chart$render()
  expect_true(!any(grepl("c:xVal", as.character(xml))))

  wb <- openxlsx2::wb_workbook()$
    add_worksheet(sheet = "ScatterData")$
    add_data(sheet = "ScatterData", x = df_scatter)$
    add_encharter(sheet = "ScatterData", graph = sc_chart)

})
