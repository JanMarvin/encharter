test_that("Chart: Styling, Markers, and Labels", {
  chart <- Chart$new("lineChart")

  # Test Data Label Style (from Line.R)
  chart$set_data_label_style(
    show_val = TRUE, show_cat = FALSE, pos = "t", bold = TRUE, font_size = 9
  )

  # Test Marker Styling
  chart$add_series(
    name = "S1!$B$1", data = "S1!$B$2:$B$5",
    marker = "circle", marker_size = 7, marker_fill = "#FFFFFF"
  )

  xml <- as.character(chart$render())

  expect_match(xml, "<c:showVal val=\"1\"/>")
  expect_match(xml, "<c:dLblPos val=\"t\"/>")
  expect_match(xml, "<c:marker>")
  expect_match(xml, "<c:symbol val=\"circle\"/>")
  expect_match(xml, "<c:size val=\"7\"/>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)$
    add_encharter(graph = chart)
})

test_that("Chart: Legend and Title styles", {
  chart <- Chart$new()
  chart$set_chart_title("Main Title", bold = TRUE, font_size = 14)
  chart$set_legend_style(pos = "b", font_size = 10)
  expect_error(chart$render(), "The chart contains no data. You must add at least one series")

  chart$add_series(
    name = "S1!$B$1", data = "S1!$B$2:$B$5",
    marker = "circle", marker_size = 7, marker_fill = "#FFFFFF"
  )

  xml <- as.character(chart$render())
  expect_match(xml, "<c:title>")
  expect_match(xml, "Main Title")
  expect_match(xml, "<c:legendPos val=\"b\"/>")

  wb <- openxlsx2::wb_workbook()$add_worksheet("S1")$add_data(x = mtcars)$
    add_encharter(graph = chart)
})

test_that("Chart and Plot area styling works", {
  # 1. Create a styled chart
  chart <- Chart$new("lineChart")
  chart$add_series(name = "S1!$B$1", data = "S1!$B$2:$B$5")

  # Set Chart Background and Border (ChartSpace)
  chart$set_chart_style(
    fill = "EDF2F7",   # Light grey
    line = "2D3748",   # Dark border
    line_width = 2.25  # Should result in 28575 EMUs
  )

  # Set Plot Area Background (PlotArea)
  chart$set_plot_style(
    fill = "FFEE00",   # Yellow plot area
    line = "000000",   # Black plot border
    line_width = 1     # Should result in 12700 EMUs
  )

  # Render XML
  xml_str <- as.character(chart$render())
  xml <- read_xml(xml_str)

  # 2. Test ChartSpace Styling (Root level spPr)
  # Usually the last spPr child of chartSpace
  chart_sp_pr <- xml_find_first(xml, "/c:chartSpace/c:spPr")
  expect_false(is.null(chart_sp_pr))

  # Check Chart Fill
  expect_match(as.character(chart_sp_pr), 'val="EDF2F7"')

  # Check Chart Line Width (2.25 * 12700 = 28575)
  chart_ln <- xml_find_first(chart_sp_pr, "a:ln")
  expect_equal(xml_attr(chart_ln, "w"), "28575")

  # 3. Test PlotArea Styling
  plot_sp_pr <- xml_find_first(xml, "//c:plotArea/c:spPr")
  expect_false(is.null(plot_sp_pr))

  # Check Plot Fill
  expect_match(as.character(plot_sp_pr), 'val="FFEE00"')

  # Check Plot Line Width (1 * 12700 = 12700)
  plot_ln <- xml_find_first(plot_sp_pr, "a:ln")
  expect_equal(xml_attr(plot_ln, "w"), "12700")
})

test_that("ChartEx chart and plot styling works", {
  # 1. Create a styled ChartEx (treemap)
  ce <- ChartEx$new()
  ce$add_series(
    data = "Sheet1!$B$2:$B$5",
    label = "Sheet1!$A$2:$A$5",
    type = "treemap"
  )

  # Set Background (ChartSpace equivalent)
  ce$set_chart_style(
    fill = "F0F0F0",
    line = "FF0000",
    line_width = 2
  )

  # Set Plot Area Background (The area inside the chart)
  ce$set_plot_style(
    fill = "CCFFCC", # Light green
    line = "0000FF", # Blue border
    line_width = 1
  )

  # Render XML
  xml_str <- as.character(ce$render(1))
  xml <- read_xml(xml_str)

  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$add_data(x = mtcars)
  wb <- openxlsx2::wb_add_encharter(wb, sheet = "Sheet1", dims = "A2:G12", graph = ce)

  # 2. Test Chart Styling (cx:chart/cx:spPr)
  # NOTE they behave different
  # with xml2::xml_find_first(xml2::read_xml(xml_str), "cx:spPr")
  # encharter::xml_find_first(encharter::read_xml(xml_str), "cx:spPr")
  chart_sp_pr <- xml_find_all(xml, "cx:spPr")[[2]]
  expect_false(is.null(chart_sp_pr))
  expect_match(as.character(chart_sp_pr), 'val="F0F0F0"')

  # 3. Test Plot Area Styling (cx:plotArea/cx:spPr)
  plot_sp_pr <- xml_find_first(xml, "//cx:chart/cx:plotArea/cx:plotAreaRegion/cx:plotSurface/cx:spPr")
  expect_false(is.null(plot_sp_pr))
  expect_match(as.character(plot_sp_pr), 'val="CCFFCC"')

  # Check Plot Line Width (1 * 12700 = 12700)
  plot_ln <- xml_find_first(plot_sp_pr, "a:ln")
  expect_equal(xml_attr(plot_ln, "w"), "12700")
})

test_that("Major and Minor grid_lines are correctly rendered and visible", {

  # 1. Create the dataset
  sales_data <- data.frame(
    Month = month.abb,
    Volume = c(1200, 1150, 1300, 1250, 1400, 1350, 1100, 1050, 1200, 1500, 1800, 2000),
    Sales = c(12000, 11500, 13000, 12500, 14000, 13500, 13200, 12600, 14400, 18000, 21600, 24000)
  )

  # 2. Setup the Chart Object
  my_chart <- Chart$new("barChart")

  my_chart$add_series(
    name = "Sheet1!$B$1",
    data   = "Sheet1!$B$2:$B$13",
    label  = "Sheet1!$A$2:$A$13",
    color  = "4472C4"
  )$set_y_axis(
    # Major: Dashed, 1.5pt, Dark Grey
    grid_lines        = "dash",
    grid_width       = 1.5,
    grid_color       = "333333",
    # Minor: Dotted, 0.5pt, Light Grey
    minor_grid_lines  = "dotted",
    minor_grid_width = 0.5,
    minor_grid_color = "D9D9D9"
  )

  # 3. XML Validation (Logic check)
  xml_str <- as.character(my_chart$render())
  xml <- read_xml(xml_str)

  # Check Major Gridlines
  major_ln <- xml_find_first(xml, "//c:valAx/c:majorGridlines/c:spPr/a:ln")
  expect_equal(xml_attr(major_ln, "w"), "19050") # 1.5 * 12700
  expect_equal(xml_attr(xml_find_first(major_ln, ".//a:srgbClr"), "val"), "333333")
  expect_equal(xml_attr(xml_find_first(major_ln, "a:prstDash"), "val"), "dash")

  # Check Minor Gridlines
  minor_ln <- xml_find_first(xml, "//c:valAx/c:minorGridlines/c:spPr/a:ln")
  expect_equal(xml_attr(minor_ln, "w"), "6350") # 0.5 * 12700
  expect_equal(xml_attr(xml_find_first(minor_ln, ".//a:srgbClr"), "val"), "D9D9D9")
  expect_equal(xml_attr(xml_find_first(minor_ln, "a:prstDash"), "val"), "dot")

  # 4. Visual Verification Workbook
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Sheet1") |>
    openxlsx2::wb_add_data(x = sales_data) |>
    openxlsx2::wb_add_encharter(dims = "E2:M25", graph = my_chart)
})

test_that("ChartEx renders full styling and axis properties", {
  ce <- ChartEx$new()

  # Apply comprehensive Y-axis styling
  ce$set_y_axis(
    min = 50,
    max = 500,
    major = 100,
    minor = 50,
    grid_lines = "dash",
    grid_color = "FF0000",
    color = "000000",      # Black axis line
    font_size = 12,        # 12pt font
    bold = TRUE,
    italic = TRUE,
    format = "#,##0"
  )

  # Apply Title styling
  ce$set_y_title("USD (Millions)", font_size = 14, bold = TRUE, font_color = "000000")

  xml <- read_xml(ce$render(1))

  # 1. Verify Scaling Attributes (cx:valScaling)
  scaling_node <- xml_find_first(xml, "//cx:axis[@id='1']/cx:valScaling")
  expect_equal(xml_attr(scaling_node, "min"), "50")
  expect_equal(xml_attr(scaling_node, "max"), "500")
  expect_equal(xml_attr(scaling_node, "majorUnit"), "100")
  expect_equal(xml_attr(scaling_node, "minorUnit"), "50")

  # 2. Verify Axis Title Styling (cx:rich)
  # Titles use a:rPr for styling
  title_rpr <- xml_find_first(xml, "//cx:axis[@id='1']/cx:title//a:rPr")
  expect_equal(xml_attr(title_rpr, "sz"), "1400")
  # expect_equal(xml_attr(title_rpr, "b"), "1")
  expect_true(xml_has_attr(xml_find_first(title_rpr, ".//a:srgbClr"), "val"))
  expect_equal(xml_attr(xml_find_first(title_rpr, ".//a:srgbClr"), "val"), "000000")

  # 3. Verify Axis Label Styling (txPr)
  # Check BOTH defRPr (start) and endParaRPr (the fix for washed-out colors)
  tx_pr <- xml_find_first(xml, "//cx:axis[@id='1']/cx:txPr")

  # Start Properties
  def_rpr <- xml_find_first(tx_pr, ".//a:defRPr")
  expect_equal(xml_attr(def_rpr, "sz"), "1200")
  expect_equal(xml_attr(def_rpr, "b"), "1")
  expect_equal(xml_attr(def_rpr, "i"), "1")
  expect_equal(xml_attr(xml_find_first(def_rpr, ".//a:srgbClr"), "val"), "000000")

  # End Properties (CRITICAL FIX)
  end_rpr <- xml_find_first(tx_pr, ".//a:endParaRPr")
  expect_equal(xml_attr(end_rpr, "sz"), "1200")
  expect_equal(xml_attr(end_rpr, "b"), "1")
  expect_equal(xml_attr(end_rpr, "i"), "1")
  expect_equal(xml_attr(xml_find_first(end_rpr, ".//a:solidFill/a:srgbClr"), "val"), "000000")

  # 4. Verify Gridlines
  dash_node <- xml_find_first(xml, "//cx:axis[@id='1']/cx:majorGridlines//a:prstDash")
  expect_equal(xml_attr(dash_node, "val"), "dash")

  grid_clr <- xml_find_first(xml, "//cx:axis[@id='1']/cx:majorGridlines//a:srgbClr")
  expect_equal(xml_attr(grid_clr, "val"), "FF0000")

  # 5. Verify Axis Line Color
  axis_line_clr <- xml_find_first(xml, "//cx:axis[@id='1']/cx:spPr//a:ln/a:solidFill/a:srgbClr")
  expect_equal(xml_attr(axis_line_clr, "val"), "000000")
})

test_that("Bubble chart generates valid XML and integrates with workbook", {

  skip_if_not_installed("viridisLite")

  # 1. Setup Data
  df <- data.frame(
    Product = paste("Item", 1:10),
    Sales = runif(10, 50, 100),
    Profit = runif(10, 10, 40),
    MarketShare = runif(10, 5, 25)
  )

  # 2. Initialize Chart
  bc <- Chart$new()
  bc$add_series(
    name = "Market Performance",
    label  = "Sheet1!$B$2:$B$11",
    data   = "Sheet1!$C$2:$C$11",
    weight = "Sheet1!$D$2:$D$11",
    color  = viridisLite::viridis(10),
    type   = "bubbleChart"
  )

  bc$set_bubble_options(scale = 120, show_neg = FALSE)
  bc$set_chart_title("Market Share Analysis")

  # 3. Test XML structure
  # Render and parse back to xml2 object for easy XPath testing
  xml_str <- bc$render()
  xml_parsed <- read_xml(xml_str)

  # Check for mandatory bubbleChart node
  expect_true(xml_name(xml_find_first(xml_parsed, ".//c:bubbleChart")) == "c:bubbleChart")

  # Check for bubble-specific settings
  expect_equal(xml_attr(xml_find_first(xml_parsed, ".//c:bubbleScale"), "val"), "120")
  expect_equal(xml_attr(xml_find_first(xml_parsed, ".//c:showNegBubbles"), "val"), "0")

  # Check for the three data dimensions in the first series
  expect_true(xml_length(xml_find_first(xml_parsed, ".//c:ser/c:xVal")) > 0)
  expect_true(xml_length(xml_find_first(xml_parsed, ".//c:ser/c:yVal")) > 0)
  expect_true(xml_length(xml_find_first(xml_parsed, ".//c:ser/c:bubbleSize")) > 0)

  # Check Palette Application (dPt nodes)
  dpts <- xml_find_all(xml_parsed, ".//c:dPt")
  expect_gt(length(dpts), 0)
})

test_that("set_data_table correctly adds dTable node to XML", {
  # 1. Minimal setup
  chart <- Chart$new(type = "barChart")
  chart$add_series(name = "H1", data = "Sheet1!A1:A5", label = "Sheet1!B1:B5")

  # 2. Enable data table
  chart$set_data_table(TRUE)

  # 3. Render and parse XML
  xml_res <- read_xml(chart$render())

  # 4. Assertions
  # Check if the dTable tag exists
  dtable_node <- xml_find_first(xml_res, ".//c:dTable")
  expect_false(xml_type(dtable_node) == "element_absent")

  # Optional: check for specific data table attributes (keys, borders, etc.)
  expect_true(any(grepl("showKeys", as.character(xml_res))))
})

test_that("drop_lines, high_low_lines, and up_down_bars render correctly", {
  # 1. Setup a basic line chart with two series
  ch <- Chart$new(type = "lineChart")
  ch$add_series(name = "S1", data = "AA!A2:A5", label = "AA!B2:B5")
  ch$add_series(name = "S2", data = "AA!C2:C5")

  # 2. Enable features
  ch$drop_lines     <- TRUE
  ch$high_low_lines <- TRUE
  ch$up_down_bars   <- TRUE

  # 3. Render and parse XML
  expect_message(
    expect_message(
      xml_res <- read_xml(ch$render()),
      "drop lines require ",
    ),
    "high low lines require "
  )

  # 4. Assertions for specialized OOXML nodes
  # Drop Lines: <c:dropLines>
  expect_false(xml_type(xml_find_first(xml_res, ".//c:dropLines")) == "element_absent")

  # High-Low Lines: <c:hiLowLines>
  expect_false(xml_type(xml_find_first(xml_res, ".//c:hiLowLines")) == "element_absent")

  # Up-Down Bars: <c:upDownBars>
  expect_false(xml_type(xml_find_first(xml_res, ".//c:upDownBars")) == "element_absent")

  # Verify they are nested within the lineChart node
  expect_equal(length(xml_find_all(xml_res, ".//c:lineChart/c:dropLines")), 1)
})

test_that("barChart with theme colors and stacked grouping renders correctly", {
  # 1. Minimal Setup
  ch <- Chart$new()
  ch$add_series(
    name      = "Sheet1!$B$1",
    data      = "Sheet1!$B$2:$B$4",
    label     = "Sheet1!$A$2:$A$4",
    color     = wb_color(theme = 5),
    grouping  = "percentStacked",
    dir       = "bar",
    type      = "barChart"
  )

  # 2. Render XML
  xml_res <- ch$render()

  # 3. Assertions
  # Check Bar Direction (Horizontal 'bar' vs Vertical 'col')
  expect_true(any(grepl('<c:barDir val="bar"/>', as.character(xml_res))))

  # Check Stacked Grouping
  expect_true(any(grepl('<c:grouping val="percentStacked"/>', as.character(xml_res))))

  # Check Theme Color: Looking for theme="5" in the fill properties
  # Standard path: c:ser -> c:spPr -> a:solidFill -> a:schemeClr
  expect_true(any(grepl('val="accent2"', as.character(xml_res))))

  # Verify titles and styling
  ch$set_chart_title("Test Title", font_color = "0D6797")
  xml_titled <- ch$render()
  expect_true(any(grepl("0D6797", as.character(xml_titled))))
})

test_that("trendlines and error bars render correctly in a series", {
  # 1. Setup a basic chart
  ch <- Chart$new(type = "barChart")
  ch$add_series(
    name = "Revenue",
    data = "Sheet1!$B$2:$B$4",
    label = "Sheet1!$A$2:$A$4",
    # Add Trendline configuration
    trendline = list(
      type = "linear",
      color = "FF0000",
      show_r2 = FALSE
    ),
    # Add Error Bar configuration
    error_bars = list(
      type = "percentage",
      value = 10,
      color = "404040"
    )
  )

  # 2. Render XML
  xml_res <- read_xml(ch$render())

  # 3. Assertions for Trendlines
  # OOXML node for trendline is <c:trendline>
  trend_node <- xml_find_first(xml_res, ".//c:ser/c:trendline")
  expect_false(xml_type(trend_node) == "element_absent")
  expect_true(any(grepl('trendlineType val="linear"', as.character(xml_res))))
  expect_true(any(grepl('dispRSqr val="0"', as.character(xml_res))))

  # 4. Assertions for Error Bars
  # OOXML node for error bars is <c:errBars>
  err_node <- xml_find_first(xml_res, ".//c:ser/c:errBars")
  expect_false(xml_type(err_node) == "element_absent")
  expect_true(any(grepl('errBarType val="both"', as.character(xml_res)))) # Default behavior
  expect_true(any(grepl('errValType val="percentage"', as.character(xml_res))))

  # 5. Check colors (Solid Fill inside spPr of these nodes)
  expect_true(any(grepl("FF0000", as.character(xml_res)))) # Trendline color
  expect_true(any(grepl("404040", as.character(xml_res)))) # Error bar color
})

test_that("ChartEx boxWhisker with complex styling renders correctly", {
  # 1. Setup minimal data
  ch <- ChartEx$new()

  # 2. Apply Titles and Axis Styles
  ch$set_chart_title("MPG Distribution", font_size = 16, font_name = "Arial", bold = TRUE, italic = TRUE, fill = "#DDADDA", line = wb_color("blue"), line_width = 5) # test italic, fill and line
  ch$set_x_title("by Cylinder", font_size = 12, italic = TRUE)
  ch$set_x_axis(font_size = 10) # test this
  ch$set_y_axis(font_size = 12, font_name = "Times New Roman", italic = TRUE, color = "000000")

  # 3. Add Series with Color and Line Style
  ch$add_series(
    name = '"Super Duper MPG"',
    data   = "Data!$A$2:$A$5",
    label  = "Data!$B$2:$B$5",
    color  = wb_color("magenta"),
    line_color = wb_color("black"),
    type   = "boxWhisker"
  )

  ch$
    set_legend_style( # test this
      pos       = "r",
      font_size = 15,
      bold      = TRUE,
      color     = wb_color(theme = "4")
    )

  wb <- openxlsx2::wb_workbook()$add_worksheet("data")$add_encharter(graph = ch)

  # 4. Render XML
  xml_res <- read_xml(ch$render(1))

  # --- Assertions ---

  # Verify Font Names and Sizes (sz is 100x in OOXML)
  expect_true(any(grepl('typeface="Arial"', as.character(xml_res))))
  expect_true(any(grepl('typeface="Times New Roman"', as.character(xml_res))))
  expect_true(any(grepl('sz="1600"', as.character(xml_res)))) # Title sz 16
  expect_true(any(grepl('sz="1200"', as.character(xml_res)))) # Axis sz 12

  # Verify Boolean Styles
  expect_true(any(grepl(' b="1"', as.character(xml_res)))) # Bold
  expect_true(any(grepl(' i="1"', as.character(xml_res)))) # Italic

  # Verify BoxWhisker Type
  expect_true(any(grepl('layoutId="boxWhisker"', as.character(xml_res))))

  # Verify Fill and Line Colors
  # Magenta (FF00FF) and Black (000000)
  expect_true(any(grepl("FF00FF", as.character(xml_res), ignore.case = TRUE)))

  # Verify ChartEx Axis Node presence
  expect_false(xml_type(xml_find_first(xml_res, ".//cx:axis")) == "element_absent")
})
