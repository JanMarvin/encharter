test_that("normalize_encharter_type handles all aliases", {
  # Canonical OOXML names pass through unchanged
  expect_equal(normalize_encharter_type("barChart"),      "barChart")
  expect_equal(normalize_encharter_type("lineChart"),     "lineChart")
  expect_equal(normalize_encharter_type("scatterChart"),  "scatterChart")
  expect_equal(normalize_encharter_type("bubbleChart"),   "bubbleChart")
  expect_equal(normalize_encharter_type("pieChart"),      "pieChart")
  expect_equal(normalize_encharter_type("doughnutChart"), "doughnutChart")
  expect_equal(normalize_encharter_type("areaChart"),     "areaChart")
  expect_equal(normalize_encharter_type("radarChart"),    "radarChart")
  expect_equal(normalize_encharter_type("surfaceChart"),  "surfaceChart")
  expect_equal(normalize_encharter_type("waterfall"),     "waterfall")
  expect_equal(normalize_encharter_type("treemap"),       "treemap")

  # R-style aliases
  expect_equal(normalize_encharter_type("bar"),       "barChart")
  expect_equal(normalize_encharter_type("col"),       "barChart")
  expect_equal(normalize_encharter_type("column"),    "barChart")
  expect_equal(normalize_encharter_type("barplot"),   "barChart")
  expect_equal(normalize_encharter_type("hist"),      "barChart")
  expect_equal(normalize_encharter_type("histogram"), "barChart")
  expect_equal(normalize_encharter_type("line"),      "lineChart")
  expect_equal(normalize_encharter_type("scatter"),   "scatterChart")
  expect_equal(normalize_encharter_type("point"),     "scatterChart")
  expect_equal(normalize_encharter_type("xy"),        "scatterChart")
  expect_equal(normalize_encharter_type("bubble"),    "bubbleChart")
  expect_equal(normalize_encharter_type("pie"),       "pieChart")
  expect_equal(normalize_encharter_type("doughnut"),  "doughnutChart")
  expect_equal(normalize_encharter_type("donut"),     "doughnutChart")
  expect_equal(normalize_encharter_type("area"),      "areaChart")
  expect_equal(normalize_encharter_type("radar"),     "radarChart")
  expect_equal(normalize_encharter_type("spider"),    "radarChart")
  expect_equal(normalize_encharter_type("surface"),   "surfaceChart")
  expect_equal(normalize_encharter_type("box"),       "boxWhisker")
  expect_equal(normalize_encharter_type("boxplot"),   "boxWhisker")
  expect_equal(normalize_encharter_type("map"),       "regionMap")
  expect_equal(normalize_encharter_type("pareto"),    "paretoLine")

  # Case-insensitive
  expect_equal(normalize_encharter_type("BAR"),       "barChart")
  expect_equal(normalize_encharter_type("Line"),      "lineChart")
  expect_equal(normalize_encharter_type("SCATTER"),   "scatterChart")
})

test_that("encharter() factory returns correct R6 class", {
  # Standard chart types → Chart
  for (tp in c("barChart", "lineChart", "areaChart", "scatterChart",
               "pieChart", "doughnutChart", "radarChart", "bubbleChart")) {
    obj <- encharter(tp)
    expect_s3_class(obj, "Chart")
    expect_s3_class(obj, "EncharterBase")
  }

  # Extended chart types → ChartEx
  for (tp in c("waterfall", "treemap", "sunburst", "funnel", "boxWhisker")) {
    obj <- encharter(tp)
    expect_s3_class(obj, "ChartEx")
    expect_s3_class(obj, "EncharterBase")
  }

  # Aliases work end-to-end through the factory
  expect_s3_class(encharter("bar"),     "Chart")
  expect_s3_class(encharter("line"),    "Chart")
  expect_s3_class(encharter("scatter"), "Chart")
  expect_s3_class(encharter("pie"),     "Chart")

  # Unknown type errors
  expect_error(encharter("unknown_type"), regexp = "should be one of")
})

test_that("ec() is an alias for encharter()", {
  expect_identical(ec, encharter)
  expect_s3_class(ec("line"), "Chart")
})

test_that("set_chart_title stores text and style correctly", {
  ch <- ec("line")
  ch$set_chart_title("My Title", font_size = 14, bold = TRUE, font_color = "FF0000")

  expect_equal(ch$chart_title$text, "My Title")
  expect_equal(ch$chart_title$style$font_size, 14)
  expect_true(ch$chart_title$style$bold)
  expect_equal(ch$chart_title$style$font_color, "FF0000")
})

test_that("set_x_title and set_y_title store correctly", {
  ch <- ec("bar")
  ch$set_x_title("X Axis", italic = TRUE)
  ch$set_y_title("Y Axis", font_name = "Arial")

  expect_equal(ch$x_title$text, "X Axis")
  expect_true(ch$x_title$style$italic)
  expect_equal(ch$y_title$text, "Y Axis")
  expect_equal(ch$y_title$style$font_name, "Arial")
})

test_that("set_x_axis updates axis_params$x", {
  ch <- ec("line")
  ch$set_x_axis(min = 0, max = 10, major = 2, grid_lines = TRUE,
                font_color = "888888", rotation =  -45)

  expect_equal(ch$axis_params$x$min, 0)
  expect_equal(ch$axis_params$x$max, 10)
  expect_equal(ch$axis_params$x$major, 2)
  expect_true(ch$axis_params$x$grid_lines)
  expect_equal(ch$axis_params$x$font_color, "888888")
  expect_equal(ch$axis_params$x$rotation, -45)
})

test_that("set_y_axis updates axis_params$y", {
  ch <- ec("bar")
  ch$set_y_axis(min = 0, max = 1000, major = 200, format = "#,##0",
                grid_lines = TRUE, grid_color = "DDDDDD")

  expect_equal(ch$axis_params$y$min, 0)
  expect_equal(ch$axis_params$y$max, 1000)
  expect_equal(ch$axis_params$y$major, 200)
  expect_equal(ch$axis_params$y$format, "#,##0")
  expect_true(ch$axis_params$y$grid_lines)
  expect_equal(ch$axis_params$y$grid_color, "DDDDDD")
})

test_that("set_axis_params validates crosses argument", {
  ch <- ec("line")
  expect_error(ch$set_x_axis(crosses = "bad_value"), regexp = "crosses")

  # Valid values don't error
  expect_no_error(ch$set_x_axis(crosses = "autoZero"))
  expect_no_error(ch$set_x_axis(crosses = "min"))
  expect_no_error(ch$set_x_axis(crosses = "max"))
})

test_that("set_axis_params validates label_pos argument", {
  ch <- ec("line")
  expect_error(ch$set_y_axis(label_pos = "invalid"), regexp = "label_pos")

  expect_no_error(ch$set_y_axis(label_pos = "nextTo"))
  expect_no_error(ch$set_y_axis(label_pos = "high"))
  expect_no_error(ch$set_y_axis(label_pos = "low"))
  expect_no_error(ch$set_y_axis(label_pos = "none"))
})

test_that("set_axis_params validates tick mark arguments", {
  ch <- ec("line")
  expect_error(ch$set_x_axis(major_tick = "bad"), regexp = "major_tick")
  expect_error(ch$set_x_axis(minor_tick = "bad"), regexp = "minor_tick")

  expect_no_error(ch$set_x_axis(major_tick = "cross"))
  expect_no_error(ch$set_x_axis(major_tick = "in"))
  expect_no_error(ch$set_x_axis(major_tick = "out"))
  expect_no_error(ch$set_x_axis(major_tick = "none"))
})

test_that("set_axis_params validates grid_lines dash style", {
  ch <- ec("line")
  expect_error(ch$set_x_axis(grid_lines = "wavy"), regexp = "grid_lines")

  expect_no_error(ch$set_x_axis(grid_lines = "dash"))
  expect_no_error(ch$set_x_axis(grid_lines = "dot"))
  expect_no_error(ch$set_x_axis(grid_lines = TRUE))
  expect_no_error(ch$set_x_axis(grid_lines = FALSE))
})

test_that("set_axis_params is idempotent — only updates supplied fields", {
  ch <- ec("line")
  # Set one field
  ch$set_y_axis(min = 5)
  expect_equal(ch$axis_params$y$min, 5)
  # grid_lines default is TRUE for y-axis — should be unchanged
  expect_true(ch$axis_params$y$grid_lines)

  # Update a different field
  ch$set_y_axis(max = 100)
  # min should be preserved
  expect_equal(ch$axis_params$y$min, 5)
  expect_equal(ch$axis_params$y$max, 100)
})

test_that("set_y2_axis and set_x2_axis update correct axis_params slot", {
  ch <- ec("line")
  ch$set_y2_axis(min = 0, max = 1, format = "0%")
  expect_equal(ch$axis_params$y2$min, 0)
  expect_equal(ch$axis_params$y2$max, 1)
  expect_equal(ch$axis_params$y2$format, "0%")
  # y-axis should be untouched
  expect_null(ch$axis_params$y$format)

  ch$set_x2_axis(grid_lines = TRUE)
  expect_true(ch$axis_params$x2$grid_lines)
  expect_false(ch$axis_params$x$grid_lines)
})

test_that("set_chart_style and set_plot_style store correctly", {
  ch <- ec("bar")
  ch$set_chart_style(fill = "F0F0F0", line = "CCCCCC", line_width = 0.5)
  expect_equal(ch$chart_style$fill, "F0F0F0")
  expect_equal(ch$chart_style$line, "CCCCCC")
  expect_equal(ch$chart_style$line_width, 0.5)

  ch$set_plot_style(fill = "FAFAFA")
  expect_equal(ch$plot_style$fill, "FAFAFA")
  expect_null(ch$plot_style$line)
})

test_that("set_legend_style stores params correctly", {
  ch <- ec("line")
  ch$set_legend_style(pos = "b", align = "min", overlay = TRUE,
                      font_size = 9, bold = TRUE)

  expect_equal(ch$legend_params$pos, "b")
  expect_equal(ch$legend_params$align, "min")
  expect_equal(ch$legend_params$overlay, "1")
  expect_equal(ch$legend_params$style$font_size, 9)
  expect_true(ch$legend_params$style$bold)
})

test_that("set_data_label_style stores params correctly", {
  ch <- ec("bar")
  ch$set_data_label_style(show_val = TRUE, show_cat = FALSE, pos = "outEnd")

  expect_true(ch$label_params$show_val)
  expect_false(ch$label_params$show_cat)
  expect_equal(ch$label_params$pos, "outEnd")
})

test_that("set_data_label_style validates pos argument", {
  ch <- ec("bar")
  expect_error(ch$set_data_label_style(pos = "invalid"), regexp = "pos")
  expect_no_error(ch$set_data_label_style(pos = "t"))
  expect_no_error(ch$set_data_label_style(pos = "ctr"))
  expect_no_error(ch$set_data_label_style(pos = "inEnd"))
  expect_no_error(ch$set_data_label_style(pos = "outEnd"))
})

test_that("add_series stores series data correctly for Chart", {
  ch <- ec("line")
  ch$add_series(
    name = "'Sheet1'!$A$1",
    data   = "'Sheet1'!$A$2:$A$10",
    label  = "'Sheet1'!$B$2:$B$10",
    color  = "FF0000"
  )

  expect_length(ch$series_data, 1)
  s <- ch$series_data[[1]]
  expect_equal(s$line$color, "FF0000")
  expect_equal(s$type, "lineChart")
  expect_equal(s$sec_type, "none")
})

test_that("add_series validates marker type", {
  ch <- ec("line")
  expect_error(
    ch$add_series(data = "'Sheet1'!$A$2:$A$10", marker = "hexagon"),
    regexp = "marker"
  )
  expect_no_error(
    ch$add_series(data = "'Sheet1'!$A$2:$A$10", marker = "circle")
  )
})

test_that("add_series validates grouping argument", {
  ch <- ec("bar")
  expect_error(
    ch$add_series(data = "'Sheet1'!$A$2:$A$10", grouping = "invalid"),
    regexp = "grouping"
  )
  expect_no_error(
    ch$add_series(data = "'Sheet1'!$A$2:$A$10", grouping = "stacked")
  )
})

test_that("add_series secondary argument accepts logical and string", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5", secondary = FALSE)
  ch$add_series(data = "'Sheet1'!$B$2:$B$5", secondary = TRUE)
  ch$add_series(data = "'Sheet1'!$C$2:$C$5", secondary = "x")

  expect_equal(ch$series_data[[1]]$sec_type, "none")
  expect_equal(ch$series_data[[2]]$sec_type, "y")
  expect_equal(ch$series_data[[3]]$sec_type, "x")
})

test_that("add_series requires a sheet reference for data", {
  ch <- ec("line")
  expect_error(
    ch$add_series(data = "A2:A10"),
    regexp = "sheet reference"
  )
})

test_that("add_series accumulates multiple series", {
  ch <- ec("bar")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  ch$add_series(data = "'Sheet1'!$B$2:$B$5")
  ch$add_series(data = "'Sheet1'!$C$2:$C$5")
  expect_length(ch$series_data, 3)
})

test_that("add_series line_color falls back to color", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5", color = "4472C4")
  expect_equal(ch$series_data[[1]]$line$color, "4472C4")

  ch$add_series(data = "'Sheet1'!$B$2:$B$5",
                color = "4472C4", line_color = "FF0000")
  expect_equal(ch$series_data[[2]]$line$color, "FF0000")
})

test_that("render() errors without series data", {
  ch <- ec("line")
  expect_error(ch$render(), regexp = "no data|add.*series")
})

test_that("render() returns xml document for Chart", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$10")
  result <- ch$render()
  expect_true(is.character(result))
})

test_that("render() produces c:chartSpace root for Chart", {
  ch <- ec("bar")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  xml <- ch$render()
  root_name <- xml_name(openxlsx2::read_xml(xml)) # works with xml2
  expect_equal(root_name, "c:chartSpace")
})

test_that("render() includes chart title when set", {
  ch <- ec("bar")
  ch$set_chart_title("Test Title")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, "Test Title")
})

test_that("render() omits autoTitleDeleted=0 when title is set", {
  ch <- ec("bar")
  ch$set_chart_title("Has Title")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, 'autoTitleDeleted val="0"')
})

test_that("render() sets autoTitleDeleted=1 when no title", {
  ch <- ec("bar")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, 'autoTitleDeleted val="1"')
})

test_that("render() includes secondary valAx when secondary series present", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  ch$add_series(data = "'Sheet1'!$B$2:$B$5", secondary = TRUE)
  xml_str <- as.character(ch$render())
  # Two valAx nodes expected
  expect_equal(length(gregexpr("<c:valAx>", xml_str)[[1]]), 2)
})

test_that("render() respects axis min/max", {
  ch <- ec("bar")
  ch$set_y_axis(min = 0, max = 500)
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, '<c:min val="0"')
  expect_match(xml_str, '<c:max val="500"')
})

test_that("render() outputs grid_lines when enabled", {
  ch <- ec("bar")
  ch$set_x_axis(grid_lines = TRUE)
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, "majorGridlines")
})

test_that("render() suppresses legend when pos = 'none'", {
  ch <- ec("line")
  ch$set_legend_style(pos = "none")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  xml_str <- as.character(ch$render())
  expect_false(grepl("<c:legend>", xml_str))
})

test_that("set_y2_title warns when no secondary series", {
  ch <- ec("line")
  expect_warning(
    ch$set_y2_title("Secondary"),
    regexp = "secondary"
  )
})

test_that("set_y2_title accepts title after secondary series added", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  ch$add_series(data = "'Sheet1'!$B$2:$B$5", secondary = TRUE)
  expect_no_warning(ch$set_y2_title("Right Axis"))
  expect_equal(ch$y2_title$text, "Right Axis")
})

test_that("print() returns self invisibly", {
  ch <- ec("bar")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  result <- capture.output(ret <- ch$print())
  expect_identical(ret, ch)
})

test_that("print() shows correct series count", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5")
  ch$add_series(data = "'Sheet1'!$B$2:$B$5")
  output <- capture.output(ch$print())
  expect_match(paste(output, collapse = "\n"), "Number of Series: 2")
})

test_that("palette is overridden by vector color for pie/doughnut", {
  ch <- ec("pie")
  custom_pal <- c("FF0000", "00FF00", "0000FF")
  ch$add_series(data = "'Sheet1'!$A$2:$A$4", color = custom_pal)
  expect_equal(ch$palette, custom_pal)
})

# --- ChartEx-specific tests ---

test_that("ChartEx initialises with correct default legend params", {
  ch <- ec("waterfall")
  expect_equal(ch$legend_params$pos, "t")
  expect_equal(ch$legend_params$align, "ctr")
})

test_that("ChartEx add_series stores series correctly", {
  ch <- ec("waterfall")
  ch$add_series(
    data = "'Sheet1'!$B$2:$B$6",
    label = "'Sheet1'!$A$2:$A$6"
  )
  expect_length(ch$series_data, 1)
  expect_equal(ch$series_data[[1]]$type, "waterfall")
})

test_that("ChartEx add_series rejects non-extended type", {
  ch <- ec("treemap")
  expect_error(
    ch$add_series(data = "'Sheet1'!$A$2:$A$5", type = "lineChart"),
    regexp = "series type"
  )
})

test_that("ChartEx set_x_axis updates axis_params$x", {
  ch <- ec("waterfall")
  ch$set_x_axis(grid_lines = TRUE, font_color = "444444")
  expect_true(ch$axis_params$x$grid_lines)
  expect_equal(ch$axis_params$x$font_color, "444444")
})

test_that("ChartEx render() returns xml with cx:chartSpace root", {
  ch <- ec("waterfall")
  ch$add_series(
    data = "'Sheet1'!$B$2:$B$5",
    label = "'Sheet1'!$A$2:$A$5"
  )
  xml <- ch$render()
  expect_true(is.character(xml))
  expect_equal(xml_name(openxlsx2::read_xml(xml)), "cx:chartSpace")
})

test_that("ChartEx render() includes chart title when set", {
  ch <- ec("treemap")
  ch$set_chart_title("Treemap Title")
  ch$add_series(
    data = "'Sheet1'!$B$2:$B$5",
    label = "'Sheet1'!$A$2:$A$5"
  )
  xml_str <- as.character(ch$render())
  expect_match(xml_str, "Treemap Title")
})

test_that("ChartEx waterfall subtotals render correctly", {
  ch <- ec("waterfall")
  ch$add_series(
    data      = "'Sheet1'!$B$2:$B$6",
    label     = "'Sheet1'!$A$2:$A$6",
    subtotals = c(4)
  )
  xml_str <- as.character(ch$render())
  expect_match(xml_str, "cx:subtotals")
  expect_match(xml_str, 'cx:idx val="4"')
})

test_that("ChartEx boxWhisker statistics argument renders correctly", {
  ch <- ec("boxWhisker")
  ch$add_series(
    data       = "'Sheet1'!$A$2:$A$20",
    statistics = "inclusive"
  )
  xml_str <- as.character(ch$render())
  expect_match(xml_str, 'quartileMethod="inclusive"')
})

# --- Color rendering ---

test_that("render_color_core handles plain hex", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5", color = "4472C4")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, 'val="4472C4"')
})

test_that("render_color_core strips # prefix from hex", {
  ch <- ec("line")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5", color = "#4472C4")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, 'val="4472C4"')
  expect_false(grepl('val="#4472C4"', xml_str))
})

test_that("render_color_core handles 8-digit hex with alpha", {
  ch <- ec("bar")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5", color = "804472C4")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, 'val="4472C4"')
  expect_match(xml_str, "a:alpha")
})

test_that("render_color_core handles 'auto' as accent1 scheme color", {
  ch <- ec("bar")
  ch$add_series(data = "'Sheet1'!$A$2:$A$5", color = "auto")
  xml_str <- as.character(ch$render())
  expect_match(xml_str, 'val="accent1"')
})

# --- normalize_encharter_string ---

test_that("normalize_encharter_string maps direction aliases", {
  expect_equal(normalize_encharter_string("v"),          "col")
  expect_equal(normalize_encharter_string("vertical"),   "col")
  expect_equal(normalize_encharter_string("h"),          "bar")
  expect_equal(normalize_encharter_string("horizontal"), "bar")
})

test_that("normalize_encharter_string maps position aliases", {
  expect_equal(normalize_encharter_string("left"),   "l")
  expect_equal(normalize_encharter_string("right"),  "r")
  expect_equal(normalize_encharter_string("top"),    "t")
  expect_equal(normalize_encharter_string("bottom"), "b")
  expect_equal(normalize_encharter_string("center"), "ctr")
})

test_that("normalize_encharter_string returns input unchanged for unknown values", {
  expect_equal(normalize_encharter_string("r"),   "r")
  expect_equal(normalize_encharter_string("ctr"), "ctr")
})

test_that("normalize_encharter_string handles NULL", {
  expect_null(normalize_encharter_string(NULL))
})

# --- set_pie_options / set_bubble_options ---

test_that("set_pie_options stores values on Chart", {
  ch <- ec("pie")
  ch$set_pie_options(rotation = 90, expansion = 10)
  expect_equal(ch$first_slice_ang, 90)
  expect_equal(ch$expansion, 10)
})

test_that("set_bubble_options stores scale on Chart", {
  ch <- ec("bubble")
  ch$set_bubble_options(scale = 150, show_neg = TRUE)
  expect_equal(ch$bubble_scale, 150)
  expect_true(ch$show_neg_bubbles)
})

test_that("set_disp_blanks validates and stores value", {
  ch <- ec("line")
  ch$set_disp_blanks("span")
  expect_equal(ch$disp_blanks_as, "span")
  expect_error(ch$set_disp_blanks("invalid"), regexp = "disp_blanks_as")
})

# --- combo chart smoke test ---

test_that("combo chart renders with bar and line series", {
  ch <- ec("bar")
  ch$add_series(
    data = "'Data'!$B$2:$B$13",
    label = "'Data'!$A$2:$A$13",
    type = "barChart"
  )
  ch$add_series(
    data      = "'Data'!$C$2:$C$13",
    label     = "'Data'!$A$2:$A$13",
    type      = "lineChart",
    secondary = TRUE,
    marker    = "circle"
  )
  xml_str <- as.character(ch$render())
  expect_match(xml_str, "c:barChart")
  expect_match(xml_str, "c:lineChart")
  # Should have two valAx (primary left, secondary right)
  n_valax <- length(gregexpr("<c:valAx>", xml_str)[[1]])
  expect_equal(n_valax, 2)
})

test_that("fmt_txt() is supported", {
  txt <- fmt_txt("Bold", bold = TRUE, size = 18) +
    fmt_txt("\nItalic", italic = TRUE, color = wb_color("black"), size = 14)

  e <- encharter::ec(type = "barplot")
  e$add_series(data = "Sheet1!A1:A2")
  e$set_chart_title(txt)
  xml <- e$render()
  expect_match(xml, "b=\"1\"")
  expect_match(xml, "i=\"1\"")
})
