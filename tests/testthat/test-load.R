render_loaded <- function(wb, i = 1L, type = NULL) {
  ch  <- ec_load(wb, i, type)
  ids <- ec_axis_ids(wb, i, type)
  if (inherits(ch, "ChartEx")) {
    as.character(ch$render(id_start = ids$id_start, guid = ids$guid))
  } else {
    as.character(ch$render(u_ids = ids))
  }
}

stored_xml <- function(wb, i = 1L, type = NULL) {
  if (identical(type, "chartEx") || (is.null(type) && !nzchar(wb$charts$chart[[i]]))) {
    wb$charts$chartEx[[i]]
  } else {
    wb$charts$chart[[i]]
  }
}

data_wb <- function() {
  openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Month = month.abb[1:6],
    Sales = c(120, 135, 128, 160, 175, 190),
    Cost  = c(80, 90, 85, 100, 110, 120),
    Day   = as.Date("2024-01-01") + 0:5
  ))
}

test_that("ec_load validates its input", {
  expect_error(ec_load(list()), "wbWorkbook")
  expect_error(ec_load(openxlsx2::wb_workbook()), "no charts")
  wb <- data_wb()
  wb$add_encharter(dims = "F2:L20", graph = ec("bar")$add_series(data = "Data!B2:B7"))
  expect_error(ec_load(wb, 2), "between 1 and 1")
  expect_error(ec_load(wb, 1, "chartEx"), "no chartEx XML")
})

test_that("styled combo chart round-trips", {
  wb <- data_wb()
  chart <- ec("barChart")$
    set_chart_title("Bars", font_size = 12, font_color = "333333")$
    set_x_title("Month")$set_y_title("EUR", italic = TRUE)$
    set_y_axis(min = 0, max = 300, major = 50, format = "#,##0", grid_lines = "dash", grid_color = "AAAAAA")$
    set_x_axis(rotation = -45, font_size = 8, tick_lbl_skip = 2)$
    set_legend_style(pos = "b", font_size = 9)$
    set_data_label_style(show_val = TRUE, pos = "outEnd", font_size = 7)$
    set_chart_style(fill = "F0F0F0", line = "888888", line_width = 0.5)$
    set_plot_style(fill = "FAFAFA")$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7",
               color = "2E4057", gap_width = 80, overlap = -10)$
    add_series(name = "Data!$C$1", data = "Data!$C$2:$C$7", label = "Data!$A$2:$A$7",
               color = "ED7D31", type = "lineChart", secondary = TRUE,
               line_type = "dashed", line_width = 2, marker = "diamond", marker_size = 7,
               marker_fill = "FFFFFF",
               trendline = list(type = "linear", color = "00FF00", show_r2 = TRUE),
               error_bars = list(type = "percentage", value = 10, color = "FF00FF"))$
    set_y2_title("Cost")$set_y2_axis(min = 0, max = 200)
  wb$add_encharter(dims = "F2:L20", graph = chart)

  expect_identical(render_loaded(wb), stored_xml(wb))

  loaded <- ec_load(wb)
  expect_s3_class(loaded, "Chart")
  expect_length(loaded$series_data, 2)
  expect_equal(loaded$series_data[[2]]$sec_type, "y")
  expect_equal(loaded$series_data[[2]]$type, "lineChart")
  expect_equal(loaded$series_data[[1]]$data, "'Data'!$B$2:$B$7")
  expect_equal(loaded$chart_title$text, "Bars")
  expect_equal(loaded$axis_params$y$max, 300)
})

test_that("caches from wb_data round-trip including dates and NA", {
  wb <- data_wb()
  wb$add_data(x = data.frame(v = c(1.5, NA, 3)), dims = "F1")
  wd <- openxlsx2::wb_data(wb, sheet = "Data", dims = "A1:F7")
  chart <- ec("line")$add_series(name = Sales, data = wd, label = Day)$
    set_x_axis(major = 1, major_time = "days", base_time = "days", format = "yyyy-mm-dd")
  wb$add_encharter(dims = "H2:P20", graph = chart)
  expect_identical(render_loaded(wb), stored_xml(wb))
  loaded <- ec_load(wb)
  expect_s3_class(loaded$series_data[[1]]$cat_cache, "Date")
  expect_equal(loaded$axis_params$x$base_time, "days")

  chart <- ec("bar")$add_series(name = v, data = wd, label = Month)
  wb$add_encharter(dims = "H22:P40", graph = chart)
  expect_identical(render_loaded(wb, 2), stored_xml(wb, 2))
  expect_equal(ec_load(wb, 2)$series_data[[1]]$data_cache, c(1.5, NA, 3, NA, NA, NA))
})

test_that("pie, bubble, scatter and 3D charts round-trip", {
  wb <- data_wb()
  wb$add_encharter(dims = "F2:L20", graph = ec("pieChart")$
    set_pie_options(rotation = 90, expansion = 10)$
    set_data_label_style(show_val = TRUE, show_percent = TRUE)$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7",
               color = c("FF0000", "00FF00", "0000FF")))
  wb$add_encharter(dims = "F22:L40", graph = ec("bubbleChart")$
    set_bubble_options(scale = 50, show_neg = TRUE, size_represents = "w")$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$C$2:$C$7",
               weight = "Data!$C$2:$C$7", color = "80FF0000"))
  wb$add_encharter(dims = "F42:L60", graph = ec("scatterChart")$
    add_series(name = "S", data = "Data!$C$2:$C$7", label = "Data!$B$2:$B$7",
               show_line = FALSE, marker = "square")$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$C$2:$C$7",
               secondary = "x", smooth = TRUE)$
    set_x2_title("top")$set_x_axis(log_base = 10, min = 1))
  wb$add_encharter(dims = "F62:L80", graph = ec("bar3DChart")$
    set_3d_options(rot_x = 20, rot_y = 30, shape = "cylinder", gap_depth = 120, h_percent = 80)$
    add_series(data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7", invert_if_negative = TRUE, dir = "bar"))
  wb$add_encharter(dims = "F82:L100", graph = ec("ofPieChart")$
    set_of_pie_options(type = "bar", split_type = "cust", split_pos = c(1, 3), second_size = 60)$
    add_series(data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7"))

  for (i in seq_len(NROW(wb$charts))) {
    expect_identical(render_loaded(wb, i), stored_xml(wb, i), info = paste("chart", i))
  }
  expect_equal(ec_load(wb, 1)$palette, c("FF0000", "00FF00", "0000FF"))
  expect_equal(ec_load(wb, 2)$series_data[[1]]$line$color, "80FF0000")
  expect_equal(ec_load(wb, 3)$series_data[[2]]$sec_type, "x")
  expect_equal(ec_load(wb, 5)$split_pos, c(1L, 3L))
})

test_that("rich text titles are rebuilt as fmt_txt", {
  wb <- data_wb()
  txt <- openxlsx2::fmt_txt("Head", bold = TRUE, size = 18) + openxlsx2::fmt_txt("\nSub", italic = TRUE, color = openxlsx2::wb_color("black"))
  wb$add_encharter(dims = "F2:L20", graph = ec("bar")$set_chart_title(txt)$add_series(data = "Data!$B$2:$B$7"))
  expect_identical(render_loaded(wb), stored_xml(wb))
  expect_s3_class(ec_load(wb)$chart_title$text, "fmt_txt")
})

test_that("chartEx charts round-trip", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("My Sheet")$add_data(x = data.frame(a = letters[1:5], b = c(5, -2, 3, -1, 4)))
  chart <- ec("waterfall")$
    add_series(name = "My Sheet!B1", data = "My Sheet!B2:B6", label = "My Sheet!A2:A6",
               subtotals = c(2, 4), color = c("FF0000", "00FF00"))$
    set_chart_title("WF", bold = TRUE, fill = "EEEEEE")$
    set_legend_style(pos = "b", font_size = 9, bold = TRUE)$
    set_data_label_style(show_val = TRUE, font_size = 8, format = "0.0")$
    set_waterfall_colors(increase = "00AA00", decrease = "AA0000", total = "888888")$
    set_x_title("Cats", italic = TRUE)$
    set_y_axis(min = -5, max = 15, major = 5, grid_lines = "dotted", format = "0")
  wb$add_encharter(dims = "D2:L20", graph = chart)
  wb$add_encharter(dims = "D22:L40", graph = ec("treemap")$
    add_series(data = "My Sheet!B2:B6", label = "My Sheet!A2:A6", parent_label = "banner"))
  wb$add_encharter(dims = "D42:L60", graph = ec("boxWhisker")$
    add_series(data = "My Sheet!B2:B6", statistics = "exclusive",
               visibility = list(meanLine = TRUE, outliers = FALSE)))
  wb$add_encharter(dims = "D62:L80", graph = ec("clusteredColumn")$
    add_series(data = "My Sheet!B2:B6", binning = list(binCount = 3L, underflow = 0, overflow = "auto")))

  for (i in seq_len(NROW(wb$charts))) {
    expect_identical(render_loaded(wb, i), stored_xml(wb, i), info = paste("chart", i))
  }
  loaded <- ec_load(wb, 1)
  expect_s3_class(loaded, "ChartEx")
  expect_equal(loaded$series_data[[1]]$data, "'My Sheet'!$B$2:$B$6")
  expect_equal(loaded$series_data[[1]]$name, "'My Sheet'!$B$1")
  expect_equal(loaded$series_data[[1]]$subtotals, c(2, 4))
  expect_identical(loaded$color_xml, chart$color_xml)
  expect_equal(ec_load(wb, 3)$series_data[[1]]$visibility, list(meanLine = TRUE, outliers = FALSE))
  expect_equal(ec_load(wb, 4)$series_data[[1]]$binning, list(binCount = 3L, underflow = 0, overflow = "auto"))
})

test_that("charts survive save, load and extending a range", {
  wb <- data_wb()
  wb$add_encharter(dims = "F2:L20", graph = ec("line")$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7", marker = "circle"))
  wb$add_encharter(dims = "F22:L40", graph = ec("waterfall")$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7"))
  tmp <- tempfile(fileext = ".xlsx")
  wb$save(tmp)
  wb2 <- openxlsx2::wb_load(tmp)

  expect_error(ec_load(wb2, 1), "both a standard chart and a chartEx")
  expect_identical(render_loaded(wb2, 1, "chart"),   stored_xml(wb, 1, "chart"))
  expect_identical(render_loaded(wb2, 1, "chartEx"), stored_xml(wb, 1, "chartEx"))

  chart <- ec_load(wb2, 1, "chart")
  chart$series_data[[1]]$data  <- "Data!$B$2:$B$8"
  chart$series_data[[1]]$label <- "Data!$A$2:$A$8"
  chart$add_series(name = "Data!$C$1", data = "Data!$C$2:$C$8", color = "00AA00")
  wb2$add_encharter(dims = "F42:L60", graph = chart)
  expect_equal(NROW(wb2$charts), 2L)
  expect_match(wb2$charts$chart[[2]], "\\$B\\$2:\\$B\\$8")
  expect_match(wb2$charts$chart[[2]], "\\$C\\$2:\\$C\\$8")
})

test_that("literal series (numLit/strLit) round-trip", {
  chart <- ec("bar")$add_series(data = "Data!$B$2:$B$4", label = "Data!$A$2:$A$4")
  chart$series_data[[1]]$data  <- NULL
  chart$series_data[[1]]$label <- NULL
  chart$series_data[[1]]$data_cache <- c(1, NA, 3)
  chart$series_data[[1]]$cat_cache  <- c("a", "b", "c")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")
  wb$add_encharter(dims = "F2:L20", graph = chart)
  expect_match(wb$charts$chart[[1]], "<c:cat><c:strLit><c:ptCount val=\"3\"/>")
  expect_match(wb$charts$chart[[1]], "<c:val><c:numLit><c:ptCount val=\"3\"/><c:pt idx=\"0\"><c:v>1</c:v></c:pt><c:pt idx=\"2\">")
  expect_identical(render_loaded(wb), stored_xml(wb))
  loaded <- ec_load(wb)
  expect_null(loaded$series_data[[1]]$data)
  expect_equal(loaded$series_data[[1]]$data_cache, c(1, NA, 3))
  expect_equal(loaded$series_data[[1]]$cat_cache, c("a", "b", "c"))
})

test_that("charts from other applications load: horizontal bars, General format codes", {
  xml <- paste0(
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><c:chart><c:plotArea><c:layout/>',
    '<c:barChart><c:barDir val="bar"/><c:grouping val="clustered"/><c:varyColors val="0"/>',
    '<c:ser><c:idx val="0"/><c:order val="0"/><c:tx><c:v>S</c:v></c:tx>',
    '<c:val><c:numLit><c:formatCode>General</c:formatCode><c:ptCount val="2"/>',
    '<c:pt idx="0"><c:v>44774</c:v></c:pt><c:pt idx="1"><c:v>44409</c:v></c:pt></c:numLit></c:val></c:ser>',
    '<c:gapWidth val="150"/><c:axId val="1"/><c:axId val="2"/></c:barChart>',
    '<c:catAx><c:axId val="1"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/>',
    '<c:axPos val="l"/><c:crossAx val="2"/></c:catAx>',
    '<c:valAx><c:axId val="2"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/>',
    '<c:axPos val="b"/><c:majorGridlines/><c:crossAx val="1"/></c:valAx>',
    '</c:plotArea><c:plotVisOnly val="1"/></c:chart></c:chartSpace>'
  )
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")
  wb$add_chart_xml(dims = "F2:L20", xml = xml)
  loaded <- ec_load(wb)
  expect_equal(loaded$type, "barChart")
  expect_equal(loaded$series_data[[1]]$dir, "bar")
  expect_equal(loaded$series_data[[1]]$data_cache, c(44774, 44409))
  expect_true(loaded$axis_params$y$grid_lines)
  expect_false(loaded$axis_params$x$grid_lines)
  expect_silent(out <- loaded$render())
  expect_match(out, "<c:barDir val=\"bar\"/>")
})

test_that("update_series() re-points loaded series at new data", {
  sales <- data.frame(Month = month.abb[1:6], Volume = 1:6, Sales = 11:16)
  wb <- openxlsx2::wb_workbook()$add_worksheet("Sheet1")$add_data(x = sales)
  wd <- openxlsx2::wb_data(wb)
  chart <- ec("bar")$
    add_series(name = Volume, data = wd, label = Month)$
    add_series(name = Sales, data = wd, label = Month, type = "line", secondary = TRUE, line_type = "dashed")
  wb$add_encharter(dims = "E2:M20", graph = chart)

  wb$add_data(x = data.frame(Month = "Jul", Volume = 7, Sales = 17), dims = "A8", col_names = FALSE)
  loaded <- ec_load(wb)
  loaded$update_series(data = openxlsx2::wb_data(wb), label = Month)

  expect_equal(loaded$series_data[[1]]$name,  "'Sheet1'!$B$1")
  expect_equal(loaded$series_data[[1]]$data,  "'Sheet1'!$B$2:$B$8")
  expect_equal(loaded$series_data[[2]]$data,  "'Sheet1'!$C$2:$C$8")
  expect_equal(loaded$series_data[[2]]$label, "'Sheet1'!$A$2:$A$8")
  expect_equal(loaded$series_data[[2]]$data_cache, 11:17)
  expect_equal(loaded$series_data[[2]]$cat_cache, month.abb[1:7])
  expect_equal(loaded$series_data[[2]]$line$type, "dash")
  expect_equal(loaded$series_data[[2]]$sec_type, "y")

  loaded$update_series(2, color = "C00000", line_type = "solid", data = "Sheet1!C2:C9")
  expect_equal(loaded$series_data[[2]]$line$color, "C00000")
  expect_equal(loaded$series_data[[2]]$marker$fill, "C00000")
  expect_equal(loaded$series_data[[2]]$data, "'Sheet1'!$C$2:$C$9")
  expect_null(loaded$series_data[[2]]$data_cache)
  expect_equal(loaded$series_data[[1]]$line$color, "4472C4")

  expect_error(loaded$update_series(3, color = "000000"), "between 1 and 2")
  expect_error(loaded$update_series(data = openxlsx2::wb_data(wb), label = Nope), "object 'Nope' not found")

  wb$add_encharter(dims = "E22:M40", graph = loaded)
  expect_match(wb$charts$chart[[2]], "\\$C\\$2:\\$C\\$9")

  wf <- ec("waterfall")$add_series(name = Sales, data = wd, label = Month)
  wb$add_encharter(dims = "O2:W20", graph = wf)
  wf2 <- ec_load(wb, 1, "chartEx")$update_series(data = openxlsx2::wb_data(wb), label = Month, subtotals = 6)
  expect_equal(wf2$series_data[[1]]$data,  "'Sheet1'!$C$2:$C$8")
  expect_equal(wf2$series_data[[1]]$label, "'Sheet1'!$A$2:$A$8")
  expect_equal(wf2$series_data[[1]]$subtotals, 6)
})

test_that("luminance modifiers of theme colors survive a round trip", {
  skip_if_not_installed("openxlsx")
  wb <- openxlsx2::wb_load(system.file("extdata", "loadExample.xlsx", package = "openxlsx"))
  chart <- encharter_load(wb, 2)
  grid_color <- chart$axis_params$y$grid_color
  expect_equal(attr(grid_color, "lumMod"), 0.15)
  expect_equal(attr(grid_color, "lumOff"), 0.85)
  xml <- chart$render(u_ids = paste0("1000", 1:5))
  expect_match(xml, '<c:majorGridlines><c:spPr><a:ln w="9525"><a:solidFill><a:schemeClr val="tx1"><a:lumMod val="15000"/><a:lumOff val="85000"/>', fixed = TRUE)
  expect_equal(plot_color(grid_color), "#D9D9D9")
})

test_that("chart templates round trip", {
  skip_if(Sys.which("zip") == "")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Month = month.abb[1:6], Sales = c(120, 135, 128, 160, 175, 190), Cost = c(80, 90, 85, 100, 110, 120)
  ))
  wd <- openxlsx2::wb_data(wb)
  chart <- ec("bar")$set_chart_title("Sales", bold = TRUE, font_color = "C00000")$
    set_y_axis(grid_lines = TRUE, grid_color = "EEEEEE", format = "#,##0")$
    set_chart_style(fill = "F7F7F7")$set_legend_style(pos = "b")$
    add_series(name = Sales, data = wd, label = Month, color = "2E4057")$
    add_series(name = Cost, data = wd, label = Month, color = "E84855", type = "line",
               marker = "circle", line_type = "dashed", secondary = TRUE)
  tmp <- tempfile(fileext = ".crtx")
  expect_invisible(ec_to_crtx(chart, tmp))
  expect_setequal(utils::unzip(tmp, list = TRUE)$Name, c("[Content_Types].xml", "_rels/.rels", "chart/chart.xml"))
  xml <- readLines(unz(tmp, "chart/chart.xml"), warn = FALSE)
  expect_false(any(grepl("<c:f>|<c:numCache>|<c:strCache>", xml)))
  expect_true(any(grepl("<c:tx><c:strRef/></c:tx>", xml, fixed = TRUE)))
  expect_true(any(grepl("<c:val><c:numRef/></c:val>", xml, fixed = TRUE)))

  tpl <- ec_from_crtx(tmp)
  expect_length(tpl$series_data, 0)
  expect_length(tpl$template, 2)
  expect_equal(tpl$chart_style$fill, "F7F7F7")
  expect_equal(tpl$legend_params$pos, "b")
  expect_equal(tpl$axis_params$y$format, "#,##0")
  tpl$add_series(name = Sales, data = wd, label = Month)$add_series(name = Cost, data = wd, label = Month)
  expect_equal(tpl$series_data[[1]]$line$color, "2E4057")
  expect_equal(tpl$series_data[[2]]$type, "lineChart")
  expect_equal(tpl$series_data[[2]]$sec_type, "y")
  expect_equal(tpl$series_data[[2]]$marker$symbol, "circle")
  expect_equal(tpl$series_data[[2]]$line$type, "dash")
  expect_match(tpl$render(), "<c:legendPos val=\"b\"/>", fixed = TRUE)

  plain <- ec("line")$add_series(name = Sales, data = wd, label = Month)$add_series(name = Cost, data = wd, label = Month)
  plain$apply_crtx(tmp)
  expect_equal(plain$series_data[[1]]$line$color, "2E4057")
  expect_equal(plain$series_data[[2]]$marker$symbol, "circle")
  expect_equal(plain$chart_title$style$font_color, "C00000")
  expect_null(plain$chart_title$text)
  expect_error(ec_to_crtx(ec("waterfall"), tmp), "ChartEx")
})

test_that("a template written by Excel loads", {
  tpl <- ec_from_crtx(system.file("extdata", "excel_template.crtx", package = "encharter"))
  expect_length(tpl$series_data, 0)
  expect_length(tpl$template, 2)
  expect_equal(tpl$template[[1]]$line$color, "2E4057")
  expect_true(tpl$template[[1]]$invert_if_negative)
  expect_equal(tpl$template[[2]]$type, "lineChart")
  expect_equal(tpl$template[[2]]$sec_type, "y")
  expect_equal(tpl$template[[2]]$line$type, "dash")
  expect_equal(tpl$axis_params$y$format, "#,##0")
  expect_equal(tpl$legend_params$pos, "b")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Month = month.abb[1:6], Sales = c(120, 135, 128, 160, 175, 190), Cost = c(80, 90, 85, 100, 110, 120)
  ))
  wd <- openxlsx2::wb_data(wb)
  tpl$add_series(name = Sales, data = wd, label = Month)$add_series(name = Cost, data = wd, label = Month)
  xml <- tpl$render()
  expect_match(xml, "<c:lineChart>", fixed = TRUE)
  expect_match(xml, '<a:prstDash val="dash"/>', fixed = TRUE)
})

test_that("deleted axes and per-series labels load and render", {
  xml <- paste0(
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><c:chart><c:plotArea><c:layout/>',
    '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:varyColors val="0"/>',
    '<c:ser><c:idx val="0"/><c:order val="0"/><c:val><c:numLit><c:ptCount val="2"/><c:pt idx="0"><c:v>1</c:v></c:pt><c:pt idx="1"><c:v>2</c:v></c:pt></c:numLit></c:val></c:ser>',
    '<c:ser><c:idx val="1"/><c:order val="1"/><c:dLbls><c:showLegendKey val="0"/><c:showVal val="1"/><c:showCatName val="0"/><c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/></c:dLbls>',
    '<c:val><c:numLit><c:ptCount val="2"/><c:pt idx="0"><c:v>3</c:v></c:pt><c:pt idx="1"><c:v>4</c:v></c:pt></c:numLit></c:val></c:ser>',
    '<c:axId val="1"/><c:axId val="2"/></c:barChart>',
    '<c:catAx><c:axId val="1"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:crossAx val="2"/></c:catAx>',
    '<c:valAx><c:axId val="2"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="1"/><c:axPos val="l"/><c:crossAx val="1"/></c:valAx>',
    "</c:plotArea></c:chart></c:chartSpace>"
  )
  chart <- encharter:::load_chart(xml)
  expect_true(chart$axis_params$y$delete)
  expect_null(chart$axis_params$x$delete)
  expect_null(chart$axis_params$x$auto)
  expect_match(chart$render(), '<c:auto val="1"/>', fixed = TRUE)
  chart2 <- encharter:::load_chart(sub('<c:axPos val="b"/>', '<c:axPos val="b"/><c:auto val="0"/>', xml, fixed = TRUE))
  expect_false(chart2$axis_params$x$auto)
  expect_match(chart2$render(), '<c:auto val="0"/>', fixed = TRUE)
  expect_false(chart$series_data[[1]]$label_params$show_val)
  expect_true(chart$label_params$show_val)
  expect_null(chart$series_data[[2]]$label_params)
  out <- chart$render()
  expect_match(out, '<c:valAx><c:axId val="60812428"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="1"/>', fixed = TRUE)
  expect_equal(lengths(regmatches(out, gregexpr('<c:showVal val="1"/>', out))), 1)
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 400, 300)
  plot(chart)
  grDevices::dev.off()
  expect_gt(file.info(f)$size, 1000)
})

test_that("per-point formatting, labels and the plot area layout round trip", {
  xml <- paste0(
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><c:chart><c:plotArea>',
    '<c:layout><c:manualLayout><c:layoutTarget val="inner"/><c:xMode val="edge"/><c:yMode val="edge"/>',
    '<c:x val="0.1"/><c:y val="0.2"/><c:w val="0.8"/><c:h val="0.6"/></c:manualLayout></c:layout>',
    '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:varyColors val="0"/>',
    '<c:ser><c:idx val="0"/><c:order val="0"/><c:spPr><a:solidFill><a:srgbClr val="30384D"/></a:solidFill></c:spPr>',
    '<c:invertIfNegative val="0"/>',
    '<c:dPt><c:idx val="1"/><c:invertIfNegative val="0"/><c:bubble3D val="0"/><c:spPr><a:solidFill><a:srgbClr val="B0B0B0"/></a:solidFill></c:spPr></c:dPt>',
    '<c:dPt><c:idx val="2"/><c:invertIfNegative val="0"/><c:bubble3D val="0"/><c:spPr><a:noFill/></c:spPr></c:dPt>',
    '<c:dLbls><c:dLbl><c:idx val="0"/><c:delete val="1"/></c:dLbl>',
    '<c:dLbl><c:idx val="2"/><c:layout><c:manualLayout><c:x val="-0.05"/><c:y val="0"/></c:manualLayout></c:layout>',
    '<c:showLegendKey val="0"/><c:showVal val="0"/><c:showCatName val="0"/><c:showSerName val="1"/><c:showPercent val="0"/><c:showBubbleSize val="0"/></c:dLbl>',
    '<c:numFmt formatCode="\\+#,##0;\\-#,##0;" sourceLinked="0"/><c:showLegendKey val="0"/><c:showVal val="1"/><c:showCatName val="0"/>',
    '<c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/><c:separator> </c:separator></c:dLbls>',
    '<c:val><c:numLit><c:ptCount val="3"/><c:pt idx="0"><c:v>5</c:v></c:pt><c:pt idx="1"><c:v>-3</c:v></c:pt><c:pt idx="2"><c:v>2</c:v></c:pt></c:numLit></c:val></c:ser>',
    '<c:axId val="1"/><c:axId val="2"/></c:barChart>',
    '<c:catAx><c:axId val="1"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:crossAx val="2"/></c:catAx>',
    '<c:valAx><c:axId val="2"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="l"/><c:crossAx val="1"/></c:valAx>',
    "</c:plotArea></c:chart></c:chartSpace>"
  )
  chart <- encharter:::load_chart(xml)
  s <- chart$series_data[[1]]
  expect_false(s$invert_if_negative)
  expect_equal(chart$plot_layout, list(x = 0.1, y = 0.2, w = 0.8, h = 0.6, target = "inner"))
  expect_equal(lapply(s$points, `[[`, "color"), list("B0B0B0", "none"))
  expect_true(s$point_labels[[1]]$delete)
  expect_true(s$point_labels[[2]]$show_ser_name)
  expect_equal(s$point_labels[[2]]$dx, -0.05)
  expect_equal(chart$label_params$format, "\\+#,##0;\\-#,##0;")
  expect_equal(chart$label_params$sep, " ")
  out <- chart$render()
  expect_match(out, '<c:invertIfNegative val="0"/>', fixed = TRUE)
  expect_match(out, '<c:dPt><c:idx val="2"/><c:invertIfNegative val="0"/><c:bubble3D val="0"/><c:spPr><a:noFill/></c:spPr></c:dPt>', fixed = TRUE)
  expect_match(out, '<c:dLbl><c:idx val="0"/><c:delete val="1"/></c:dLbl>', fixed = TRUE)
  expect_match(out, '<c:dLbl><c:idx val="2"/><c:layout><c:manualLayout><c:x val="-0.05"/><c:y val="0"/></c:manualLayout></c:layout>', fixed = TRUE)
  expect_match(out, '<c:layoutTarget val="inner"/><c:xMode val="edge"/><c:yMode val="edge"/><c:x val="0.1"/>', fixed = TRUE)
  expect_match(out, "<c:separator> </c:separator>", fixed = TRUE)
  expect_equal(plot_format(c(5, -3, 0), "\\+#,##0;\\-#,##0;"), c("+5", "-3", ""))
  expect_equal(plot_format(1119.29, "#,##0.00\\ \"€\""), "1,119.29 €")
  expect_equal(plot_format(0.094, "\\+#,##0.0%;\\-#,##0.0%"), "+9.4%")
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 400, 300)
  plot(chart)
  grDevices::dev.off()
  expect_gt(file.info(f)$size, 1000)
})

test_that("chart text defaults, label alignment, leader lines and literal categories survive a round trip", {
  xml <- paste0(
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
    '<c:chart><c:autoTitleDeleted val="1"/><c:plotArea><c:layout/>',
    '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:varyColors val="0"/>',
    '<c:ser><c:idx val="0"/><c:order val="0"/>',
    '<c:dLbls><c:txPr><a:bodyPr/><a:lstStyle/><a:p><a:pPr algn="l"><a:defRPr/></a:pPr></a:p></c:txPr>',
    '<c:showLegendKey val="0"/><c:showVal val="1"/><c:showCatName val="0"/><c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/>',
    '<c:showLeaderLines val="0"/><c:extLst><c:ext uri="{CE6537A1-D6FC-4f65-9D91-7224C49458BB}" xmlns:c15="http://schemas.microsoft.com/office/drawing/2012/chart">',
    '<c15:showLeaderLines val="0"/></c:ext></c:extLst></c:dLbls>',
    '<c:cat><c:strLit><c:ptCount val="2"/><c:pt idx="0"><c:v>a</c:v></c:pt><c:pt idx="1"><c:v>b</c:v></c:pt></c:strLit></c:cat>',
    '<c:val><c:numRef><c:f>Sheet1!$B$2:$B$3</c:f><c:numCache><c:formatCode>General</c:formatCode><c:ptCount val="2"/>',
    '<c:pt idx="0"><c:v>5</c:v></c:pt><c:pt idx="1"><c:v>3</c:v></c:pt></c:numCache></c:numRef></c:val></c:ser>',
    '<c:axId val="1"/><c:axId val="2"/></c:barChart>',
    '<c:catAx><c:axId val="1"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:crossAx val="2"/>',
    '<c:txPr><a:bodyPr wrap="square" lIns="38100" tIns="19050" rIns="38100" bIns="19050" anchor="ctr"><a:spAutoFit/></a:bodyPr><a:lstStyle/><a:p><a:pPr><a:defRPr/></a:pPr></a:p></c:txPr>',
    '<c:lblOffset val="800"/></c:catAx>',
    '<c:valAx><c:axId val="2"/><c:scaling><c:orientation val="minMax"/><c:max val="800000"/></c:scaling><c:delete val="0"/><c:axPos val="l"/><c:crossAx val="1"/></c:valAx>',
    "</c:plotArea></c:chart>",
    "<c:spPr><a:noFill/><a:ln><a:noFill/></a:ln></c:spPr>",
    '<c:txPr><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr sz="800"><a:solidFill><a:srgbClr val="404040"/></a:solidFill><a:latin typeface="Arial"/></a:defRPr></a:pPr></a:p></c:txPr>',
    "</c:chartSpace>"
  )
  chart <- encharter:::load_chart(xml)
  expect_equal(chart$text_style[c("font_size", "font_name", "font_color")], list(font_size = 8, font_name = "Arial", font_color = "404040"))
  expect_equal(chart$chart_style$fill, "none")
  expect_equal(chart$chart_style$line, "none")
  expect_equal(chart$label_params$style$align, "l")
  expect_false(chart$label_params$leader_lines)
  expect_null(chart$axis_params$x$font_size)
  expect_equal(chart$series_data[[1]]$cat_cache, c("a", "b"))
  out <- chart$render()
  expect_match(out, "</c:chart><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln></c:spPr><c:txPr>", fixed = TRUE)
  expect_match(out, '<a:defRPr sz="800"><a:solidFill><a:srgbClr val="404040"/></a:solidFill><a:latin typeface="Arial"/></a:defRPr>', fixed = TRUE)
  expect_match(out, '<a:pPr algn="l"><a:defRPr>', fixed = TRUE)
  expect_match(out, '<c:showLeaderLines val="0"/><c:extLst><c:ext uri="{CE6537A1-D6FC-4f65-9D91-7224C49458BB}"', fixed = TRUE)
  expect_match(out, '<c15:showLeaderLines val="0"/>', fixed = TRUE)
  expect_match(out, '<c:cat><c:strLit><c:ptCount val="2"/><c:pt idx="0"><c:v>a</c:v></c:pt>', fixed = TRUE)
  # text without its own size takes the chart default
  expect_false(grepl('<c:catAx>.*sz="1000"', out))
  expect_equal(chart$axis_params$x$label_offset, 800L)
  expect_match(out, '<c:lblOffset val="800"/>', fixed = TRUE)
  expect_match(out, '<a:bodyPr lIns="38100" tIns="19050" rIns="38100" bIns="19050" wrap="square" anchor="ctr"><a:spAutoFit/></a:bodyPr>', fixed = TRUE)
  expect_match(out, "</c:spPr><c:txPr><a:bodyPr/><a:lstStyle/>", fixed = TRUE)
  expect_match(out, '<c:max val="800000"/>', fixed = TRUE)
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 400, 300)
  plot(chart)
  grDevices::dev.off()
  expect_gt(file.info(f)$size, 1000)
})

test_that("bar outlines and axis positions of horizontal bars survive a round trip", {
  xml <- paste0(
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
    '<c:chart><c:autoTitleDeleted val="1"/><c:plotArea><c:layout/>',
    '<c:barChart><c:barDir val="bar"/><c:grouping val="percentStacked"/><c:varyColors val="0"/>',
    '<c:ser><c:idx val="0"/><c:order val="0"/>',
    '<c:spPr><a:solidFill><a:srgbClr val="747C8F"/></a:solidFill><a:ln w="12700"><a:solidFill><a:srgbClr val="FFFFFF"/></a:solidFill></a:ln></c:spPr>',
    '<c:invertIfNegative val="0"/>',
    '<c:dPt><c:idx val="1"/><c:invertIfNegative val="0"/><c:bubble3D val="0"/><c:spPr><a:noFill/><a:ln w="12700"><a:noFill/></a:ln></c:spPr></c:dPt>',
    '<c:val><c:numLit><c:ptCount val="2"/><c:pt idx="0"><c:v>5</c:v></c:pt><c:pt idx="1"><c:v>3</c:v></c:pt></c:numLit></c:val></c:ser>',
    '<c:overlap val="100"/><c:axId val="1"/><c:axId val="2"/></c:barChart>',
    '<c:catAx><c:axId val="1"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="l"/>',
    '<c:spPr><a:ln w="9525"><a:solidFill><a:srgbClr val="404040"/></a:solidFill></a:ln></c:spPr><c:crossAx val="2"/></c:catAx>',
    '<c:valAx><c:axId val="2"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:crossAx val="1"/></c:valAx>',
    "</c:plotArea></c:chart></c:chartSpace>"
  )
  chart <- encharter:::load_chart(xml)
  s <- chart$series_data[[1]]
  expect_equal(s$border, list(color = "FFFFFF", width = 1))
  expect_equal(s$points[[1]]$border, "none")
  expect_equal(chart$axis_params$x$line_width, 0.75)
  out <- chart$render()
  expect_match(out, '<a:srgbClr val="747C8F"/></a:solidFill><a:ln w="12700"><a:solidFill><a:srgbClr val="FFFFFF"/></a:solidFill></a:ln></c:spPr>', fixed = TRUE)
  expect_match(out, '<c:dPt><c:idx val="1"/><c:invertIfNegative val="0"/><c:bubble3D val="0"/><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln></c:spPr></c:dPt>', fixed = TRUE)
  expect_match(out, '<c:catAx>.*<c:axPos val="l"/>.*<a:ln w="9525">.*<c:valAx>.*<c:axPos val="b"/>')
  # a template made from it hands outline, direction and grouping to new series
  tmp <- tempfile(fileext = ".crtx")
  ec_to_crtx(chart, tmp)
  tpl <- ec_from_crtx(tmp)
  tpl$add_series(name = "Sheet1!$A$1", data = "Sheet1!$A$2:$A$3")
  expect_equal(tpl$series_data[[1]]$border, list(color = "FFFFFF", width = 1))
  expect_equal(tpl$series_data[[1]]$dir, "bar")
  expect_equal(tpl$series_data[[1]]$grouping, "percentStacked")
  expect_equal(tpl$series_data[[1]]$overlap, 100)
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 400, 300)
  plot(chart)
  grDevices::dev.off()
  expect_gt(file.info(f)$size, 1000)
})
