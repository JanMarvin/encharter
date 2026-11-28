cov_data <- function() {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    K = c("a", "b", "c", "d"), X = c(1, 2, 3, 4), Y = c(2, 4, 3, 5), Z = c(5, 9, 7, 3)))
  list(wb = wb, d = openxlsx2::wb_data(wb, sheet = "Data"))
}

render_xml <- function(ch) as.character(ch$render())

test_that("plot_vis_only needs a single logical", {
  expect_error(ec("bar")$set_plot_vis_only("yes"), "TRUE or FALSE")
})

test_that("pugi nodes and nodesets print", {
  doc <- read_xml("<r><a/><b/></r>")
  expect_output(print(doc), "<r>")
  kids <- xml_children(doc)
  expect_output(print(kids), "pugi_nodeset \\(2\\)")
  many <- read_xml(paste0("<r>", strrep("<a/>", 25), "</r>"))
  expect_output(print(xml_children(many)), "\\.\\.\\.")
  expect_output(print(xml_children(read_xml("<r/>"))), "pugi_nodeset \\(0\\)")
  expect_identical(xml_name(read_xml("<r/>")), "r")
})

test_that("axis options reach the XML", {
  x <- cov_data()
  ch <- ec("scatter")$add_series(name = "Y", data = x$d, label = "X")$
    set_y_axis(min = 1, max = 10, log_base = 10, minor = 1, crosses_at = 2, color = "none", italic = TRUE)$
    set_x_axis(color = "none", minor = 0.5, crosses_at = 1)
  xml <- render_xml(ch)
  expect_match(xml, "c:logBase", fixed = TRUE)
  expect_match(xml, "c:crossesAt", fixed = TRUE)
  expect_match(xml, "c:minorUnit", fixed = TRUE)
})

test_that("series styling options reach the XML", {
  x <- cov_data()
  ch <- ec("line")$add_series(name = "Y", data = x$d, label = "K",
    trendline = list(type = "poly", order = 2, name = "fit", forward = 1, backward = 1, intercept = 0,
                     show_r2 = FALSE, show_eq = FALSE))$
    add_series(name = "Z", data = x$d, label = "K", trendline = list(type = "movingAvg", period = 2))
  xml <- render_xml(ch)
  expect_match(xml, "<c:name>fit</c:name>", fixed = TRUE)
  expect_match(xml, "c:order", fixed = TRUE)
  expect_match(xml, "c:period", fixed = TRUE)
  expect_match(xml, "c:dispEq", fixed = TRUE)
  ch <- ec("bar")$add_series(name = "Y", data = x$d, label = "K")$
    set_data_label_style(pos = "b")$set_3d_options(gap_depth = 50)
  expect_match(render_xml(ch), "inBase", fixed = TRUE)
  ch <- ec("line3DChart")$add_series(name = "Y", data = x$d, label = "K")$set_3d_options(gap_depth = 50)
  expect_match(render_xml(ch), "c:gapDepth", fixed = TRUE)
})

chart_space <- function(plot_area) {
  paste0(
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
    '<c:chart><c:autoTitleDeleted val="1"/><c:plotArea><c:layout/>', plot_area,
    "</c:plotArea></c:chart></c:chartSpace>"
  )
}

num_ref <- function(f, vals, fmt = "General") {
  pts <- paste0('<c:pt idx="', seq_along(vals) - 1L, '"><c:v>', vals, "</c:v></c:pt>", collapse = "")
  paste0("<c:numRef><c:f>", f, "</c:f><c:numCache><c:formatCode>", fmt, '</c:formatCode><c:ptCount val="',
         length(vals), '"/>', pts, "</c:numCache></c:numRef>")
}

plot_png2 <- function(chart, wb) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 500, 400)
  plot(chart, wb = wb)
  grDevices::dev.off()
  file.info(f)$size
}

plot_png <- function(chart) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 500, 400)
  plot(chart)
  grDevices::dev.off()
  file.info(f)$size
}

test_that("loading keeps theme colors, markers, points and line extras", {
  ser <- paste0(
    '<c:ser><c:idx val="0"/><c:order val="0"/>',
    '<c:spPr><a:ln w="19050"><a:solidFill><a:schemeClr val="accent2"><a:lumMod val="60000"/><a:lumOff val="40000"/></a:schemeClr></a:solidFill></a:ln></c:spPr>',
    '<c:marker><c:symbol val="circle"/><c:size val="7"/><c:spPr><a:noFill/><a:ln w="9525"><a:solidFill><a:srgbClr val="FF0000"/></a:solidFill><a:prstDash val="dash"/></a:ln></c:spPr></c:marker>',
    '<c:dPt><c:idx val="1"/><c:marker><c:symbol val="square"/><c:size val="9"/><c:spPr><a:solidFill><a:srgbClr val="00FF00"/></a:solidFill></c:spPr></c:marker></c:dPt>',
    '<c:dPt><c:idx val="2"/><c:spPr><a:noFill/><a:ln><a:noFill/></a:ln></c:spPr></c:dPt>',
    '<c:dPt><c:idx val="3"/><c:spPr><a:solidFill><a:schemeClr val="accent1"/></a:solidFill><a:ln w="12700"><a:solidFill><a:srgbClr val="0000FF"/></a:solidFill></a:ln></c:spPr></c:dPt>',
    "<c:cat>", num_ref("Data!$A$2:$A$5", c(45292, 45293, 45294.5, 45295), "yyyy-mm-dd"), "</c:cat>",
    "<c:val>", num_ref("Data!$B$2:$B$5", c(1, 3, 2, 4)), "</c:val></c:ser>"
  )
  xml <- chart_space(paste0(
    '<c:lineChart><c:grouping val="standard"/>', ser,
    '<c:dropLines/><c:hiLowLines/><c:upDownBars><c:gapWidth val="120"/></c:upDownBars>',
    '<c:axId val="1"/><c:axId val="2"/></c:lineChart>',
    '<c:dateAx><c:axId val="1"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="b"/><c:crossAx val="2"/></c:dateAx>',
    '<c:valAx><c:axId val="2"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/><c:axPos val="l"/><c:crossAx val="1"/>',
    '<c:dispUnits><c:custUnit val="1000"/></c:dispUnits></c:valAx>'
  ))
  ch <- encharter:::load_chart(xml)
  expect_true(ch$drop_lines)
  expect_true(ch$high_low_lines)
  expect_true(ch$up_down_bars)
  expect_equal(ch$axis_params$x$major_time, "days")
  expect_equal(ch$axis_params$y$disp_units, 1000)
  s <- ch$series_data[[1]]
  expect_equal(s$marker$fill, "none")
  expect_equal(s$marker$line$type, "dash")
  expect_length(s$points, 3L)
  expect_match(as.character(ch$render()), "c:upDownBars", fixed = TRUE)
  expect_gt(plot_png(ch), 1000)
})

test_that("loading keeps pie of pie, doughnut, bubble and 3D group settings", {
  pie_ser <- paste0('<c:ser><c:idx val="0"/><c:order val="0"/><c:cat><c:strLit><c:ptCount val="3"/>',
                    '<c:pt idx="0"><c:v>a</c:v></c:pt><c:pt idx="1"><c:v>b</c:v></c:pt><c:pt idx="2"><c:v>c</c:v></c:pt></c:strLit></c:cat><c:val>',
                    num_ref("Data!$B$2:$B$4", c(3, 2, 1)), "</c:val></c:ser>")
  ch <- encharter:::load_chart(chart_space(paste0(
    '<c:ofPieChart><c:ofPieType val="bar"/>', pie_ser,
    '<c:splitType val="cust"/><c:custSplit><c:secondPiePt val="1"/><c:secondPiePt val="2"/></c:custSplit></c:ofPieChart>')))
  expect_equal(ch$split_pos, c(1L, 2L))
  ch <- encharter:::load_chart(chart_space(paste0(
    '<c:ofPieChart><c:ofPieType val="pie"/>', pie_ser, '<c:splitType val="pos"/><c:splitPos val="2"/></c:ofPieChart>')))
  expect_equal(ch$split_pos, 2)
  ch <- encharter:::load_chart(chart_space(paste0("<c:doughnutChart>", pie_ser, "</c:doughnutChart>")))
  expect_equal(ch$hole_size, 75)
  ch <- encharter:::load_chart(chart_space(paste0(
    '<c:bar3DChart><c:barDir val="col"/><c:grouping val="clustered"/>', pie_ser,
    '<c:gapDepth val="40"/><c:shape val="cone"/><c:axId val="1"/><c:axId val="2"/><c:axId val="3"/></c:bar3DChart>')))
  expect_equal(ch$gap_depth, 40)
  expect_equal(ch$bar_shape, "cone")
})

test_that("extended charts write binning, subtotals and styled text", {
  ce <- ChartEx$new()
  ce$add_series(name = "H", data = "dat!B2:B10", type = "clusteredColumn", color = NULL,
                binning = list(intervalClosed = "left", underflow = 1, overflow = 9, binSize = 2))
  ce$add_series(name = "H2", data = "dat!C2:C10", type = "clusteredColumn",
                binning = list(intervalClosed = "right", binCount = 4))
  xml <- as.character(ce$render(id_start = 1))
  expect_match(xml, "intervalClosed=\"l\"", fixed = TRUE)
  expect_match(xml, "cx:binSize", fixed = TRUE)
  expect_match(xml, "cx:binCount", fixed = TRUE)

  wf <- ChartEx$new()
  expect_error(wf$add_series(name = "H", data = "dat!B2:B10", type = "waterfall", subtotals = c(-1, 2)), "non-negative")
  wf$add_series(name = "H", data = "dat!B2:B10", type = "waterfall", subtotals = c(0, 8))
  wf$update_series(1, subtotals = c(1, 2))
  expect_match(as.character(wf$render(id_start = 1)), "<cx:idx val=\"2\"/>", fixed = TRUE)
  wf$update_series(1, subtotals = TRUE)
  expect_match(as.character(wf$render(id_start = 1)), "cx:subtotals", fixed = TRUE)
  expect_error(wf$update_series(1, subtotals = c(NA, 1)), "non-negative")

  x <- cov_data()
  expect_error(wf$update_series(1, data = x$d, label = "nothing"), "not found")

  ch <- ChartEx$new()
  ch$add_series(name = "H", data = "dat!B2:B10", label = "dat!A2:A10", type = "treemap")
  ch$set_chart_title(openxlsx2::fmt_txt("Rich", bold = TRUE))
  ch$set_legend_style(pos = "none")
  expect_false(grepl("cx:legend", as.character(ch$render(id_start = 1)), fixed = TRUE))
  ch$set_legend_style(pos = "b", bold = TRUE, italic = TRUE, color = "FF0000", font_size = 9)
  ch$set_data_label_style(show_val = TRUE, font_size = 8, bold = FALSE, italic = TRUE, font_color = "00FF00", font_name = "Arial")
  xml <- as.character(ch$render(id_start = 1))
  expect_match(xml, "i=\"1\"", fixed = TRUE)
})

test_that("helper functions handle their edge cases", {
  expect_error(encharter:::as_xml_attr(NA), "must be 0, 1")
  expect_equal(encharter:::as_xml_attr(NULL), "")
  expect_equal(encharter:::as_xml_attr(TRUE), "1")
  expect_equal(encharter:::to_abs_ref(c(1, 2)), c(1, 2))
  expect_equal(unname(encharter:::to_abs_ref(c("Data!A1", "b"))), c("'Data'!$A$1", "b"))
  expect_match(encharter:::get_color(openxlsx2::wb_color(theme = 4)), "schemeClr", fixed = TRUE)
  expect_warning(encharter:::get_color(openxlsx2::wb_color(indexed = 3)), "only rgb and theme")
  expect_equal(encharter:::get_color("red"), "")
  txt <- openxlsx2::fmt_txt("a", bold = TRUE, font = "Arial", size = 12, color = openxlsx2::wb_color("FF0000"))
  expect_match(encharter:::fmt_txt2(txt), "Arial", fixed = TRUE)
  expect_silent(encharter:::check_title_text(NULL))
  expect_identical(encharter:::check_wb_color(openxlsx2::wb_color(auto = TRUE), "col"), openxlsx2::wb_color(auto = TRUE))
})

test_that("private color helpers cover their special cases", {
  ch <- ec("bar")
  priv <- ch$.__enclos_env__$private
  node <- read_xml("<r/>")
  priv$render_color(node, "none")
  priv$render_color(node, "auto")
  priv$render_color(node, NULL)
  priv$render_color_core(node, NULL)
  priv$render_color_core(node, "auto")
  priv$render_color_core(node, openxlsx2::wb_color(auto = TRUE))
  priv$render_color_core(node, "abc")
  xml <- as.character(node)
  expect_match(xml, "<a:noFill/>", fixed = TRUE)
  expect_match(xml, "accent1", fixed = TRUE)
  expect_match(xml, "000000", fixed = TRUE)
  expect_equal(priv$sanitize_xml(1), 1)
})

test_that("templates report their problems", {
  x <- cov_data()
  ch <- ec("bar")$add_series(name = "Y", data = x$d, label = "K")
  f <- tempfile(fileext = ".txt")
  expect_warning(ec_to_crtx(ch, f), "does not end in .crtx")
  expect_error(encharter:::crtx_chart_xml(tempfile(fileext = ".crtx")), "not found")
  zf <- tempfile(fileext = ".crtx")
  old <- Sys.getenv("R_ZIPCMD")
  withr_zip <- function(cmd) Sys.setenv(R_ZIPCMD = cmd)
  on.exit(Sys.setenv(R_ZIPCMD = old), add = TRUE)
  withr_zip("/nonexistent/zip")
  expect_error(ec_to_crtx(ch, zf), "zip program")
  withr_zip("false")
  if (nzchar(Sys.which("false"))) expect_error(ec_to_crtx(ch, zf), "Could not write")
  withr_zip(old)
  if (nzchar(old) || nzchar(Sys.which("zip"))) {
    ec_to_crtx(ch, zf)
    ec_to_crtx(ch, zf)
    expect_true(file.exists(zf))
    empty <- tempfile(fileext = ".zip")
    d <- tempfile()
    dir.create(d)
    writeLines("x", file.path(d, "a.txt"))
    old_wd <- setwd(d)
    utils::zip(empty, "a.txt", flags = "-rq")
    setwd(old_wd)
    expect_error(encharter:::crtx_chart_xml(empty), "No chart part")
  }
})

na_data <- function() {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    K = c("a", "b", "c", "d", "e"), X = c(1, 2, 3, 4, 5), Y = c(2, NA, 3, 5, 4), Z = c(5, 9, NA, 3, 6),
    Day = as.Date("2024-01-01") + c(0, 1, 2, 3, 40)))
  list(wb = wb, d = openxlsx2::wb_data(wb, sheet = "Data"))
}

test_that("plot() handles gaps, titles, grids and trendlines in every chart family", {
  x <- na_data()
  for (type in c("line", "bar", "area", "scatter")) {
    ch <- ec(type)$add_series(name = "Y", data = x$d, label = if (type == "scatter") "X" else "K",
                              trendline = list(type = "linear", show_eq = FALSE, show_r2 = FALSE))$
      add_series(name = "Z", data = x$d, label = if (type == "scatter") "X" else "K",
                 trendline = list(type = "poly", order = 2, color = "FF0000", show_r2 = TRUE),
                 error_bars = list(type = "stdDev", value = 1))$
      set_x_title("x")$set_y_title("y")$
      set_x_axis(grid_lines = TRUE, minor_grid_lines = TRUE, minor_tick = "out", major_tick = "in", tick_mark_skip = 2)$
      set_y_axis(grid_lines = TRUE, crosses = "min", minor = 0.5)
    expect_gt(plot_png(ch), 1000)
  }
  ch <- ec("bar")$add_series(name = "Y", data = x$d, label = "K", dir = "bar")$set_x_title("x")$set_y_title("y")$
    set_x_axis(grid_lines = TRUE, label_pos = "none")$set_y_axis(crosses_at = 2)
  expect_gt(plot_png(ch), 1000)
  ch <- ec("line")$add_series(name = "Y", data = x$d, label = "Day")$set_x_axis(minor_tick = "out", minor = 7, minor_time = "days")
  expect_gt(plot_png(ch), 1000)
  ch <- ec("bubble")$add_series(name = "Y", data = x$d, label = "X", weight = "Z", trendline = list(type = "linear"))$
    set_bubble_options(show_neg = TRUE)
  expect_gt(plot_png(ch), 1000)
  ch <- ec("pie")$add_series(name = "Z", data = x$d, label = "K", color = c("FF0000", "00FF00"))
  expect_gt(plot_png(ch), 1000)
  ch <- ec("line3DChart")$add_series(name = "Y", data = x$d, label = "K")$add_series(name = "Z", data = x$d, label = "K")
  expect_gt(plot_png(ch), 1000)
  ch <- ec("bar3DChart")$add_series(name = "Y", data = x$d, label = "K")$add_series(name = "Z", data = x$d, label = "K")
  expect_gt(plot_png(ch), 1000)
  ch <- ec("area3DChart")$add_series(name = "Y", data = x$d, label = "K")
  expect_gt(plot_png(ch), 1000)
  ch <- ec("pie3DChart")$add_series(name = "Z", data = x$d, label = "K", color = c("FF0000", "00FF00"))
  expect_gt(plot_png(ch), 1000)
})

test_that("plot() reports bad input", {
  expect_error(plot(ec("bar")), "no series")
  x <- na_data()
  ch <- ec("bar")$add_series(name = "Y", data = x$d, label = "K")
  expect_error(plot(ch, wb = 1), "wbWorkbook")
  expect_error(plot.Chart(ChartEx$new()), "standard charts")
  expect_error(plot(ChartEx$new()), "no series")
  ch$series_data[[1]]$type <- "madeUpChart"
  expect_error(plot(ch), "not supported")
})

test_that("plot() draws extended charts with styles, titles and odd data", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    Item = c("a", "b", "c", "d", "e"), V = c(5, -2, 4, NA, 6), Const = c(3, 3, 3, 3, 3),
    Zero = c(0, 0, 0, 0, 0)))
  ex <- function(type, col = "B", ...) {
    ChartEx$new()$add_series(name = "Data!$B$1", data = sprintf("Data!$%s$2:$%s$6", col, col),
                             label = "Data!$A$2:$A$6", type = type, ...)
  }
  styled <- function(ch) ch$set_chart_style(fill = "EEEEEE", line = "333333")$set_plot_style(fill = "FAFAFA", line = "999999")
  for (type in c("waterfall", "clusteredColumn", "boxWhisker", "funnel", "paretoLine")) {
    ch <- styled(ex(type, color = "FF0000"))$set_x_title("x")$set_y_title("y")$
      set_x_axis(grid_lines = TRUE)$set_y_axis(grid_lines = TRUE)
    expect_gt(plot_png2(ch, wb), 1000)
  }
  ch <- ex("clusteredColumn", "C")
  expect_gt(plot_png2(ch, wb), 1000)
  ch <- ex("clusteredColumn", "D", binning = list(binCount = 3))
  expect_gt(plot_png2(ch, wb), 1000)
  for (type in c("treemap", "sunburst")) {
    ch <- ex(type, "B")$set_data_label_style(show_val = TRUE, show_cat = TRUE)
    expect_gt(plot_png2(ch, wb), 1000)
    ch <- ex(type, "D")
    expect_gt(plot_png2(ch, wb), 1000)
  }
  expect_error(plot(ex("waterfall"), wb = 1), "wbWorkbook")
  expect_error(plot(ex("waterfall")), "pass the workbook")
  bad <- ChartEx$new()$add_series(name = "H", data = "Nope!$B$2:$B$6", type = "waterfall")
  expect_error(plot(bad, wb = wb), "not found")
  lit <- ChartEx$new()$add_series(name = "\"Own name\"", data = "Data!$B$2:$B$6", type = "waterfall")
  expect_gt(plot_png2(lit, wb), 1000)
  lit <- ChartEx$new()$add_series(data = "Data!$B$2:$B$6", type = "waterfall")
  expect_gt(plot_png2(lit, wb), 1000)
})
