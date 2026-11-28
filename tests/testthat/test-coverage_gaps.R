gap_chart <- function(plot_area) {
  encharter:::load_chart(paste0(
    '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><c:chart><c:plotArea><c:layout/>',
    plot_area, "</c:plotArea></c:chart></c:chartSpace>"
  ))
}

gap_lit <- function(vals) {
  paste0('<c:numLit><c:ptCount val="', length(vals), '"/>',
         paste0('<c:pt idx="', seq_along(vals) - 1L, '"><c:v>', vals, "</c:v></c:pt>", collapse = ""),
         "</c:numLit>")
}

gap_wb <- function() {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    k = c("a", "b", "c"), v = c(3, 4, 2), w = c(5, 6, 4)))
  list(wb = wb, d = openxlsx2::wb_data(wb, sheet = "Data"))
}

test_that("literal series with numeric categories and bubble sizes are written", {
  bubble <- gap_chart(paste0(
    '<c:bubbleChart><c:ser><c:idx val="0"/><c:order val="0"/><c:xVal>', gap_lit(1:3), "</c:xVal>",
    "<c:yVal>", gap_lit(c(3, 4, 2)), "</c:yVal><c:bubbleSize>", gap_lit(c(5, 6, 4)), "</c:bubbleSize></c:ser>",
    '<c:axId val="1"/><c:axId val="2"/></c:bubbleChart>'))
  expect_match(as.character(bubble$render()), "<c:bubbleSize><c:numLit>", fixed = TRUE)
  line <- gap_chart(paste0(
    '<c:lineChart><c:grouping val="standard"/><c:ser><c:idx val="0"/><c:order val="0"/><c:cat>', gap_lit(1:3),
    "</c:cat><c:val>", gap_lit(c(3, 4, 2)), "</c:val></c:ser>",
    '<c:axId val="1"/><c:axId val="2"/></c:lineChart>'))
  expect_match(as.character(line$render()), "<c:cat><c:numLit>", fixed = TRUE)
})

test_that("bubble sizes with a cache are written as a reference", {
  g <- gap_wb()
  ch <- ec("bubble")$add_series(name = v, data = g$d, label = k, weight = w)
  ch$series_data[[1]]$z_cache <- c(5, 6, 4)
  expect_match(as.character(ch$render()), "<c:bubbleSize><c:numRef>", fixed = TRUE)
})

test_that("category axes write scaling, line and crossing settings", {
  g <- gap_wb()
  ch <- ec("line")$add_series(name = v, data = g$d, label = k)$
    set_x_axis(log_base = 10, min = 1, max = 3, color = "none", crosses_at = 2)
  xml <- as.character(ch$render())
  expect_match(xml, "<c:logBase", fixed = TRUE)
  expect_match(xml, "<c:crossesAt", fixed = TRUE)
  expect_match(xml, "<a:ln><a:noFill/></a:ln>", fixed = TRUE)
})

test_that("label separators and label fills are written back", {
  bar <- gap_chart(paste0(
    '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:ser><c:idx val="0"/><c:order val="0"/>',
    '<c:spPr><a:solidFill><a:srgbClr val="FF0000"/></a:solidFill><a:ln><a:noFill/></a:ln></c:spPr>',
    '<c:dLbls><c:dLbl><c:idx val="0"/><c:showLegendKey val="0"/><c:showVal val="1"/><c:showCatName val="0"/>',
    '<c:showSerName val="0"/><c:showPercent val="0"/><c:showBubbleSize val="0"/><c:separator>; </c:separator></c:dLbl>',
    '<c:spPr><a:solidFill><a:srgbClr val="FFFF00"/></a:solidFill></c:spPr>',
    '<c:showLegendKey val="0"/><c:showVal val="1"/><c:showCatName val="0"/><c:showSerName val="0"/>',
    '<c:showPercent val="0"/><c:showBubbleSize val="0"/></c:dLbls>',
    "<c:val>", gap_lit(c(3, 4, 2)), "</c:val></c:ser>",
    '<c:axId val="1"/><c:axId val="2"/></c:barChart>'))
  xml <- as.character(bar$render())
  expect_match(xml, "<c:separator>; </c:separator>", fixed = TRUE)
  expect_match(xml, '<c:dLbls>.*<c:spPr><a:solidFill><a:srgbClr val="FFFF00"/>')
})

test_that("literal categories next to referenced values are written", {
  line <- gap_chart(paste0(
    '<c:lineChart><c:grouping val="standard"/><c:ser><c:idx val="0"/><c:order val="0"/><c:tx><c:v>s</c:v></c:tx>',
    "<c:cat>", gap_lit(1:3), "</c:cat>",
    "<c:val><c:numRef><c:f>Data!$B$2:$B$4</c:f>", gap_lit(c(3, 4, 2)), "</c:numRef></c:val></c:ser>",
    '<c:axId val="1"/><c:axId val="2"/></c:lineChart>'))
  expect_match(as.character(line$render()), "<c:cat><c:numLit>", fixed = TRUE)
})

test_that("a series name that is not a column of the data is an error", {
  g <- gap_wb()
  expect_error(ec("line")$add_series(name = "", data = g$d, label = k), "not found in the wb_data object")
})

test_that("update_series() checks references and fills missing fields", {
  g <- gap_wb()
  ch <- ec("line")$add_series(name = v, data = g$d, label = k)
  expect_error(ch$update_series(label = "A2:A4"), "sheet reference")
  lit <- gap_chart(paste0(
    '<c:lineChart><c:grouping val="standard"/><c:ser><c:idx val="0"/><c:order val="0"/>',
    "<c:val>", gap_lit(c(3, 4, 2)), "</c:val></c:ser>",
    '<c:axId val="1"/><c:axId val="2"/></c:lineChart>'))
  expect_error(lit$update_series(data = g$d), "series 1: object")
  lit$update_series(name = "Data!$B$1", color = "FF0000")
  expect_equal(lit$series_data[[1]]$name, "'Data'!$B$1")
})

test_that("3D chart types combined with others warn", {
  g <- gap_wb()
  ch <- ec("bar3DChart")$add_series(name = v, data = g$d, label = k)$
    add_series(name = w, data = g$d, label = k, type = "line")
  expect_warning(ch$render(), "3D chart types cannot be combined")
})

test_that("chart template styles of titles are applied to titles that are set", {
  skip_if(Sys.which("zip") == "")
  g <- gap_wb()
  tpl <- ec("bar")$set_x_title("T", bold = TRUE, font_color = "C00000")$
    add_series(name = v, data = g$d, label = k)
  tmp <- tempfile(fileext = ".crtx")
  ec_to_crtx(tpl, tmp)
  ch <- ec("bar")$set_x_title("Own")$set_y_title("Own y")$add_series(name = v, data = g$d, label = k)
  ch$apply_crtx(tmp)
  expect_equal(ch$x_title$text, "Own")
  expect_equal(ch$x_title$style$font_color, "C00000")
})

test_that("deleted category axes are written as deleted", {
  ch <- gap_chart(paste0(
    '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:ser><c:idx val="0"/><c:order val="0"/>',
    "<c:val>", gap_lit(c(3, 4, 2)), "</c:val></c:ser>", '<c:axId val="1"/><c:axId val="2"/></c:barChart>',
    '<c:catAx><c:axId val="1"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="1"/>',
    '<c:axPos val="b"/><c:crossAx val="2"/></c:catAx>',
    '<c:valAx><c:axId val="2"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="0"/>',
    '<c:axPos val="l"/><c:crossAx val="1"/></c:valAx>'))
  expect_match(as.character(ch$render()), '<c:catAx><c:axId val="[0-9]+"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="1"/>')
})

test_that("points without outline and series with an empty name are written", {
  bar <- gap_chart(paste0(
    '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:ser><c:idx val="0"/><c:order val="0"/>',
    "<c:tx><c:v></c:v></c:tx>",
    '<c:dPt><c:idx val="0"/><c:invertIfNegative val="0"/><c:spPr><a:solidFill><a:srgbClr val="FF0000"/></a:solidFill>',
    "<a:ln><a:noFill/></a:ln></c:spPr></c:dPt>",
    "<c:val>", gap_lit(c(3, 4, 2)), "</c:val></c:ser>",
    '<c:axId val="1"/><c:axId val="2"/></c:barChart>'))
  expect_match(as.character(bar$render()), "<c:dPt>.*<a:ln><a:noFill/></a:ln>")
})

test_that("extended charts take subtotals, legend fonts, axis rotation and rgb colors", {
  ce <- ec("waterfall")$add_series(name = "Data!$B$1", data = "Data!$B$2:$B$5", label = "Data!$A$2:$A$5", subtotals = TRUE)$
    set_legend_style(color = "FF0000", font_name = "Arial", font_size = 9)$
    set_y_axis(rotation = 45)$set_y_title("Y")$set_x_title("X")
  xml <- as.character(ce$render(1))
  expect_match(xml, '<cx:subtotals><cx:idx val="0"/></cx:subtotals>', fixed = TRUE)
  expect_match(xml, '<a:latin typeface="Arial"/>', fixed = TRUE)
  tm <- ec("treemap")$add_series(data = "Data!$B$2:$B$5", label = "Data!$A$2:$A$5")$
    set_color_cycle(list(openxlsx2::wb_color("FF0000"), openxlsx2::wb_color("00FF00")))
  expect_match(tm$color_xml, "FF0000", fixed = TRUE)
})

test_that("extended charts without legend, with series outline and region colors load", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    a = letters[1:5], b = c(5, 2, 3, 1, 4)))
  wb$add_encharter(dims = "D2:L20", graph = ec("funnel")$
    add_series(name = "Data!B1", data = "Data!B2:B6", label = "Data!A2:A6", color = "FF0000",
               line_color = "0000FF", line_width = 2, gap_width = 40)$
    set_legend_style(pos = "none")$set_x_title("X")$set_y_title("Y"))
  wb$add_encharter(dims = "D22:L40", graph = ec("regionMap")$
    add_series(data = "Data!B2:B6", label = "Data!A2:A6")$
    set_region_map_colors(min = "FFFFFF", max = "FF0000", mid = "FFFF00"))
  f <- encharter_load(wb, 1)
  expect_equal(f$legend_params$pos, "none")
  m <- encharter_load(wb, 2)
  expect_equal(m$region_colors$max, "FF0000")
})

test_that("plot helpers handle unusual formats, symbols and scales", {
  d <- as.Date("2024-03-05")
  expect_equal(encharter:::plot_format(d, "yyyy\\-mm\\-dd"), "2024-03-05")
  expect_equal(encharter:::plot_format(d, "dd_)"), "05 ")
  expect_equal(encharter:::plot_format(d, "dd*x"), "05")
  expect_equal(encharter:::plot_format(1.5, "0.0_)"), "1.5 ")
  expect_equal(encharter:::plot_format(1.5, "\\$0"), "$2")
  expect_equal(encharter:::plot_format(1.5, "0*-"), "2")
  expect_equal(encharter:::plot_color(NA, "#123456"), "#123456")
  expect_equal(encharter:::plot_color("zz", "#123456"), "#123456")
  expect_equal(encharter:::plot_lty("unknown"), "solid")
  expect_equal(unname(vapply(c("x", "plus", "star", "dash", "dot"), encharter:::plot_pch, numeric(1))), c(4, 3, 8, 45, 20))
  expect_true(is.na(encharter:::plot_pch("unknown")))
  sc <- encharter:::plot_scale(NA_real_, NA_real_)
  expect_equal(sc$min, 0)
  expect_gte(sc$max, 1)
  expect_lte(encharter:::plot_scale(5, 5)$min, 0)
  expect_gte(encharter:::plot_scale(-3, -3)$max, -3)
  expect_gt(encharter:::plot_scale(1, 2, list(min = 5, major = 1))$max, 5)
  expect_equal(encharter:::plot_ticks(list(min = 0, max = 10, major = 3, log = NULL)), c(0, 3, 6, 9, 10))
})

gap_plot <- function(chart) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 600, 400)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(chart)
  invisible(TRUE)
}

test_that("plots skip missing and zero values", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    k = c("a", "b", "c", "d"), v = c(3, NA, 2, 5), w = c(5, 6, NA, 4), z = c(-2, 3, 4, 1), o = c(0, 0, 0, 0),
    h = c(6, 7, NA, 9), l = c(1, 2, NA, 3), c = c(4, 5, NA, 6), n = NA_real_))
  d <- openxlsx2::wb_data(wb, sheet = "Data")
  for (type in c("line", "scatter", "bar", "area", "line3DChart", "bar3DChart", "area3DChart")) {
    expect_true(gap_plot(ec(type)$add_series(name = v, data = d, label = k)$add_series(name = w, data = d, label = k)))
  }
  expect_true(gap_plot(ec("scatter")$add_series(name = v, data = d, label = w)))
  expect_true(gap_plot(ec("bubble")$add_series(name = v, data = d, label = w, weight = z)))
  expect_true(gap_plot(ec("bubble")$add_series(name = v, data = d, label = w, weight = z)$
    set_bubble_options(show_neg = TRUE)))
  expect_true(gap_plot(ec("line")$add_series(name = n, data = d, label = k)))
  expect_true(gap_plot(ec("pie")$add_series(name = o, data = d, label = k)))
  expect_true(gap_plot(ec("pie3DChart")$add_series(name = o, data = d, label = k)))
  expect_true(gap_plot(ec("pie3DChart")$add_series(name = z, data = d, label = k)))
  expect_true(gap_plot(ec("pie")$add_series(name = v, data = d, label = k, color = c("FF0000", "00FF00", "0000FF"))))
  expect_true(gap_plot(ec("stock")$add_series(name = w, data = d, label = k)$add_series(name = h, data = d, label = k)$
    add_series(name = l, data = d, label = k)$add_series(name = c, data = d, label = k)))
})

test_that("plots honor tick skips, crossing values and plot area styles", {
  g <- gap_wb()
  expect_true(gap_plot(ec("bar")$add_series(name = v, data = g$d, label = k, dir = "bar")$
    set_x_axis(tick_mark_skip = 2)))
  expect_true(gap_plot(ec("bar")$add_series(name = v, data = g$d, label = k)$
    set_y_axis(crosses_at = 3)$set_x_axis(crosses_at = 2)))
  expect_true(gap_plot(ec("line")$add_series(name = v, data = g$d, label = k)$
    set_plot_style(fill = "EEEEEE", line = "000000", line_width = 2)))
  expect_true(gap_plot(ec("scatter")$add_series(name = v, data = g$d, label = w)$
    set_x_axis(minor_tick = "out")$set_y_axis(minor_tick = "out")))
})

test_that("extended chart references are quoted and checked", {
  priv <- ec("waterfall")$.__enclos_env__$private
  expect_null(priv$fix_quote(NULL))
  expect_equal(priv$fix_quote("Data!B1"), "'Data'!B1")
  expect_equal(priv$fix_quote("'Data'!B1"), "'Data'!B1")
  expect_equal(priv$fix_quote("B1"), "B1")
  expect_false(priv$is_ref(""))
  expect_false(priv$is_ref(NULL))
  expect_true(priv$is_ref("'Data'!B1"))
})

test_that("extended charts take legend font colors and rotated titles", {
  ce <- ec("waterfall")$add_series(name = "Data!B1", data = "Data!B2:B5", label = "Data!A2:A5")$
    set_legend_style(color = "FF0000", font_name = "Arial")
  expect_match(as.character(ce$render(1)), "FF0000", fixed = TRUE)
})

test_that("pugixml helpers handle bad paths, printing and non-string names", {
  doc <- read_xml("<a><!-- note --><b/></a>")
  expect_output(print(doc), "<a>")
  expect_error(xml_find_all(doc, "//["))
  expect_null(.Call(encharter:::C_pugi_add_child, xml_find_first(doc, "//b"), 1L, -1L))
  expect_type(fmt_txt2("plain"), "character")
})

gap_cx <- function(series) {
  paste0('<cx:chartSpace xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex" ',
         'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><cx:chartData>',
         '<cx:data id="0"><cx:numDim type="val"><cx:f>Data!$B$2:$B$5</cx:f></cx:numDim></cx:data></cx:chartData>',
         "<cx:chart><cx:plotArea><cx:plotAreaRegion>", series, "</cx:plotAreaRegion></cx:plotArea></cx:chart></cx:chartSpace>")
}

test_that("extended charts load a literal series name and need at least one series", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(a = letters[1:4], b = 1:4))
  wb$add_encharter(dims = "D2:L20", graph = ec("funnel")$
    add_series(name = "Data!B1", data = "Data!B2:B5", label = "Data!A2:A5"))
  ch <- encharter:::load_chartex(gap_cx(paste0(
    '<cx:series layoutId="funnel" uniqueId="{1}"><cx:tx><cx:txData><cx:v>Name</cx:v></cx:txData></cx:tx>',
    '<cx:dataId val="0"/></cx:series>')), wb, 1)
  expect_equal(ch$series_data[[1]]$name, "Name")
  expect_error(encharter:::load_chartex(gap_cx(""), wb, 1), "No series found")
})

test_that("data points without any formatting are skipped when loading", {
  bar <- gap_chart(paste0(
    '<c:barChart><c:barDir val="col"/><c:grouping val="clustered"/><c:ser><c:idx val="0"/><c:order val="0"/>',
    '<c:dPt><c:idx val="0"/></c:dPt>', "<c:val>", gap_lit(c(3, 4, 2)), "</c:val></c:ser>",
    '<c:axId val="1"/><c:axId val="2"/></c:barChart>'))
  expect_length(bar$series_data[[1]]$points, 0L)
})

test_that("extended charts plot pareto lines, missing name sheets and crowded treemaps", {
  set.seed(1)
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
    k = paste("A rather long category label", 1:30), v = round(runif(30, 1, 100))))
  f <- tempfile(fileext = ".png")
  grDevices::png(f, 600, 400)
  on.exit(grDevices::dev.off(), add = TRUE)
  pareto <- ec("clusteredColumn")$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$31", label = "Data!$A$2:$A$31", type = "clusteredColumn")$
    add_series(name = "Data!$B$1", data = "Data!$B$2:$B$31", label = "Data!$A$2:$A$31", type = "paretoLine")
  expect_no_error(plot(pareto, wb = wb))
  nosheet <- ec("funnel")$add_series(name = "Gone!$B$1", data = "Data!$B$2:$B$6", label = "Data!$A$2:$A$6")
  expect_no_error(plot(nosheet, wb = wb))
  tm <- ec("treemap")$add_series(name = "Data!$B$1", data = "Data!$B$2:$B$31", label = "Data!$A$2:$A$31")$
    set_data_label_style(show_val = TRUE, show_cat = TRUE)
  expect_no_error(plot(tm, wb = wb))
})

test_that("plot helpers skip colors in number formats and read ranges", {
  expect_equal(encharter:::plot_format(1.5, "[Red]0.0"), "1.5")
  expect_equal(encharter:::plot_format(1.5, "\"text\""), "text")
  expect_lte(encharter:::plot_scale(-10, -1)$max, 0)
  wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(f = factor(c("a", "b")), n = 1:2))
  expect_equal(encharter:::plot_read_ref(wb, "Data!$A$2:$A$3"), c("a", "b"))
})
