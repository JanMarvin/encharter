test_that("all EG_PlotAreaChoice chart types are routed", {
  # the complete set from CT_PlotArea in ECMA-376 dml-chart.xsd
  expect_setequal(
    ENCHARTER_STANDARD,
    c("areaChart", "area3DChart", "lineChart", "line3DChart", "stockChart",
      "radarChart", "scatterChart", "pieChart", "pie3DChart", "doughnutChart",
      "barChart", "bar3DChart", "ofPieChart", "surfaceChart", "surface3DChart",
      "bubbleChart")
  )
  for (type in ENCHARTER_STANDARD) {
    expect_s3_class(encharter(type), "Chart")
  }
})

test_that("new type aliases resolve", {
  expect_identical(encharter("pieOfPie")$type, "ofPieChart")
  expect_identical(encharter("bar3d")$type, "bar3DChart")
  expect_identical(encharter("column3d")$type, "bar3DChart")
  expect_identical(encharter("line3d")$type, "line3DChart")
  expect_identical(encharter("pie3d")$type, "pie3DChart")
  expect_identical(encharter("area3d")$type, "area3DChart")
  expect_identical(encharter("surface3d")$type, "surface3DChart")

  # barOfPie preselects the bar subtype
  bp <- encharter("barOfPie")
  expect_identical(bp$type, "ofPieChart")
  expect_identical(bp$of_pie_type, "bar")
})

test_that("ofPieChart renders per CT_OfPieChart", {
  ch <- encharter("ofPieChart")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9")$
    set_of_pie_options(second_size = 60, split_type = "pos", split_pos = 3)
  xml <- ch$render()

  expect_true(grepl("<c:ofPieChart>", xml))
  # ofPieType is the mandatory first child
  expect_true(grepl('<c:ofPieChart><c:ofPieType val="pie"/>', xml))
  expect_true(grepl('<c:splitType val="pos"/><c:splitPos val="3"/>', xml))
  expect_true(grepl('<c:secondPieSize val="60"/><c:serLines/>', xml))
  # pie charts carry no axes
  expect_false(grepl("<c:catAx>", xml))
  expect_false(grepl("<c:axId", xml))
})

test_that("ofPieChart custom split emits custSplit", {
  ch <- encharter("barOfPie")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9")$
    set_of_pie_options(split_type = "cust", split_pos = c(4, 6))
  xml <- ch$render()
  expect_true(grepl('<c:ofPieType val="bar"/>', xml))
  expect_true(grepl('<c:custSplit><c:secondPiePt val="4"/><c:secondPiePt val="6"/></c:custSplit>', xml))
  expect_false(grepl("<c:splitPos", xml))
})

test_that("set_of_pie_options validates against the schema", {
  ch <- encharter("ofPieChart")
  expect_error(ch$set_of_pie_options(type = "column"), "type")
  # ST_SecondPieSize: 5..200
  expect_error(ch$set_of_pie_options(second_size = 4), "second_size")
  expect_error(ch$set_of_pie_options(second_size = 201), "second_size")
  expect_error(ch$set_of_pie_options(split_type = "size"), "split_type")
  expect_error(ch$set_of_pie_options(split_type = "cust", split_pos = c(1.5, 2)), "split_pos")
})

test_that("bar3DChart renders barDir, shape, gapDepth, and three axes", {
  ch <- encharter("bar3DChart")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9", gap_width = 120)$
    set_3d_options(rot_x = 20, rot_y = 30, gap_depth = 80, shape = "cylinder")
  xml <- ch$render()

  expect_true(grepl('<c:bar3DChart><c:barDir val="col"/>', xml))
  expect_true(grepl('<c:gapWidth val="120"/><c:gapDepth val="80"/><c:shape val="cylinder"/>', xml))
  # view3D with user values
  expect_true(grepl('<c:view3D><c:rotX val="20"/><c:rotY val="30"/><c:rAngAx val="1"/></c:view3D>', xml))
  # three axis ids in the chart group and a series axis
  grp <- regmatches(xml, regexpr("<c:bar3DChart>.*?</c:bar3DChart>", xml))
  expect_length(gregexpr("<c:axId", grp)[[1]], 3L)
  expect_true(grepl("<c:serAx>", xml))
})

test_that("line3DChart and surface3DChart emit three axIds as required", {
  # CT_Line3DChart and CT_Surface3DChart: axId minOccurs = 3
  x1 <- encharter("line3DChart")$add_series(data = "Sheet1!B2:B9")$render()
  grp1 <- regmatches(x1, regexpr("<c:line3DChart>.*?</c:line3DChart>", x1))
  expect_length(gregexpr('<c:axId val="', grp1)[[1]], 3L)
  expect_true(grepl('<c:line3DChart><c:grouping val="standard"/>', x1))

  ch <- encharter("surface3DChart")$
    add_series(data = "Sheet1!B2:B9")$
    add_series(data = "Sheet1!C2:C9")
  x2 <- ch$render()
  grp2 <- regmatches(x2, regexpr("<c:surface3DChart>.*?</c:surface3DChart>", x2))
  expect_length(gregexpr('<c:axId val="', grp2)[[1]], 3L)
  expect_true(grepl('<c:surface3DChart><c:wireframe val="0"/>', x2))
  # surface3D keeps its value axis visible (unlike the contour surfaceChart)
  expect_false(grepl('<c:valAx><c:axId val="[0-9]+"/><c:scaling><c:orientation val="minMax"/></c:scaling><c:delete val="1"/>', x2))
})

test_that("pie3DChart renders without axes, position, or firstSliceAng", {
  ch <- encharter("pie3DChart")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9")$
    set_pie_options(rotation = 90)$
    set_data_label_style(show_val = TRUE)
  xml <- ch$render()

  expect_true(grepl('<c:pie3DChart><c:varyColors val="1"/>', xml))
  # CT_Pie3DChart has no firstSliceAng and no axId
  expect_false(grepl("firstSliceAng", xml))
  expect_false(grepl("<c:axId", xml))
  # dLblPos is not allowed on 3D chart groups
  expect_false(grepl("<c:dLblPos", xml))
  # default 3D view for pies
  expect_true(grepl('<c:view3D><c:rotX val="30"/><c:rotY val="0"/><c:rAngAx val="0"/><c:perspective val="30"/></c:view3D>', xml))
})

test_that("surfaceChart output is unchanged by the 3D additions", {
  ch <- encharter("surface")$
    add_series(data = "Sheet1!B2:B9")$
    add_series(data = "Sheet1!C2:C9")
  xml <- ch$render()
  expect_true(grepl('<c:view3D><c:rotX val="90"/><c:rotY val="0"/><c:rAngAx val="0"/><c:perspective val="0"/></c:view3D>', xml))
})

test_that("set_3d_options validates and applies hPercent/depthPercent", {
  ch <- encharter("surface3DChart")
  # ST_RotX / ST_RotY / ST_Perspective / ST_DepthPercent / ST_HPercent
  expect_error(ch$set_3d_options(rot_x = 91), "rot_x")
  expect_error(ch$set_3d_options(rot_y = 361), "rot_y")
  expect_error(ch$set_3d_options(perspective = 241), "perspective")
  expect_error(ch$set_3d_options(depth_percent = 19), "depth_percent")
  expect_error(ch$set_3d_options(h_percent = 501), "h_percent")
  expect_error(ch$set_3d_options(shape = "sphere"), "shape")
  expect_error(ch$set_3d_options(gap_depth = 501), "gap_depth")

  ch$add_series(data = "Sheet1!B2:B9")$add_series(data = "Sheet1!C2:C9")
  ch$set_3d_options(h_percent = 100, depth_percent = 150)
  xml <- ch$render()
  expect_true(grepl('<c:rotX val="15"/><c:hPercent val="100"/><c:rotY val="20"/><c:depthPercent val="150"/>', xml))
})

test_that("secondary axes on 3D types warn and fall back to primary", {
  ch <- encharter("bar3DChart")$
    add_series(data = "Sheet1!B2:B9")$
    add_series(data = "Sheet1!C2:C9", secondary = TRUE)
  expect_warning(xml <- ch$render(), "Secondary axes")
  # both series groups reference the same primary pair and no secondary
  # axis is rendered
  ids <- regmatches(xml, gregexpr('(?<=<c:axId val=")[0-9]+', xml, perl = TRUE))[[1]]
  expect_length(unique(ids), 3L)
})

test_that("tick skips and display units render into the axes", {
  ch <- encharter("bar")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9")$
    set_x_axis(tick_lbl_skip = 2, tick_mark_skip = 3)$
    set_y_axis(disp_units = "thousands")
  xml <- ch$render()
  expect_true(grepl('<c:tickLblSkip val="2"/><c:tickMarkSkip val="3"/>', xml))
  expect_true(grepl('<c:dispUnits><c:builtInUnit val="thousands"/></c:dispUnits>', xml))

  ch2 <- encharter("line")$
    add_series(data = "Sheet1!B2:B9")$
    set_y_axis(disp_units = 500)
  expect_true(grepl('<c:dispUnits><c:custUnit val="500"/></c:dispUnits>', ch2$render()))
})

test_that("extended data label options render", {
  ch <- encharter("pie")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9")$
    set_data_label_style(show_val = FALSE, show_percent = TRUE,
                         show_ser_name = TRUE, format = "0.0%")
  xml <- ch$render()
  expect_true(grepl('<c:numFmt formatCode="0.0%" sourceLinked="0"/>', xml))
  expect_true(grepl('<c:showVal val="0"/>', xml))
  expect_true(grepl('<c:showSerName val="1"/>', xml))
  expect_true(grepl('<c:showPercent val="1"/>', xml))
})

test_that("ChartEx label number format is applied", {
  cx <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9")$
    set_data_label_style(show_val = TRUE, format = "#,##0")
  expect_true(grepl('<cx:numFmt formatCode="#,##0" sourceLinked="0"/>', cx$render()))
})

test_that("ChartEx per-point colors render as cx:dataPt", {
  cx <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B5", label = "Sheet1!A2:A5",
               color = c("C00000", "4472C4", "70AD47", "FFC000"))
  xml <- as.character(cx$render())
  expect_true(grepl('<cx:dataPt idx="0"><cx:spPr><a:solidFill><a:srgbClr val="C00000"/>', xml))
  expect_true(grepl('<cx:dataPt idx="3">', xml))
  expect_false(grepl("<cx:dPt", xml))
  expect_lt(regexpr("<cx:dataPt", xml), regexpr("<cx:dataId", xml))
})

test_that("ChartEx single series color and R color names work like Chart", {
  cx <- encharter("funnel")$
    add_series(data = "Sheet1!B2:B5", label = "Sheet1!A2:A5",
               color = "steelblue", line_color = "red", line_width = 2)
  xml <- as.character(cx$render())
  expect_true(grepl('val="4682B4"', xml))
  expect_true(grepl('val="FF0000"', xml))
})

test_that("Chart-valid legend tokens no longer brick ChartEx", {
  # "tr" and side-style align values are valid for Chart but outside the
  # chartex enums (ST_SidePos has no "tr"; align is min/ctr/max)
  cx <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B5", label = "Sheet1!A2:A5")$
    set_legend_style(pos = "tr", align = "l")
  xml <- as.character(cx$render())
  expect_true(grepl('<cx:legend pos="t" align="min"', xml))

  cx2 <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B5", label = "Sheet1!A2:A5")$
    set_legend_style(pos = "b", align = "r")
  expect_true(grepl('<cx:legend pos="b" align="max"', as.character(cx2$render())))
})

test_that("trendline forward/backward/intercept and error bar axis render", {
  ch <- encharter("scatter")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9",
               trendline = list(type = "linear", forward = 2, backward = 1, intercept = 0),
               error_bars = list(type = "stdErr", axis = "x"))
  xml <- ch$render()
  expect_true(grepl('<c:forward val="2"/><c:backward val="1"/><c:intercept val="0"/>', xml))
  expect_true(grepl('<c:errDir val="x"/>', xml))

  expect_error(
    encharter("scatter")$add_series(data = "Sheet1!B2:B9",
                                    trendline = list(type = "linear", forward = -1)),
    "forward"
  )
  expect_error(
    encharter("scatter")$add_series(data = "Sheet1!B2:B9",
                                    error_bars = list(type = "stdErr", axis = "z")),
    "axis"
  )
})

test_that("invertIfNegative and sizeRepresents render", {
  ch <- encharter("bar")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9", invert_if_negative = TRUE)
  expect_true(grepl('<c:invertIfNegative val="1"/>', ch$render()))

  ch2 <- encharter("bubble")$
    add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9", weight = "Sheet1!C2:C9")$
    set_bubble_options(scale = 120, size_represents = "w")
  expect_true(grepl('<c:showNegBubbles val="0"/><c:sizeRepresents val="w"/>', ch2$render()))
  expect_error(
    encharter("bubble")$set_bubble_options(size_represents = "diameter"),
    "size_represents"
  )
})

test_that("color validation delegates to wb_color and stays byte-compatible", {
  a <- encharter("bar")$add_series(data = "Sheet1!B2:B5", color = "red")$render()
  b <- encharter("bar")$add_series(data = "Sheet1!B2:B5", color = "FF0000")$render()
  expect_identical(a, b)
  expect_error(
    encharter("bar")$add_series(data = "Sheet1!B2:B5", color = "not-a-color"),
    "invalid color"
  )
})

test_that("ChartEx axis children follow the CT_Axis sequence", {
  cx <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B6", label = "Sheet1!A2:A6")$
    set_x_axis(grid_lines = TRUE, major_tick = "out", format = "#,##0")
  xml <- as.character(cx$render())
  ax <- regmatches(xml, regexpr('<cx:axis id="0">.*?</cx:axis>', xml))
  # scaling < gridlines < tickMarks < tickLabels < numFmt < spPr < txPr;
  # take the LAST cx:spPr (gridlines carry a nested one)
  pos <- vapply(c("<cx:catScaling", "<cx:majorGridlines", "<cx:majorTickMarks",
                  "<cx:tickLabels", "<cx:numFmt", "<cx:txPr"),
                function(p) regexpr(p, ax, fixed = TRUE)[[1]], numeric(1))
  sp_all <- gregexpr("<cx:spPr", ax, fixed = TRUE)[[1]]
  pos <- sort(c(pos, spPr = max(sp_all)))
  expect_identical(names(pos)[6], "spPr")
  expect_true(all(diff(pos) > 0))
})

test_that("set_waterfall_colors patches the color style part", {
  cx <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B7", label = "Sheet1!A2:A7", subtotals = c(0, 5))$
    set_waterfall_colors(increase = "70AD47", decrease = "red", total = "A6A6A6")

  cs <- cx$color_xml
  # increase/decrease/total land in cycle slots 1-3, in order
  p1 <- regexpr('<a:srgbClr val="70AD47"/>', cs, fixed = TRUE)
  p2 <- regexpr('<a:srgbClr val="FF0000"/>', cs, fixed = TRUE)
  p3 <- regexpr('<a:srgbClr val="A6A6A6"/>', cs, fixed = TRUE)
  expect_true(all(c(p1, p2, p3) > 0))
  expect_true(p1 < p2 && p2 < p3)
  # untouched cycle entries and variations survive
  expect_true(grepl('<a:schemeClr val="accent4"/>', cs))
  expect_true(grepl("<cs:variation", cs))
  # the chart part itself carries no dataPt overrides
  expect_false(grepl("<cx:dataPt", as.character(cx$render())))
})

test_that("set_waterfall_colors supports partial updates and theme colors", {
  cx <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B7", label = "Sheet1!A2:A7")
  before <- cx$color_xml
  cx$set_waterfall_colors(total = "A6A6A6")
  cs <- cx$color_xml
  # slots 1 and 2 untouched, slot 3 replaced
  expect_true(grepl('<a:schemeClr val="accent1"/>\\s*<a:schemeClr val="accent2"/>\\s*<a:srgbClr val="A6A6A6"/>', cs))

  cx$set_waterfall_colors(increase = openxlsx2::wb_color(theme = 4))
  expect_true(startsWith(
    sub(".*?(<a:[a-zA-Z]+Clr[^>]*/>).*", "\\1", cx$color_xml),
    "<a:schemeClr"
  ))

  # no-op call leaves the part alone
  cx2 <- encharter("waterfall")$add_series(data = "Sheet1!B2:B7")
  ref <- cx2$color_xml
  cx2$set_waterfall_colors()
  expect_identical(cx2$color_xml, ref)
})

test_that("set_waterfall_colors validates its input", {
  cx <- encharter("waterfall")
  expect_error(cx$set_waterfall_colors(increase = c("111111", "222222")), "single color")
  expect_error(cx$set_waterfall_colors(decrease = "auto"), "concrete color")
  expect_error(cx$set_waterfall_colors(total = "not-a-color"), "invalid color")
  # a color style without three entries is rejected
  cx$color_xml <- '<cs:colorStyle meth="cycle" id="10"><a:schemeClr val="accent1"/></cs:colorStyle>'
  expect_error(cx$set_waterfall_colors(total = "A6A6A6"), "three color entries")
})

test_that("set_color_cycle replaces and extends the color style cycle", {
  cx <- encharter("treemap")$add_series(data = "Sheet1!B2:B9", label = "Sheet1!A2:A9")
  cx$set_color_cycle(c("C00000", "steelblue"))
  m <- regmatches(cx$color_xml, gregexpr('<a:[a-zA-Z]+Clr val="[^"]+"/>', cx$color_xml))[[1]]
  expect_identical(m[1], '<a:srgbClr val="C00000"/>')
  expect_identical(m[2], '<a:srgbClr val="4682B4"/>')
  expect_identical(m[3], '<a:schemeClr val="accent3"/>')

  # extending beyond the six shipped entries keeps order
  cx$set_color_cycle(sprintf("%06d", 111111 * 1:8))
  m2 <- regmatches(cx$color_xml, gregexpr('val="[0-9]{6}"', cx$color_xml))[[1]]
  expect_identical(m2, sprintf('val="%06d"', 111111 * 1:8))
  # variations survive
  expect_true(grepl("<cs:variation", cx$color_xml))

  expect_error(encharter("treemap")$set_color_cycle(character()), "at least one")
  expect_error(encharter("treemap")$set_color_cycle("auto"), "concrete color")
})

test_that("set_region_map_colors renders schema-valid valueColors", {
  cx <- encharter("regionMap")$
    add_series(data = "Sheet1!B2:B6", label = "Sheet1!A2:A6")$
    set_region_map_colors(min = "FFF2CC", mid = "ED7D31", max = "red")
  xml <- as.character(cx$render())
  expected <- paste0(
    "<cx:valueColors>",
    "<cx:minColor><a:srgbClr val=\"FFF2CC\"/></cx:minColor>",
    "<cx:midColor><a:srgbClr val=\"ED7D31\"/></cx:midColor>",
    "<cx:maxColor><a:srgbClr val=\"FF0000\"/></cx:maxColor>",
    "</cx:valueColors><cx:valueColorPositions count=\"3\"/>"
  )
  expect_true(grepl(expected, xml, fixed = TRUE))
  # sequence: after spPr slot, before dataId
  expect_lt(regexpr("<cx:valueColors>", xml), regexpr("<cx:dataId", xml))

  # two-color scale
  cx2 <- encharter("regionMap")$
    add_series(data = "Sheet1!B2:B6", label = "Sheet1!A2:A6")$
    set_region_map_colors(min = "FFF2CC", max = "C00000")
  x2 <- as.character(cx2$render())
  expect_true(grepl('<cx:valueColorPositions count="2"/>', x2))
  expect_false(grepl("midColor", x2))

  # only applied to regionMap series
  cx3 <- encharter("waterfall")$
    add_series(data = "Sheet1!B2:B6")
  cx3$region_colors <- list(min = "FFF2CC", mid = NULL, max = "C00000")
  expect_false(grepl("valueColors", as.character(cx3$render())))
})
