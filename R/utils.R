#' Internal Helper: Backport of deparse1 for R < 4.0.0
#' @noRd
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

#' Internal Helper: Null Coalescing Operator
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

xml_unescape <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", '"', x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  x
}

to_abs_ref <- function(x) {
  # If it's NULL, length 0, or doesn't look like a reference (no !), return as-is
  if (is.null(x) || length(x) == 0 || !any(grepl("!", x))) {
    return(x)
  }

  sapply(x, function(ref) {
    if (!is.character(ref) || !grepl("!", ref)) return(ref)

    # Split to keep sheet name separate from coordinates
    parts <- strsplit(ref, "!", fixed = TRUE)[[1]]
    sheet <- gsub("^'|'$", "", parts[1]) # Clean existing quotes
    # unescape for wb_data() and escape afterwards
    sheet <- xml_escape(xml_unescape(sheet))
    range <- parts[2]

    # Only add $ to coordinates, not the sheet name
    # Regex: find letters/numbers not preceded by $
    fixed_range <- gsub("(?<!\\$)([A-Z]+)(?<!\\$)([0-9]+)", "$\\1$\\2", range, perl = TRUE)

    sprintf("'%s'!%s", sheet, fixed_range)
  }, USE.NAMES = FALSE)
}

normalize_encharter_type <- function(type) {
  # Keep original for the fallback to preserve camelCase if user was precise
  type_orig <- type
  type_low  <- tolower(as.character(type))

  # Map familiar R names to OOXML types (Named Vector is cleaner than List here)
  type_map <- c(
    # bar / column
    "bar"             = "barChart",
    "barchart"        = "barChart",
    "barplot"         = "barChart",
    "col"             = "barChart",
    "column"          = "barChart",
    "histogram"       = "barChart",
    "hist"            = "barChart",
    # line
    "line"            = "lineChart",
    "linechart"       = "lineChart",
    # area
    "area"            = "areaChart",
    "areachart"       = "areaChart",
    # scatter
    "scatter"         = "scatterChart",
    "scatterchart"    = "scatterChart",
    "point"           = "scatterChart",
    "xy"              = "scatterChart",
    # bubble
    "bubble"          = "bubbleChart",
    "bubblechart"     = "bubbleChart",
    # pie
    "pie"             = "pieChart",
    "piechart"        = "pieChart",
    # doughnut
    "doughnut"        = "doughnutChart",
    "donut"           = "doughnutChart",
    "doughnutchart"   = "doughnutChart",
    # radar
    "radar"           = "radarChart",
    "radarchart"      = "radarChart",
    "spider"          = "radarChart",
    # surface
    "surface"         = "surfaceChart",
    "surfacechart"    = "surfaceChart",
    # stock
    "stock"           = "stockChart",
    "stockchart"      = "stockChart",
    # extended
    "box"             = "boxWhisker",
    "boxplot"         = "boxWhisker",
    "boxwhisker"      = "boxWhisker",
    "map"             = "regionMap",
    "regionmap"       = "regionMap",
    "funnel"          = "funnel",
    "treemap"         = "treemap",
    "sunburst"        = "sunburst",
    "pareto"          = "paretoLine",
    "paretoline"      = "paretoLine",
    "clusteredcolumn" = "clusteredColumn"
  )


  if (!is.null(type) && type_low %in% names(type_map)) {
    return(unname(type_map[type_low]))
  }

  # Return original to preserve camelCase (e.g. "barChart") for match.arg
  type_orig
}

#' Internal helper to normalize directions and positions
#' @noRd
normalize_encharter_string <- function(x) {
  if (is.null(x)) return(NULL)

  switch(trimws(tolower(as.character(x))),
         # Directions
         "v"          = "col",
         "vertical"   = "col",
         "h"          = "bar",
         "horizontal" = "bar",
         # Positions
         "left"       = "l",
         "right"      = "r",
         "top"        = "t",
         "bottom"     = "b",
         "center"     = "ctr",
         # Return original if no match
         x
  )
}

#' a trimmed down styleplot_xml
#' @noRd
styleplot_xml <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<cs:chartStyle xmlns:cs="http://schemas.microsoft.com/office/drawing/2012/chartStyle"
               xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" id="201">
  <cs:axisTitle><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:axisTitle>
  <cs:categoryAxis><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:categoryAxis>
  <cs:chartArea><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"></cs:fontRef><cs:spPr><a:solidFill><a:schemeClr val="bg1" /></a:solidFill></cs:spPr><a:schemeClr val="tx1"/><cs:defRPr/></cs:chartArea>
  <cs:dataLabel><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:dataLabel>
  <cs:dataLabelCallout><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:dataLabelCallout>
  <cs:dataPoint><cs:lnRef idx="0"/><cs:fillRef idx="1"><cs:styleClr val="auto"/></cs:fillRef><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:dataPoint>
  <cs:dataPoint3D><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:dataPoint3D>
  <cs:dataPointLine><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:dataPointLine>
  <cs:dataPointMarker><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:dataPointMarker>
  <cs:dataPointMarkerLayout symbol="circle" size="5"/>
  <cs:dataPointWireframe><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:dataPointWireframe>
  <cs:dataTable><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:dataTable>
  <cs:downBar><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:downBar>
  <cs:dropLine><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:dropLine>
  <cs:errorBar><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:errorBar>
  <cs:floor><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:floor>
  <cs:gridlineMajor><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:gridlineMajor>
  <cs:gridlineMinor><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:gridlineMinor>
  <cs:hiLoLine><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:hiLoLine>
  <cs:leaderLine><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:leaderLine>
  <cs:legend><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:legend>
  <cs:plotArea><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:plotArea>
  <cs:plotArea3D><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:plotArea3D>
  <cs:seriesAxis><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:seriesAxis>
  <cs:seriesLine><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:seriesLine>
  <cs:title><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:title>
  <cs:trendline><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:trendline>
  <cs:trendlineLabel><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:trendlineLabel>
  <cs:upBar><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:upBar>
  <cs:valueAxis><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef><cs:defRPr/></cs:valueAxis>
  <cs:wall><cs:lnRef idx="0"/><cs:fillRef idx="0"/><cs:effectRef idx="0"/><cs:fontRef idx="minor"><a:schemeClr val="tx1"/></cs:fontRef></cs:wall>
</cs:chartStyle>'

#' A colors xml file
#' @noRd
colors1_xml <- "<cs:colorStyle xmlns:cs=\"http://schemas.microsoft.com/office/drawing/2012/chartStyle\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" meth=\"cycle\" id=\"10\">
<a:schemeClr val=\"accent1\"/>
<a:schemeClr val=\"accent2\"/>
<a:schemeClr val=\"accent3\"/>
<a:schemeClr val=\"accent4\"/>
<a:schemeClr val=\"accent5\"/>
<a:schemeClr val=\"accent6\"/>
<cs:variation/>
<cs:variation><a:lumMod val=\"60000\"/></cs:variation>
<cs:variation><a:lumMod val=\"80000\"/><a:lumOff val=\"20000\"/></cs:variation>
<cs:variation><a:lumMod val=\"80000\"/></cs:variation>
<cs:variation><a:lumMod val=\"60000\"/><a:lumOff val=\"40000\"/></cs:variation>
<cs:variation><a:lumMod val=\"50000\"/></cs:variation>
<cs:variation><a:lumMod val=\"70000\"/><a:lumOff val=\"30000\"/></cs:variation>
<cs:variation><a:lumMod val=\"70000\"/></cs:variation>
<cs:variation><a:lumMod val=\"50000\"/><a:lumOff val=\"50000\"/></cs:variation>
</cs:colorStyle>"


### borrowed from openxlsx2 (MIT) to avoid more exports ###

as_binary <- function(x) {
  # To be used within a function
  if (any(!x %in% list(0, 1, FALSE, TRUE))) {
    stop(deparse(x), " must be 0, 1, FALSE, or TRUE", call. = FALSE)
  }

  as.integer(x)
}

as_xml_attr <- function(x) {

  if (is.null(x)) {
    return("")
  }

  if (inherits(x, "logical")) {
    x <- as_binary(x)
  }

  if (inherits(x, "character")) {
    x
  } else {
    op <- options(OutDec = ".")
    on.exit(options(op), add = TRUE)
    as.character(x)
  }
}

#' create a color used in create_shape
#' @param color a [wb_color()] object
#' @param transparency an integer value
#' @noRd
get_color <- function(color, transparency = 0) {

  alignment_map <- c(
    "0" =   "bg1",
    "1" =   "tx1",
    "2" =   "bg2",
    "3" =   "tx2",
    "4" =   "accent1",
    "5" =   "accent2",
    "6" =   "accent3",
    "7" =   "accent4",
    "8" =   "accent5",
    "9" =   "accent6",
    "10" =  "hlink",
    "11" =  "folHlink",
    "12" =  "phClr",
    "13" =  "dk1",
    "14" =  "lt1",
    "15" =  "dk2",
    "16" =  "lt2"
  )

  if (inherits(color, "wbColour")) {
    if ("rgb" %in% names(color)) {
      color <- sprintf(
        '<a:solidFill>
        <a:srgbClr val="%s">
          <a:alpha val="%s" />
        </a:srgbClr>
      </a:solidFill>',
        substr(c(color["rgb"]), 3, 8),
        min(99, (100 - transparency)) * 1000
      )
    } else if ("theme" %in% names(color)) {
      color <- sprintf(
        '<a:solidFill>
        <a:schemeClr val="%s">
          <a:alpha val="%s" />
        </a:schemeClr>
      </a:solidFill>',
        alignment_map[color["theme"]],
        min(99, (100 - transparency)) * 1000
      )
    } else {
      warning("currently only rgb and theme colors are supported")
      color <- ""
    }
  } else {
    color <- ""
  }
  color
}

#' string styling used in create_shape()
#'
#' handles bold, italic, strike, size, font, charset
#' unhandled charset, outline, vert_align
#' @param txt input, character or [fmt_txt()]
#' @param text_color a [wb_color()]
#' @param transparency an integer value
#' @noRd
fmt_txt2 <- function(txt, text_color = "", transparency = 0) {
  if (!inherits(txt, "fmt_txt")) {
    txt <- openxlsx2::fmt_txt(txt)
  }

  txts <- openxlsx2::xml_node(txt, "r")

  out <- NULL
  for (txt in txts) { # no need to check for <b val="1"/>
    bold      <- ifelse(grepl("<b/>", txt), "1", "")
    italic    <- ifelse(grepl("<i/>", txt), "1", "")
    strike    <- ifelse(grepl("<strike/>", txt), "sngStrike", "")
    underline <- ifelse(grepl("<u/>", txt), "sng", "")

    color     <- sapply(openxlsx2::xml_attr(txt, "r", "rPr", "color"), "[")
    if (length(color) == 0) {
      color   <- get_color(text_color, transparency)
    } else {
      color     <- get_color(wb_color(color), transparency) # tint?
    }

    sz <- sapply(openxlsx2::xml_attr(txt, "r", "rPr", "sz"), "[")
    if (length(sz)) sz        <- as.integer(sz[["val"]]) * 100

    font <- sapply(openxlsx2::xml_attr(txt, "r", "rPr", "rFont"), "[")
    charset <- sapply(openxlsx2::xml_attr(txt, "r", "rPr", "charset"), "[")

    if (length(charset) == 0) charset <- c(val = "0")
    if (length(font)) {
      font <- c(
        sprintf('<a:latin typeface="%s" charset="%s" />', font[["val"]], charset[["val"]]),
        sprintf('<a:cs typeface="%s" charset="%s" />', font[["val"]], charset[["val"]])
      )
    } else {
      font <- NULL
    }

    rPr <- openxlsx2::xml_node_create(
      "a:rPr",
      xml_attributes = c(
        b = as_xml_attr(bold),
        i = as_xml_attr(italic),
        strike = as_xml_attr(strike),
        sz = as_xml_attr(sz),
        u  = as_xml_attr(underline)
      ),
      xml_children = c(color, font)
    )


    text <- openxlsx2::xml_value(txt, "r", "t")
    text <- openxlsx2::xml_node_create("a:t", xml_children = text)
    ar   <- openxlsx2::xml_node_create("a:r", xml_children = c(rPr, text))

    out <- c(out, ar)
  }

  paste0(out, collapse = "")
}
