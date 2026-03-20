#' Internal Helper: Backport of deparse1 for R < 4.0.0
#' @noRd
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

#' Internal Helper: Null Coalescing Operator
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
    "barplot"   = "barChart",
    "bubble"    = "bubbleChart",
    "histogram" = "barChart",
    "hist"      = "barChart",
    "line"      = "lineChart",
    "scatter"   = "scatterChart",
    "point"     = "scatterChart",
    "area"      = "areaChart",
    "pie"       = "pieChart",
    "doughnut"  = "doughnutChart",
    "radar"     = "radarChart",
    "surface"   = "surfaceChart",
    "box"       = "boxWhisker",
    "boxplot"   = "boxWhisker",
    "map"       = "regionMap",
    "pareto"    = "paretoLine"
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
  x <- trimws(tolower(as.character(x)))

  switch(x,
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
#' @export
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
#' @export
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

## these shall be moved to openxlsx2

#' Add a Chart object to a workbook sheet
#'
#' @description
#' Renders a \code{Chart} R6 object and injects the resulting XML into an
#' \code{openxlsx2} workbook at the specified location.
#'
#' @param wb An \code{openxlsx2} workbook object.
#' @param sheet Sheet name or index where the chart will be placed.
#' @param dims Character string defining the cell range (e.g., "E2:M20").
#' @param graph An initialized \code{Chart} R6 object.
#'
#' @return The workbook object, invisibly.
#' @export
wb_add_encharter <- function(wb, sheet = openxlsx2::current_sheet(), dims = NULL, graph) {

  wb <- wb$clone()

  if (inherits(graph, "Chart")) {
    chart_xml <- graph$render(u_ids = openxlsx2:::random_string(n = 5, length = 8, pattern = "[0-9]"))
    return(wb$add_chart_xml(sheet = sheet, dims = dims, xml = chart_xml))
  } else if (inherits(graph, "ChartEx")) {

    wb <- wb$clone(deep = TRUE)

    # 1. Find the highest ID to prevent collisions
    existing_names <- wb$get_named_regions()$name
    chart_ids <- grep("^_xlchart\\.v1\\.", existing_names, value = TRUE)
    id_base <- 1L
    if (length(chart_ids) > 0) {
      id_nums <- as.integer(gsub("_xlchart\\.v1\\.", "", chart_ids))
      id_base <- max(id_nums, na.rm = TRUE) + 1L
    }

    chart_xml <- graph$render(id_start = id_base, guid = openxlsx2:::st_guid())

    # 4. Add Named Regions to the DATA sheet
    h_at <- attr(chart_xml, "head")
    b_at <- attr(chart_xml, "body")
    all_refs <- c(h_at, b_at)

    for (i in seq_along(all_refs)) {
      ref <- all_refs[i]

      # Check: Must contain '!' and have at least one character after it
      # This prevents literal strings (e.g. "Total!") from being treated as ranges
      is_valid_ref <- !is.na(ref) && ref != "" && grepl("!.+", ref)
      if (!is_valid_ref) next

      sheet_part <- gsub("^'?(.*?)'?!.*$", "\\1", ref)
      range_part <- gsub("^.*!", "", ref)
      wb$add_named_region(sheet = sheet_part, dims = range_part, name = names(all_refs)[i], hidden = "1")
    }
  }

  wb$add_chart_xml(sheet = sheet, dims = dims, xml = chart_xml, color = colors1_xml, style = styleplot_xml)
}
