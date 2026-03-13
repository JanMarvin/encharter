#' Internal Helper: Backport of deparse1 for R < 4.0.0
#' @noRd
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

#' Internal Helper: Null Coalescing Operator
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
#' @param chart_obj An initialized \code{Chart} R6 object.
#'
#' @return The workbook object, invisibly.
#' @export
wb_add_chart <- function(wb, sheet = openxlsx2::current_sheet(), dims = NULL, chart_obj) {
  chart_xml <- chart_obj$render()
  wb$clone(deep = TRUE)$add_chart_xml(sheet = sheet, dims = dims, xml = chart_xml)
}

#' Add ChartEx to a Workbook
#'
#' @param wb A wbWorkbook object.
#' @param sheet The sheet name or index.
#' @param dims The cell range (e.g., "A1:G10") where the chart should be placed.
#' @param chart_obj An instance of the ChartEx R6 class.
#' @return The modified wbWorkbook object.
#' @export
wb_add_chartx <- function(wb, sheet = openxlsx2::current_sheet(), dims = "E2", chart_obj) {
  wb <- wb$clone(deep = TRUE)
  target_sheet <- wb$validate_sheet(sheet)

  # 1. Find the highest ID to prevent collisions
  existing_names <- wb$get_named_regions()$name
  chart_ids <- grep("^_xlchart\\.v1\\.", existing_names, value = TRUE)
  id_base <- 1L
  if (length(chart_ids) > 0) {
    id_nums <- as.integer(gsub("_xlchart\\.v1\\.", "", chart_ids))
    id_base <- max(id_nums, na.rm = TRUE) + 1L
  }

  chart_xml_rendered <- chart_obj$render(id_start = id_base)

  # 2. Add Drawing to the TARGET sheet
  drw_rel_id <- wb$worksheets[[target_sheet]]$relships$drawing
  next_rid <- if (length(drw_rel_id)) {
    openxlsx2:::get_next_id(wb$drawings_rels[[drw_rel_id]])
  } else { "rId1" }

  drawing_xml <- sprintf("<xdr:wsDr xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><xdr:absoluteAnchor><xdr:pos x=\"0\" y=\"0\"/><xdr:ext cx=\"4572000\" cy=\"2926080\"/><xdr:graphicFrame macro=\"\"><xdr:nvGraphicFramePr><xdr:cNvPr id=\"2\" name=\"Chart 1\"/><xdr:cNvGraphicFramePr/></xdr:nvGraphicFramePr><xdr:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"4572000\" cy=\"2926080\"/></xdr:xfrm><a:graphic><a:graphicData uri=\"http://schemas.microsoft.com/office/drawing/2014/chartex\"><cx:chart xmlns:cx=\"http://schemas.microsoft.com/office/drawing/2014/chartex\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"%s\"/></a:graphicData></a:graphic></xdr:graphicFrame><xdr:clientData/></xdr:absoluteAnchor></xdr:wsDr>", next_rid)

  wb$add_drawing(sheet = target_sheet, dims = dims, xml = drawing_xml)

  # 3. Handle Relationships & Chart XML
  chart_idx <- length(wb$charts$chartEx) + 1L
  rel_xml <- sprintf("<Relationship Id=\"%s\" Type=\"http://schemas.microsoft.com/office/2014/relationships/chartEx\" Target=\"../charts/chartEx%s.xml\"/>", next_rid, chart_idx)
  drw_id <- wb$worksheets[[target_sheet]]$relships$drawing
  wb$drawings_rels[[drw_id]] <- if (all(wb$drawings_rels[[drw_id]] == "")) rel_xml else c(wb$drawings_rels[[drw_id]], rel_xml)

  wb$charts <- rbind(wb$charts, data.frame(
    chart = "", colors = openxlsx2:::colors1_xml, style = openxlsx2:::styleplot_xml,
    rels = "", chartEx = as.character(chart_xml_rendered),
    relsEx = sprintf("<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId2\" Type=\"http://schemas.microsoft.com/office/2011/relationships/chartColorStyle\" Target=\"colors%s.xml\"/><Relationship Id=\"rId1\" Type=\"http://schemas.microsoft.com/office/2011/relationships/chartStyle\" Target=\"style%s.xml\"/></Relationships>", chart_idx, chart_idx),
    stringsAsFactors = FALSE
  ))

  # 4. Add Named Regions to the DATA sheet
  h_at <- attr(chart_xml_rendered, "head")
  b_at <- attr(chart_xml_rendered, "body")
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

  return(wb)
}
