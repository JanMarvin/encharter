#' Read and write chart templates (.crtx)
#'
#' @description
#' A chart template is a zip package holding a chart part without data
#' references. Excel writes them from "Save as Template"; other applications
#' read them as well. `encharter_to_crtx()` saves a `Chart` as a template,
#' `encharter_from_crtx()` reads a template into a `Chart` without series,
#' and `Chart$apply_crtx()` copies the styling of a template onto an existing
#' chart.
#'
#' @details
#' A template read with `encharter_from_crtx()` keeps the per-series styling
#' (color, line, marker) of the template. Series added to it with
#' `$add_series()` take that styling in order, for the arguments not given
#' explicitly; when the template has fewer series than added, its styles are
#' reused from the start.
#'
#' Templates cover standard charts only; extended charts (`ChartEx`) are not
#' supported. Templates written by Excel also carry a theme override and a
#' preview image, which are not read; a template written here holds the
#' chart part only.
#'
#' @param chart A `Chart` object.
#' @param path Path of the `.crtx` file.
#' @return `encharter_to_crtx()` returns `path` invisibly;
#'   `encharter_from_crtx()` returns a `Chart` object.
#' @examples
#' wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(x = data.frame(
#'   Month = month.abb[1:6], Sales = c(120, 135, 128, 160, 175, 190)
#' ))
#' chart <- ec("bar")$set_chart_title("Sales", bold = TRUE)$
#'   set_y_axis(grid_lines = TRUE, grid_color = "EEEEEE")$
#'   add_series(name = Sales, data = openxlsx2::wb_data(wb), label = Month, color = "2E4057")
#' tmp <- tempfile(fileext = ".crtx")
#' encharter_to_crtx(chart, tmp)
#'
#' chart2 <- encharter_from_crtx(tmp)
#' chart2$add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7")
#' chart2$series_data[[1]]$line$color
#' @name encharter_crtx
NULL

CRTX_CONTENT_TYPES <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
  '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
  '<Default Extension="xml" ContentType="application/xml"/>',
  '<Override PartName="/chart/chart.xml" ContentType="application/vnd.openxmlformats-officedocument.drawingml.chart+xml"/>',
  "</Types>"
)

CRTX_RELS <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="chart/chart.xml"/>',
  "</Relationships>"
)

#' @rdname encharter_crtx
#' @export
encharter_to_crtx <- function(chart, path) {
  if (!inherits(chart, "Chart")) stop("'chart' must be a Chart object; templates do not cover ChartEx.", call. = FALSE)
  if (!grepl("\\.crtx$", path, ignore.case = TRUE)) {
    warning("'path' does not end in .crtx; the file will not be offered as a template.", call. = FALSE)
  }

  doc <- read_xml(as.character(chart$render()))
  # a template carries the formatting of the series but no data; the
  # reference elements stay and are emptied
  for (ser in xml_find_all(doc, "//c:ser")) {
    for (nd in xml_children(ser)) {
      if (!xml_name(nd) %in% c("c:tx", "c:cat", "c:val", "c:xVal", "c:yVal", "c:bubbleSize")) next
      ref <- xml_children(nd)
      if (length(ref) == 0) next
      ref_name <- if (xml_name(ref[[1]]) %in% c("c:v", "c:numLit")) "c:numRef" else if (xml_name(ref[[1]]) == "c:strLit") "c:strRef" else xml_name(ref[[1]])
      xml_remove(ref)
      xml_add_child(nd, ref_name)
    }
  }
  xml_remove(xml_find_all(doc, "/c:chartSpace/c:externalData"))
  xml_remove(xml_find_all(doc, "/c:chartSpace/c:pivotSource"))

  tmp <- tempfile("crtx")
  dir.create(file.path(tmp, "chart"), recursive = TRUE)
  dir.create(file.path(tmp, "_rels"))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(CRTX_CONTENT_TYPES, file.path(tmp, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(CRTX_RELS, file.path(tmp, "_rels", ".rels"), useBytes = TRUE)
  writeLines(
    paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>', as.character(doc)),
    file.path(tmp, "chart", "chart.xml"), useBytes = TRUE
  )

  out <- normalizePath(path, mustWork = FALSE)
  if (file.exists(out)) unlink(out)
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)
  status <- utils::zip(out, files = c("[Content_Types].xml", "_rels/.rels", "chart/chart.xml"), flags = "-r9Xq")
  if (!identical(status, 0L) || !file.exists(out)) stop("Could not write the zip file ", path, call. = FALSE)
  invisible(path)
}

#' @rdname encharter_crtx
#' @export
ec_to_crtx <- encharter_to_crtx

# The chart XML inside a template file
crtx_chart_xml <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  files <- utils::unzip(path, list = TRUE)$Name
  part <- grep("^chart/chart[0-9]*\\.xml$", files, value = TRUE)
  if (length(part) == 0) stop("No chart part found in ", path, call. = FALSE)
  tmp <- tempfile("crtx")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = part[1], exdir = tmp)
  paste(readLines(file.path(tmp, part[1]), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' @rdname encharter_crtx
#' @export
encharter_from_crtx <- function(path) {
  chart <- load_chart(crtx_chart_xml(path))
  chart$template <- chart$series_data
  chart$series_data <- list()
  chart
}

#' @rdname encharter_crtx
#' @export
ec_from_crtx <- encharter_from_crtx
