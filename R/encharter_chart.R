#' R6 Class representing a Chart object for Excel workbooks
#'
#' @description
#' The `Chart` class provides a flexible interface to build Office OpenXML
#' (OOXML) chart objects. It allows for granular control over gridlines,
#' secondary axes, and combined chart types (e.g., Bar and Line) within a
#' single plot area.
#'
#' @details
#' This class is designed to work with the `openxlsx2` package by generating
#' the underlying XML required for the \code{add_chart_xml} method.
#'
#' @import R6
#' @import xml2
#' @import openxlsx2
#' @export
Chart <- R6::R6Class(
  "Chart",
  public = list(
    #' @field xml The raw xml2 object containing the chart space.
    xml = NULL,
    #' @field series_data A list containing all added data series and their styles.
    series_data = list(),
    #' @field type The default chart type for the object (e.g., "lineChart").
    type = NULL,
    #' @field chart_title List containing text and style for the main title.
    chart_title = list(text = NULL, style = list()),
    #' @field x_title List containing text and style for the X-axis.
    x_title  = list(text = NULL, style = list()),
    #' @field y_title List containing text and style for the primary Y-axis.
    y_title  = list(text = NULL, style = list()),
    #' @field y2_title List containing text and style for the secondary Y-axis.
    y2_title = list(text = NULL, style = list()),
    #' @field hole_size Integer. Size of the hole for doughnut charts (0-90).
    hole_size = 75,
    #' @field legend_params List of legend configuration settings.
    legend_params = list(pos = "r", overlay = "0", style = list()),
    #' @field label_params List of global data label configuration settings.
    label_params  = list(show_val = FALSE, show_cat = FALSE, show_legend_key = FALSE, pos = "t", style = list()),
    #' @field show_gridlines Logical. Toggle for major gridlines.
    show_gridlines = TRUE,
    #' @field axis_color Hex string for axis lines.
    axis_color = "000000",
    #' @field grid_color Hex string for gridlines.
    grid_color = "D9D9D9",
    #' @field chart_style List for the outer chart area styling.
    chart_style = list(fill = "FFFFFF", line = NULL, line_width = 1),
    #' @field plot_style List for the inner plot area styling.
    plot_style  = list(fill = NULL, line = NULL, line_width = 1),

    #' @description Initialize a new Chart object.
    #' @param type Initial chart type (e.g., "lineChart", "barChart", "pieChart").
    initialize = function(type = "lineChart") {
      self$type <- type
      self$xml <- xml2::read_xml(
        '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart"
                        xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                        xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006">
           <c:date1904 val="0" /><c:lang val="en-GB" /><c:roundedCorners val="0" />
           <mc:AlternateContent>
             <mc:Choice Requires="c14" xmlns:c14="http://schemas.microsoft.com/office/drawing/2007/8/2/chart">
               <c14:style val="102" />
             </mc:Choice>
             <mc:Fallback><c:style val="2" /></mc:Fallback>
           </mc:AlternateContent>
           <c:chart></c:chart>
         </c:chartSpace>'
      )
    },

    #' @description Set the chart's main title.
    #' @param text Title text string.
    #' @param ... Style arguments like `sz` (font size), `bold` (TRUE/FALSE), and `color`.
    set_chart_title = function(text, ...) { self$chart_title <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the X-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_x_title      = function(text, ...) { self$x_title      <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the primary Y-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_y_title      = function(text, ...) { self$y_title      <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the secondary Y-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_y2_title     = function(text, ...) { self$y2_title     <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the doughnut hole size.
    #' @param val Integer 0 to 90.
    set_hole_size    = function(val) { self$hole_size <- val; invisible(self) },

    #' @description Configure the chart legend.
    #' @param pos Legend position: 'r', 'l', 't', 'b', 'tr'.
    #' @param overlay Logical. Whether the legend overlays the plot area.
    #' @param ... Font styling for legend text.
    set_legend_style = function(pos = "r", overlay = FALSE, ...) {
      self$legend_params <- list(pos = pos, overlay = ifelse(overlay, "1", "0"), style = list(...))
      invisible(self)
    },

    #' @description Configure global data label settings.
    #' @param show_val Logical. Show numeric values.
    #' @param show_cat Logical. Show category names.
    #' @param show_legend_key Logical. Show legend key next to label.
    #' @param pos Label position (e.g., 't', 'b', 'ctr').
    #' @param ... Font styling for labels.
    set_data_label_style = function(show_val = TRUE, show_cat = FALSE, show_legend_key = FALSE, pos = "t", ...) {
      self$label_params <- list(show_val = show_val, show_cat = show_cat, show_legend_key = show_legend_key, pos = pos, style = list(...))
      invisible(self)
    },

    #' @description Style the outer chart background and border.
    #' @param fill Hex color for background.
    #' @param line Hex color for border line.
    #' @param line_width Numeric width of border line.
    set_chart_style = function(fill = "FFFFFF", line = NULL, line_width = 1) {
      self$chart_style <- list(fill = fill, line = line, line_width = line_width); invisible(self)
    },

    #' @description Style the inner plot area background.
    #' @param fill Hex color for background.
    #' @param line Hex color for border line.
    #' @param line_width Numeric width of border line.
    set_plot_style = function(fill = NULL, line = NULL, line_width = 1) {
      self$plot_style <- list(fill = fill, line = line, line_width = line_width); invisible(self)
    },

    #' @description Style axis lines and gridlines.
    #' @param color Hex color for the axis.
    #' @param grid_color Hex color for the gridlines.
    #' @param gridlines Logical. Show or hide gridlines.
    set_axis_style = function(color = "000000", grid_color = "D9D9D9", gridlines = TRUE) {
      self$axis_color <- color; self$grid_color <- grid_color; self$show_gridlines <- gridlines
      invisible(self)
    },

    #' @description Add a data series to the chart.
    #' @param header Excel range or string for series name (e.g., "Sheet1!$B$1").
    #' @param data Excel range for series values (e.g., "Sheet1!$B$2:$B$10").
    #' @param cat Excel range for category labels.
    #' @param z_data Excel range for bubble sizes (bubbleChart only).
    #' @param color Hex color for the series.
    #' @param type Chart type for this specific series (for combo charts).
    #' @param secondary Logical. Set to TRUE to move series to secondary axis.
    #' @param dir Bar direction ("col" or "bar").
    #' @param grouping Chart grouping ("standard", "stacked", "percentStacked").
    #' @param smooth Logical. Enable line smoothing.
    #' @param marker Marker type ("none", "circle", "square", etc.).
    #' @param ... Additional style parameters.
    add_series = function(header, data, cat = NULL, z_data = NULL, color = "4472C4", type = NULL,
                          secondary = FALSE, dir = "col", grouping = "standard",
                          smooth = FALSE, show_line = TRUE,
                          marker = "none", marker_size = 5, marker_fill = NULL,
                          marker_line = NULL, marker_line_width = 0.75,
                          show_val = NULL, show_cat = NULL) {

      `%||%` <- function(a, b) if (!is.null(a)) a else b
      sec_val <- if (isTRUE(secondary)) "y" else if (isFALSE(secondary)) "none" else as.character(secondary)

      self$series_data[[length(self$series_data) + 1]] <- list(
        header = header, data = data, cat = cat, z_data = z_data, color = color,
        type = type %||% self$type, sec_type = sec_val, dir = dir,
        grouping = grouping, smooth = smooth, show_line = show_line,
        marker = marker, marker_size = marker_size, marker_fill = marker_fill,
        marker_line = marker_line, marker_line_width = marker_line_width,
        show_val = show_val %||% self$label_params$show_val,
        show_cat = show_cat %||% self$label_params$show_cat,
        label_pos = self$label_params$pos,
        label_style = self$label_params$style
      )
      invisible(self)
    },

    #' @description Generate the final XML string for the chart.
    #' @return A character string containing the OOXML chart definition.
    render = function() {
      xml2::xml_find_all(self$xml, "c:spPr") |> xml2::xml_remove()
      private$apply_sp_pr(self$xml, self$chart_style)

      chart_root <- xml2::xml_find_first(self$xml, "//c:chart")
      xml2::xml_remove(xml2::xml_children(chart_root))

      if (!is.null(self$chart_title$text)) {
        t_node <- xml2::xml_add_child(chart_root, "c:title")
        private$add_title_content(t_node, self$chart_title$text, self$chart_title$style, default_sz = 1400)
        xml2::xml_add_child(t_node, "c:overlay", val = "0")
      }
      xml2::xml_add_child(chart_root, "c:autoTitleDeleted", val = if(is.null(self$chart_title$text)) "1" else "0")

      plot_area <- xml2::xml_add_child(chart_root, "c:plotArea")
      xml2::xml_add_child(plot_area, "c:layout")

      u_ids <- openxlsx2:::random_string(n = 4, length = 8, pattern = "[0-9]")
      id_prim_cat <- u_ids[1]; id_prim_val <- u_ids[2]
      id_sec_cat  <- u_ids[3]; id_sec_val  <- u_ids[4]

      private$current_idx <- 0
      combos <- unique(lapply(self$series_data, function(x) list(type = x$type, sec_type = x$sec_type)))

      has_axes <- FALSE
      for (combo in combos) {
        sub_series <- Filter(function(x) x$type == combo$type && x$sec_type == combo$sec_type, self$series_data)
        cat_id <- if(combo$sec_type %in% c("x", "xy")) id_sec_cat else id_prim_cat
        val_id <- if(combo$sec_type %in% c("y", "xy")) id_sec_val else id_prim_val

        private$render_series_node(plot_area, sub_series, combo$type, cat_id, val_id)
        if (!combo$type %in% c("pieChart", "doughnutChart")) has_axes <- TRUE
      }

      if (has_axes) {
        if (self$type %in% c("scatterChart", "bubbleChart")) {
          private$render_val_ax(plot_area, id_prim_cat, id_prim_val, "b", grid = FALSE, title_obj = self$x_title)
        } else {
          private$render_cat_ax(plot_area, id_prim_cat, id_prim_val, "b", delete = "0", title_obj = self$x_title)
        }
        private$render_val_ax(plot_area, id_prim_val, id_prim_cat, "l", grid = self$show_gridlines, title_obj = self$y_title)

        if (!is.null(self$y2_title$text) || any(vapply(self$series_data, function(x) x$sec_type %in% c("y", "xy"), logical(1)))) {
          private$render_val_ax(plot_area, id_sec_val, id_prim_cat, "r", grid = FALSE, title_obj = self$y2_title, crosses = "max")
        }
      }

      private$apply_sp_pr(plot_area, self$plot_style)
      legend <- xml2::xml_add_child(chart_root, "c:legend")
      xml2::xml_add_child(legend, "c:legendPos", val = self$legend_params$pos)
      xml2::xml_add_child(legend, "c:overlay", val = self$legend_params$overlay)
      if (length(self$legend_params$style) > 0) private$apply_text_style(legend, self$legend_params$style)

      return(as.character(self$xml))
    }
  ),

  private = list(
    current_idx = 0,

    apply_sp_pr = function(node, style) {
      if (is.null(style$fill) && is.null(style$line)) return()
      spPr <- xml2::xml_add_child(node, "c:spPr")
      if (!is.null(style$fill)) private$render_fill(xml2::xml_add_child(spPr, "a:solidFill"), style$fill)
      if (!is.null(style$line)) {
        ln <- xml2::xml_add_child(spPr, "a:ln", w = as.character(round(style$line_width * 12700)))
        private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), style$line)
      } else {
        xml2::xml_add_child(xml2::xml_add_child(spPr, "a:ln"), "a:noFill")
      }
    },

    render_series_node = function(plot_area, sub_series, type, cat_id, val_id) {
      c_node <- xml2::xml_add_child(plot_area, paste0("c:", type))
      `%||%` <- function(a, b) if (!is.null(a)) a else b

      if (type == "scatterChart") xml2::xml_add_child(c_node, "c:scatterStyle", val = "lineMarker")
      if (type == "barChart") xml2::xml_add_child(c_node, "c:barDir", val = sub_series[[1]]$dir %||% "col")
      if (!type %in% c("scatterChart", "pieChart", "doughnutChart", "bubbleChart")) {
        xml2::xml_add_child(c_node, "c:grouping", val = sub_series[[1]]$grouping %||% "standard")
      }

      xml2::xml_add_child(c_node, "c:varyColors", val = "1")

      if (type == "bubbleChart") {
        xml2::xml_add_child(c_node, "c:maxSize", val = "100")
        xml2::xml_add_child(c_node, "c:bubbleScale", val = "100")
        xml2::xml_add_child(c_node, "c:showNegBubbles", val = "0")
        xml2::xml_add_child(c_node, "c:sizeRepresents", val = "area")
      }

      if (type == "doughnutChart") {
        xml2::xml_add_child(c_node, "c:holeSize", val = as.character(self$hole_size))
      }

      for (s in sub_series) {
        ser <- xml2::xml_add_child(c_node, "c:ser")
        xml2::xml_add_child(ser, "c:idx", val = as.character(private$current_idx))
        xml2::xml_add_child(ser, "c:order", val = as.character(private$current_idx))
        private$current_idx <- private$current_idx + 1

        tx <- xml2::xml_add_child(ser, "c:tx")
        if (grepl("!", s$header)) xml2::xml_add_child(xml2::xml_add_child(tx, "c:strRef"), "c:f", s$header)
        else xml2::xml_add_child(tx, "c:v", s$header)

        if (type %in% c("bubbleChart", "doughnutChart", "pieChart")) {
          palette <- c("4472C4", "ED7D31", "A5A5A5", "FFC000", "5B9BD5", "70AD47", "264478", "9E480E")
          for (i in 0:7) {
            dPt <- xml2::xml_add_child(ser, "c:dPt")
            xml2::xml_add_child(dPt, "c:idx", val = as.character(i))
            sp_dpt <- xml2::xml_add_child(dPt, "c:spPr")
            private$render_fill(xml2::xml_add_child(sp_dpt, "a:solidFill"), palette[(i %% length(palette)) + 1])
          }
        }

        sp <- xml2::xml_add_child(ser, "c:spPr")
        if (type %in% c("barChart", "areaChart", "bubbleChart")) {
          if (type == "bubbleChart") {
            xml2::xml_add_child(sp, "a:noFill")
            xml2::xml_add_child(xml2::xml_add_child(sp, "a:ln"), "a:noFill")
          } else {
            private$render_fill(xml2::xml_add_child(sp, "a:solidFill"), s$color)
          }
        } else if (!type %in% c("pieChart", "doughnutChart")) {
          ln <- xml2::xml_add_child(sp, "a:ln", w = "28575")
          private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), s$color)
        }

        if (type %in% c("lineChart", "scatterChart")) {
          mkr <- xml2::xml_add_child(ser, "c:marker")
          xml2::xml_add_child(mkr, "c:symbol", val = s$marker %||% "none")
        }

        if (isTRUE(s$show_val) || isTRUE(s$show_cat)) {
          dlbls <- xml2::xml_add_child(ser, "c:dLbls")
          xml2::xml_add_child(dlbls, "c:dLblPos", val = s$label_pos %||% (if(type == "doughnutChart") "bestFit" else "t"))
          xml2::xml_add_child(dlbls, "c:showVal", val = if(s$show_val) "1" else "0")
          xml2::xml_add_child(dlbls, "c:showCatName", val = if(s$show_cat) "1" else "0")
          if (type == "bubbleChart") xml2::xml_add_child(dlbls, "c:showBubbleSize", val = "1")
        }

        if (type %in% c("scatterChart", "bubbleChart")) {
          xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(ser, "c:xVal"), "c:numRef"), "c:f", s$cat)
          xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(ser, "c:yVal"), "c:numRef"), "c:f", s$data)
          if (type == "bubbleChart" && !is.null(s$z_data)) {
            xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(ser, "c:bubbleSize"), "c:numRef"), "c:f", s$z_data)
          }
        } else {
          xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(ser, "c:cat"), "c:strRef"), "c:f", s$cat)
          xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(ser, "c:val"), "c:numRef"), "c:f", s$data)
        }

        if (type %in% c("lineChart", "scatterChart")) {
          xml2::xml_add_child(ser, "c:smooth", val = if(isTRUE(s$smooth)) "1" else "0")
        }
      }

      if (!type %in% c("pieChart", "doughnutChart")) {
        xml2::xml_add_child(c_node, "c:axId", val = cat_id)
        xml2::xml_add_child(c_node, "c:axId", val = val_id)
      }
    },

    add_title_content = function(node, text, style = list(), default_sz = 1000) {
      tx <- xml2::xml_add_child(node, "c:tx")
      rich <- xml2::xml_add_child(tx, "c:rich")
      xml2::xml_add_child(rich, "a:bodyPr"); xml2::xml_add_child(rich, "a:lstStyle")
      p <- xml2::xml_add_child(rich, "a:p")
      sz <- if(!is.null(style$sz)) style$sz * 100 else (if(!is.null(style$font_size)) style$font_size * 100 else default_sz)
      r <- xml2::xml_add_child(p, "a:r")
      rPr <- xml2::xml_add_child(r, "a:rPr", sz = as.character(sz))
      if (isTRUE(style$bold)) xml2::xml_set_attr(rPr, "b", "1")
      if (!is.null(style$color)) private$render_fill(xml2::xml_add_child(rPr, "a:solidFill"), style$color)
      xml2::xml_add_child(r, "a:t", text)
    },

    render_cat_ax = function(parent, id, cross_id, pos, delete = "0", title_obj = NULL) {
      ax <- xml2::xml_add_child(parent, "c:catAx")
      xml2::xml_add_child(ax, "c:axId", val = id)
      xml2::xml_add_child(xml2::xml_add_child(ax, "c:scaling"), "c:orientation", val = "minMax")
      xml2::xml_add_child(ax, "c:delete", val = delete); xml2::xml_add_child(ax, "c:axPos", val = pos)
      if (!is.null(title_obj$text) && delete == "0") {
        t_node <- xml2::xml_add_child(ax, "c:title")
        private$add_title_content(t_node, title_obj$text, title_obj$style)
        xml2::xml_add_child(t_node, "c:layout"); xml2::xml_add_child(t_node, "c:overlay", val = "0")
      }
      ln <- xml2::xml_add_child(xml2::xml_add_child(ax, "c:spPr"), "a:ln")
      private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), self$axis_color)
      xml2::xml_add_child(ax, "c:crossAx", val = cross_id); xml2::xml_add_child(ax, "c:lblOffset", val = "100")
    },

    render_val_ax = function(parent, id, cross_id, pos, grid, title_obj = NULL, crosses = "autoZero") {
      ax <- xml2::xml_add_child(parent, "c:valAx")
      xml2::xml_add_child(ax, "c:axId", val = id)
      xml2::xml_add_child(xml2::xml_add_child(ax, "c:scaling"), "c:orientation", val = "minMax")
      xml2::xml_add_child(ax, "c:delete", val = "0"); xml2::xml_add_child(ax, "c:axPos", val = pos)
      if (grid) {
        g <- xml2::xml_add_child(ax, "c:majorGridlines")
        private$render_fill(xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(g, "c:spPr"), "a:ln"), "a:solidFill"), self$grid_color)
      }
      if (!is.null(title_obj$text)) {
        t_node <- xml2::xml_add_child(ax, "c:title")
        private$add_title_content(t_node, title_obj$text, title_obj$style)
        xml2::xml_add_child(t_node, "c:layout"); xml2::xml_add_child(t_node, "c:overlay", val = "0")
      }
      ln <- xml2::xml_add_child(xml2::xml_add_child(ax, "c:spPr"), "a:ln")
      private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), self$axis_color)
      xml2::xml_add_child(ax, "c:crossAx", val = cross_id); xml2::xml_add_child(ax, "c:crosses", val = crosses); xml2::xml_add_child(ax, "c:crossBetween", val = "between")
    },

    apply_text_style = function(node, s) {
      txPr <- xml2::xml_add_child(node, "c:txPr")
      xml2::xml_add_child(txPr, "a:bodyPr"); xml2::xml_add_child(txPr, "a:lstStyle")
      defRPr <- xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(txPr, "a:p"), "a:pPr"), "a:defRPr")
      sz <- if(!is.null(s$sz)) s$sz * 100 else (if(!is.null(s$font_size)) s$font_size * 100 else 900)
      xml2::xml_set_attr(defRPr, "sz", as.character(sz))
      if (isTRUE(s$bold)) xml2::xml_set_attr(defRPr, "b", "1")
      if (!is.null(s$color)) private$render_fill(xml2::xml_add_child(defRPr, "a:solidFill"), s$color)
    },

    render_fill = function(node, color_val) {
      if (length(color_val) == 0 || is.null(color_val) || color_val == "") color_val <- "000000"
      `%||%` <- function(a, b) if (!is.null(a)) a else b
      hex <- if (inherits(color_val, "wbColour")) attr(color_val, "rgb") %||% as.character(color_val) else as.character(color_val)
      clean <- toupper(gsub("^#", "", hex))
      if (nchar(clean) == 8) clean <- substr(clean, 3, 8)
      xml2::xml_add_child(node, "a:srgbClr", val = clean)
    }
  )
)

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
