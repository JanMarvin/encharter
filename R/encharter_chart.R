#' R6 Class representing a Chart object for Spreadsheets
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
#' @importFrom xml2 read_xml xml_remove xml_add_child
#'  xml_find_first xml_find_all xml_set_attr
#' @importFrom openxlsx2 wb_color
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
    #' @field x2_title List containing text and style for the secondary X-axis.
    x2_title = list(text = NULL, style = list()),
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
    #' @field chart_style List for the outer chart area styling.
    chart_style = list(fill = "FFFFFF", line = NULL, line_width = 1),
    #' @field plot_style List for the inner plot area styling.
    plot_style  = list(fill = NULL, line = NULL, line_width = 1),
    #' @field axis_params Internal list for scaling, units, and formatting.
    axis_params = list(
      x  = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, format = NULL, log_base = NULL, color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2"),
      x2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, format = NULL, log_base = NULL, color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE),
      y  = list(min = NULL, max = NULL, major = NULL, minor = NULL, format = NULL, log_base = NULL, color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = TRUE),
      y2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, format = NULL, log_base = NULL, color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE)
    ),

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
    #' @param ... Style arguments like `sz` (font size), `bold` (TRUE/FALSE), `color`, and `name` (font name).
    set_chart_title = function(text, ...) { self$chart_title <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the X-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_x_title      = function(text, ...) { self$x_title      <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the secondary X-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_x2_title     = function(text, ...) { self$x2_title     <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the primary Y-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_y_title      = function(text, ...) { self$y_title      <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set the secondary Y-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_y2_title     = function(text, ...) { self$y2_title     <- list(text = text, style = list(...)); invisible(self) },

    #' @description Set Primary X-axis scaling, units, and format.
    #' @param min Minimum value for the axis.
    #' @param max Maximum value for the axis.
    #' @param major Numeric value for major unit interval.
    #' @param minor Numeric value for minor unit interval.
    #' @param major_time Time unit for major steps ("days", "months", "years"). Used for date axes.
    #' @param minor_time Time unit for minor steps ("days", "months", "years"). Used for date axes.
    #' @param base_time Base time unit for date axes ("days", "months", "years").
    #' @param format Excel number format string (e.g., "#,##0" or "yyyy-mm-dd").
    #' @param log_base Base for logarithmic scaling (e.g., 10).
    #' @param color Hex color for the axis lines.
    #' @param rot Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the gridlines.
    #' @param gridlines,minor_gridlines Logical. Show or hide gridlines.
    set_x_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                          major_time = NULL, minor_time = NULL, base_time = NULL,
                          format = NULL, log_base = NULL, color = NULL, rot = NULL,
                          grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL) {
      params <- list(min = min, max = max, major = major, minor = minor,
                     major_time = major_time, minor_time = minor_time, base_time = base_time,
                     format = format, log_base = log_base, color = color, rot = rot,
                     grid_color = grid_color, gridlines = gridlines,
                     minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines)
      self$axis_params$x <- modifyList(self$axis_params$x, Filter(Negate(is.null), params))
      invisible(self)
    },

    #' @description Set Primary Y-axis scaling, units, and format.
    #' @param min Minimum value for the axis.
    #' @param max Maximum value for the axis.
    #' @param major Numeric value for major unit interval.
    #' @param minor Numeric value for minor unit interval.
    #' @param format Excel number format string.
    #' @param log_base Base for logarithmic scaling.
    #' @param color Hex color for the axis lines.
    #' @param rot Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the gridlines.
    #' @param gridlines,minor_gridlines Logical. Show or hide gridlines.
    set_y_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL, format = NULL, log_base = NULL,
                          color = NULL, rot = NULL, grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL) {
      params <- list(min = min, max = max, major = major, minor = minor, format = format, log_base = log_base,
                     color = color, rot = rot, grid_color = grid_color, gridlines = gridlines,
                     minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines)
      self$axis_params$y <- modifyList(self$axis_params$y, Filter(Negate(is.null), params))
      invisible(self)
    },

    #' @description Set Secondary Y-axis scaling, units, and format.
    #' @param min Minimum value for the axis.
    #' @param max Maximum value for the axis.
    #' @param major Numeric value for major unit interval.
    #' @param minor Numeric value for minor unit interval.
    #' @param format Excel number format string.
    #' @param log_base Base for logarithmic scaling.
    #' @param color Hex color for the axis lines.
    #' @param rot Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the gridlines.
    #' @param gridlines,minor_gridlines Logical. Show or hide gridlines.
    set_y2_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL, format = NULL, log_base = NULL,
                           color = NULL, rot = NULL, grid_color = NULL, gridlines = NULL,
                           minor_grid_color = NULL, minor_gridlines = NULL) {
      params <- list(min = min, max = max, major = major, minor = minor, format = format, log_base = log_base,
                     color = color, rot = rot, grid_color = grid_color, gridlines = gridlines,
                     minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines)
      self$axis_params$y2 <- modifyList(self$axis_params$y2, Filter(Negate(is.null), params))
      invisible(self)
    },

    #' @description Set Secondary X-axis scaling, units, and format.
    #' @param min Minimum value for the axis.
    #' @param max Maximum value for the axis.
    #' @param major Numeric value for major unit interval.
    #' @param minor Numeric value for minor unit interval.
    #' @param format Excel number format string.
    #' @param log_base Base for logarithmic scaling.
    #' @param color Hex color for the axis lines.
    #' @param rot Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the gridlines.
    #' @param gridlines,minor_gridlines Logical. Show or hide gridlines.
    set_x2_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL, format = NULL, log_base = NULL,
                           color = NULL, rot = NULL, grid_color = NULL, gridlines = NULL,
                           minor_grid_color = NULL, minor_gridlines = NULL) {
      params <- list(min = min, max = max, major = major, minor = minor, format = format, log_base = log_base,
                     color = color, rot = rot, grid_color = grid_color, gridlines = gridlines,
                     minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines)
      self$axis_params$x2 <- modifyList(self$axis_params$x2, Filter(Negate(is.null), params))
      invisible(self)
    },

    #' @description Set the doughnut hole size.
    #' @param val Integer 0 to 90.
    set_hole_size    = function(val) { self$hole_size <- val; invisible(self) },

    #' @param ang The angle of the first slice in degrees, from 0 to 360.
    #' This rotates the chart clockwise.
    set_pie_options  = function(ang = 0) { self$first_slice_ang <- ang; invisible(self) },

    #' @param scale The scale factor for bubbles, from 0 to 300 (expressed as a percentage).
    #' @param show_neg Logical; if `TRUE`, bubbles with negative values will be displayed on the chart.
    set_bubble_options = function(scale = 100, show_neg = FALSE) {
      self$bubble_scale <- scale
      self$show_neg_bubbles <- show_neg
      invisible(self)
    },

    #' @description Configure the chart legend.
    #' @param pos Legend position: 'r' (right), 'l' (left), 't' (top), 'b' (bottom), 'tr' (top right).
    #' @param overlay Logical. Whether the legend overlays the plot area.
    #' @param ... Font styling for legend text (e.g., color, sz, name).
    set_legend_style = function(pos = "r", overlay = FALSE, ...) {
      self$legend_params <- list(pos = pos, overlay = ifelse(overlay, "1", "0"), style = list(...))
      invisible(self)
    },

    #' @description Configure global data label settings.
    #' @param show_val Logical. Show numeric values.
    #' @param show_cat Logical. Show category names.
    #' @param show_legend_key Logical. Show legend key next to label.
    #' @param pos Label position (e.g., 't', 'b', 'ctr', 'l', 'r').
    #' @param ... Font styling for labels (e.g., color, sz, name).
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

    #' @description Add a data series to the chart.
    #' @param header Cell range or string for series name.
    #' @param data Cell range for series values.
    #' @param cat Cell range for category labels.
    #' @param z_data Cell range for bubble sizes (bubbleChart only).
    #' @param color Hex color for the series.
    #' @param type Chart type for this specific series (for combo charts).
    #' @param secondary Logical. Set to TRUE to move series to secondary axis.
    #' @param dir Bar direction ("col" or "bar").
    #' @param grouping Chart grouping ("standard", "stacked", "percentStacked").
    #' @param smooth Logical. Enable line smoothing.
    #' @param show_line Logical. Show the line connecting points.
    #' @param marker Marker type ("none", "circle", "square", "diamond", "triangle").
    #' @param marker_size Integer size of marker.
    #' @param marker_fill Hex color for marker fill.
    #' @param marker_line Hex color for marker border.
    #' @param marker_line_width Numeric width of marker border.
    #' @param show_val Logical. Override global label settings for this series (show value).
    #' @param show_cat Logical. Override global label settings for this series (show category).
    #' @param overlap Integer between -100 and 100. Determines how much bars in the same category overlap.
    #' @param gap_width Integer between 0 and 500. Controls the space between bar clusters.
    add_series = function(header, data, cat = NULL, z_data = NULL, color = "4472C4", type = NULL,
                          secondary = FALSE, dir = "col", grouping = "standard",
                          overlap = NULL, gap_width = NULL,
                          smooth = FALSE, show_line = TRUE,
                          marker = "none", marker_size = 5, marker_fill = NULL,
                          marker_line = NULL, marker_line_width = 0.75,
                          show_val = NULL, show_cat = NULL) {

      sec_val <- if (isTRUE(secondary)) "y" else if (isFALSE(secondary)) "none" else as.character(secondary)

      self$series_data[[length(self$series_data) + 1]] <- list(
        header = header, data = data, cat = cat, z_data = z_data, color = color,
        type = type %||% self$type, sec_type = sec_val, dir = dir,
        grouping = grouping, overlap = overlap, gap_width = gap_width,
        smooth = smooth, show_line = show_line,
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
          private$render_val_ax(plot_area, id_prim_cat, id_prim_val, "b", title_obj = self$x_title, params = self$axis_params$x)
        } else {
          private$render_cat_ax(plot_area, id_prim_cat, id_prim_val, "b", delete = "0", title_obj = self$x_title, params = self$axis_params$x)
        }
        private$render_val_ax(plot_area, id_prim_val, id_prim_cat, "l", title_obj = self$y_title, params = self$axis_params$y)

        if (!is.null(self$y2_title$text) || any(vapply(self$series_data, function(x) x$sec_type %in% c("y", "xy"), logical(1)))) {
          private$render_val_ax(plot_area, id_sec_val, id_prim_cat, "r", title_obj = self$y2_title, crosses = "max", params = self$axis_params$y2)
        }

        if (!is.null(self$x2_title$text)) {
          private$render_cat_ax(plot_area, id_sec_cat, id_sec_val, "t", delete = "0", title_obj = self$x2_title, params = self$axis_params$x2)
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

    is_ref = function(x) {
      if (is.null(x) || x == "") return(FALSE)
      # Check if '!' exists and is not at the very end (i.e., has a cell ref after it)
      grepl("!.+", x)
    },

    apply_line_style = function(ln_node, style_val) {
      if (is.character(style_val)) {
        # Mapping common names to OOXML presets
        val <- switch(style_val,
                      "dashed"  = "dash",
                      "dotted"  = "dot",
                      "dashDot" = "dashDot",
                      style_val # Fallback to literal string
        )
        xml2::xml_add_child(ln_node, "a:prstDash", val = val)
      }
    },

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
      if (type == "scatterChart") xml2::xml_add_child(c_node, "c:scatterStyle", val = "lineMarker")
      if (type == "barChart") {
        xml2::xml_add_child(c_node, "c:barDir", val = sub_series[[1]]$dir %||% "col")
        xml2::xml_add_child(c_node, "c:grouping", val = sub_series[[1]]$grouping %||% "standard")

        # Add Overlap if provided (Range -100 to 100)
        if (!is.null(sub_series[[1]]$overlap)) {
          xml2::xml_add_child(c_node, "c:overlap", val = as.character(sub_series[[1]]$overlap))
        }

        # Add Gap Width if provided (Range 0 to 500)
        if (!is.null(sub_series[[1]]$gap_width)) {
          xml2::xml_add_child(c_node, "c:gapWidth", val = as.character(sub_series[[1]]$gap_width))
        }
      }
      if (!type %in% c("scatterChart", "pieChart", "doughnutChart", "bubbleChart", "barChart")) {
        xml2::xml_add_child(c_node, "c:grouping", val = sub_series[[1]]$grouping %||% "standard")
      }

      vary_val <- if (type %in% c("pieChart", "doughnutChart")) "1" else "0"
      xml2::xml_add_child(c_node, "c:varyColors", val = vary_val)

      if (type == "doughnutChart") xml2::xml_add_child(c_node, "c:holeSize", val = as.character(self$hole_size))

      for (s in sub_series) {
        ser <- xml2::xml_add_child(c_node, "c:ser")
        xml2::xml_add_child(ser, "c:idx", val = as.character(private$current_idx))
        xml2::xml_add_child(ser, "c:order", val = as.character(private$current_idx))
        private$current_idx <- private$current_idx + 1

        tx <- xml2::xml_add_child(ser, "c:tx")
        if (private$is_ref(s$header)) {
          xml2::xml_add_child(xml2::xml_add_child(tx, "c:strRef"), "c:f", s$header)
        } else {
          xml2::xml_add_child(tx, "c:v", as.character(s$header))
        }

        if (type %in% c("bubbleChart", "pieChart", "doughnutChart")) {
          palette <- c("4472C4", "ED7D31", "A5A5A5", "FFC000", "5B9BD5", "70AD47", "264478", "9E480E")
          for (i in 0:15) {
            dPt <- xml2::xml_add_child(ser, "c:dPt")
            xml2::xml_add_child(dPt, "c:idx", val = as.character(i))
            sp_dpt <- xml2::xml_add_child(dPt, "c:spPr")
            private$render_fill(xml2::xml_add_child(sp_dpt, "a:solidFill"), palette[(i %% length(palette)) + 1])
            ln_dpt <- xml2::xml_add_child(sp_dpt, "a:ln", w = "9525")
            private$render_fill(xml2::xml_add_child(ln_dpt, "a:solidFill"), "FFFFFF")
          }
        }

        sp <- xml2::xml_add_child(ser, "c:spPr")
        if (type %in% c("barChart", "areaChart")) {
          private$render_fill(xml2::xml_add_child(sp, "a:solidFill"), s$color)
        } else if (type %in% c("lineChart", "scatterChart")) {
          ln <- xml2::xml_add_child(sp, "a:ln", w = "28575")
          if (isFALSE(s$show_line)) xml2::xml_add_child(ln, "a:noFill")
          else private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), s$color)
        }

        if (type %in% c("lineChart", "scatterChart")) {
          mkr_symbol <- if(type == "scatterChart" && (is.null(s$marker) || s$marker == "none")) "circle" else s$marker
          mkr <- xml2::xml_add_child(ser, "c:marker")
          xml2::xml_add_child(mkr, "c:symbol", val = mkr_symbol)
          if (!is.null(mkr_symbol) && mkr_symbol != "none") {
            xml2::xml_add_child(mkr, "c:size", val = as.character(s$marker_size %||% 5))
            m_spPr <- xml2::xml_add_child(mkr, "c:spPr")
            private$render_fill(xml2::xml_add_child(m_spPr, "a:solidFill"), s$marker_fill %||% s$color)
            m_ln <- xml2::xml_add_child(m_spPr, "a:ln", w = as.character(round((s$marker_line_width %||% 0.75) * 12700)))
            private$render_fill(xml2::xml_add_child(m_ln, "a:solidFill"), s$marker_line %||% s$color)
          }
        }

        if (type %in% c("scatterChart", "bubbleChart")) {
          if (!is.null(s$cat)) {
            x_val_node <- xml2::xml_add_child(ser, "c:xVal")
            ref_type <- if (grepl("!", s$cat)) "c:numRef" else "c:numLit"
            xml2::xml_add_child(xml2::xml_add_child(x_val_node, ref_type), "c:f", s$cat)
          }
          y_val_node <- xml2::xml_add_child(ser, "c:yVal")
          y_ref_type <- if (grepl("!", s$data)) "c:numRef" else "c:numLit"
          xml2::xml_add_child(xml2::xml_add_child(y_val_node, y_ref_type), "c:f", s$data)
          if (type == "bubbleChart" && !is.null(s$z_data)) {
            z_val_node <- xml2::xml_add_child(ser, "c:bubbleSize")
            z_ref_type <- if (grepl("!", s$z_data)) "c:numRef" else "c:numLit"
            xml2::xml_add_child(xml2::xml_add_child(z_val_node, z_ref_type), "c:f", s$z_data)
          }
        } else {
          if (!is.null(s$cat)) {
            cat_node <- xml2::xml_add_child(ser, "c:cat")
            c_ref_type <- if (grepl("!", s$cat)) "c:strRef" else "c:strLit"
            xml2::xml_add_child(xml2::xml_add_child(cat_node, c_ref_type), "c:f", s$cat)
          }
          val_node <- xml2::xml_add_child(ser, "c:val")
          v_ref_type <- if (grepl("!", s$data)) "c:numRef" else "c:numLit"
          xml2::xml_add_child(xml2::xml_add_child(val_node, v_ref_type), "c:f", s$data)
        }
        if (type %in% c("lineChart", "scatterChart")) xml2::xml_add_child(ser, "c:smooth", val = if(isTRUE(s$smooth)) "1" else "0")
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
      sz <- if(!is.null(style$sz)) style$sz * 100 else default_sz
      r <- xml2::xml_add_child(p, "a:r")
      rPr <- xml2::xml_add_child(r, "a:rPr", sz = as.character(sz))
      if (isTRUE(style$bold)) xml2::xml_set_attr(rPr, "b", "1")
      if (!is.null(style$color)) private$render_fill(xml2::xml_add_child(rPr, "a:solidFill"), style$color)
      if (!is.null(style$name)) xml2::xml_add_child(rPr, "a:latin", typeface = style$name)
      xml2::xml_add_child(r, "a:t", text)
    },

    render_cat_ax = function(parent, id, cross_id, pos, delete = "0", title_obj = NULL, params = NULL) {
      is_date <- !is.null(params$major_time)
      node_name <- if (is_date) "c:dateAx" else "c:catAx"
      ax <- xml2::xml_add_child(parent, node_name)
      xml2::xml_add_child(ax, "c:axId", val = id)
      scaling <- xml2::xml_add_child(ax, "c:scaling")
      xml2::xml_add_child(scaling, "c:orientation", val = "minMax")
      if (!is.null(params$max)) xml2::xml_add_child(scaling, "c:max", val = as.character(params$max))
      if (!is.null(params$min)) xml2::xml_add_child(scaling, "c:min", val = as.character(params$min))
      if (!is.null(params$log_base)) xml2::xml_add_child(scaling, "c:logBase", val = as.character(params$log_base))

      xml2::xml_add_child(ax, "c:delete", val = delete); xml2::xml_add_child(ax, "c:axPos", val = pos)
      if (!is.null(params$format)) xml2::xml_add_child(ax, "c:numFmt", formatCode = params$format, sourceLinked = "0")

      if (is_date) {
        if (!is.null(params$base_time)) xml2::xml_add_child(ax, "c:baseTimeUnit", val = params$base_time)
        if (!is.null(params$major)) xml2::xml_add_child(ax, "c:majorUnit", val = as.character(params$major))
        if (!is.null(params$major_time)) xml2::xml_add_child(ax, "c:majorTimeUnit", val = params$major_time)
        if (!is.null(params$minor)) xml2::xml_add_child(ax, "c:minorUnit", val = as.character(params$minor))
        if (!is.null(params$minor_time)) xml2::xml_add_child(ax, "c:minorTimeUnit", val = params$minor_time)
      }

      if (!is.null(params$gridlines) && !isFALSE(params$gridlines)) {
        g <- xml2::xml_add_child(ax, "c:majorGridlines")
        ln <- xml2::xml_add_child(xml2::xml_add_child(g, "c:spPr"), "a:ln")
        private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), params$grid_color %||% "D9D9D9")
        private$apply_line_style(ln, params$gridlines)
      }
      if (!is.null(params$minor_gridlines) && !isFALSE(params$minor_gridlines)) {
        mg <- xml2::xml_add_child(ax, "c:minorGridlines")
        ln_m <- xml2::xml_add_child(xml2::xml_add_child(mg, "c:spPr"), "a:ln")
        private$render_fill(xml2::xml_add_child(ln_m, "a:solidFill"), params$minor_grid_color %||% "F2F2F2")
        private$apply_line_style(ln_m, params$minor_gridlines)
      }

      if (!is.null(title_obj$text) && delete == "0") {
        t_node <- xml2::xml_add_child(ax, "c:title")
        private$add_title_content(t_node, title_obj$text, title_obj$style)
        xml2::xml_add_child(t_node, "c:layout")
        xml2::xml_add_child(t_node, "c:overlay", val = "0")
      }
      ln <- xml2::xml_add_child(xml2::xml_add_child(ax, "c:spPr"), "a:ln")
      private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), params$color %||% "000000")
      private$apply_text_style(ax, params)
      xml2::xml_add_child(ax, "c:crossAx", val = cross_id)
      xml2::xml_add_child(ax, "c:lblOffset", val = "100")
    },

    render_val_ax = function(parent, id, cross_id, pos, title_obj = NULL, crosses = "autoZero", params = NULL) {
      ax <- xml2::xml_add_child(parent, "c:valAx")
      xml2::xml_add_child(ax, "c:axId", val = id)
      scaling <- xml2::xml_add_child(ax, "c:scaling")
      xml2::xml_add_child(scaling, "c:orientation", val = "minMax")
      if (!is.null(params$max)) xml2::xml_add_child(scaling, "c:max", val = as.character(params$max))
      if (!is.null(params$min)) xml2::xml_add_child(scaling, "c:min", val = as.character(params$min))
      if (!is.null(params$log_base)) xml2::xml_add_child(scaling, "c:logBase", val = as.character(params$log_base))

      xml2::xml_add_child(ax, "c:delete", val = "0"); xml2::xml_add_child(ax, "c:axPos", val = pos)
      if (!is.null(params$format)) xml2::xml_add_child(ax, "c:numFmt", formatCode = params$format, sourceLinked = "0")

      if (!is.null(params$major)) xml2::xml_add_child(ax, "c:majorUnit", val = as.character(params$major))
      if (!is.null(params$minor)) xml2::xml_add_child(ax, "c:minorUnit", val = as.character(params$minor))

      if (!is.null(params$gridlines) && !isFALSE(params$gridlines)) {
        g <- xml2::xml_add_child(ax, "c:majorGridlines")
        ln <- xml2::xml_add_child(xml2::xml_add_child(g, "c:spPr"), "a:ln")
        private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), params$grid_color %||% "D9D9D9")
        private$apply_line_style(ln, params$gridlines)
      }
      if (!is.null(params$minor_gridlines) && !isFALSE(params$minor_gridlines)) {
        mg <- xml2::xml_add_child(ax, "c:minorGridlines")
        ln_m <- xml2::xml_add_child(xml2::xml_add_child(mg, "c:spPr"), "a:ln")
        private$render_fill(xml2::xml_add_child(ln_m, "a:solidFill"), params$minor_grid_color %||% "F2F2F2")
        private$apply_line_style(ln_m, params$minor_gridlines)
      }
      if (!is.null(title_obj$text)) {
        t_node <- xml2::xml_add_child(ax, "c:title")
        private$add_title_content(t_node, title_obj$text, title_obj$style)
        xml2::xml_add_child(t_node, "c:layout"); xml2::xml_add_child(t_node, "c:overlay", val = "0")
      }
      ln <- xml2::xml_add_child(xml2::xml_add_child(ax, "c:spPr"), "a:ln")
      private$render_fill(xml2::xml_add_child(ln, "a:solidFill"), params$color %||% "000000")
      private$apply_text_style(ax, params)

      xml2::xml_add_child(ax, "c:crossAx", val = cross_id)
      xml2::xml_add_child(ax, "c:crosses", val = crosses)
      xml2::xml_add_child(ax, "c:crossBetween", val = "between")
    },

    apply_text_style = function(node, s) {
      txPr <- xml2::xml_add_child(node, "c:txPr")

      # 1. Create body properties and apply rotation
      bodyPr <- xml2::xml_add_child(txPr, "a:bodyPr")
      if (!is.null(s$rot)) {
        # rotation = degrees * 60000
        xml2::xml_set_attr(bodyPr, "rot", as.character(round(s$rot * 60000)))
        xml2::xml_set_attr(bodyPr, "vert", "horz")
      }

      # 2. Add required list style
      xml2::xml_add_child(txPr, "a:lstStyle")

      # 3. Build the text run properties (defRPr)
      p      <- xml2::xml_add_child(txPr, "a:p")
      pPr    <- xml2::xml_add_child(p, "a:pPr")
      defRPr <- xml2::xml_add_child(pPr, "a:defRPr")

      # Apply font size (Excel uses 1/100th of a point)
      sz <- if(!is.null(s$sz)) s$sz * 100 else 900
      xml2::xml_set_attr(defRPr, "sz", as.character(sz))

      if (isTRUE(s$bold)) xml2::xml_set_attr(defRPr, "b", "1")

      if (!is.null(s$color)) {
        private$render_fill(xml2::xml_add_child(defRPr, "a:solidFill"), s$color)
      }

      if (!is.null(s$name)) {
        xml2::xml_add_child(defRPr, "a:latin", typeface = s$name)
      }
    },

    render_fill = function(node, color_val) {
      if (length(color_val) == 0 || is.null(color_val) || color_val == "") color_val <- "000000"
      hex <- if (inherits(color_val, "wbColour")) attr(color_val, "rgb") %||% as.character(color_val) else as.character(color_val)
      clean <- toupper(gsub("^#", "", hex))
      if (nchar(clean) == 8) clean <- substr(clean, 3, 8)
      xml2::xml_add_child(node, "a:srgbClr", val = clean)
    }
  )
)
