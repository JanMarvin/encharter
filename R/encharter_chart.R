#' R6 Class representing a Chart object for Spreadsheets
#'
#' @description
#' The `Chart` class provides a flexible interface to build Office OpenXML
#' (OOXML) chart objects. It allows for granular control over grid lines,
#' secondary axes, and combined chart types (e.g., Bar and Line) within a
#' single plot area.
#'
#' @details
#' This class is designed to work with the `openxlsx2` package by generating
#' the underlying XML required for the `add_chart_xml` method.
#'
#' @rdname encharter
#' @usage NULL
Chart <- R6::R6Class(
  "Chart",
  inherit = EncharterBase,
  public = list(
    #' @field x2_title List containing text and style for the secondary X-axis.
    x2_title = list(text = NULL, style = list()),
    #' @field y2_title List containing text and style for the secondary Y-axis.
    y2_title = list(text = NULL, style = list()),
    #' @field first_slice_ang Integer. Rotation of the first slice (0-360).
    first_slice_ang = NULL,
    #' @field expansion Integer. Size of the expansion for pie charts.
    expansion = NULL,
    #' @field hole_size Integer. Size of the hole for doughnut charts (0-90).
    hole_size = 75,
    #' @field show_data_table Logical if a data table should be added.
    show_data_table = FALSE,
    #' @field drop_lines Logical; show lines from points to the axis.
    drop_lines = FALSE,
    #' @field high_low_lines Logical; show lines between max/min points.
    high_low_lines = FALSE,
    #' @field up_down_bars Logical; show bars between first and last series.
    up_down_bars = FALSE,
    #' @field bubble_scale Numeric; the scale factor for bubbles (default 100).
    bubble_scale = 100,
    #' @field show_neg_bubbles Logical; whether to show bubbles with negative values.
    show_neg_bubbles = FALSE,
    #' @field disp_blanks_as Character; "gap", "span", or "zero".
    disp_blanks_as = "gap",

    #' @description Initialize a new Chart object.
    #' @param type Initial chart type (e.g., "lineChart", "barChart", "pieChart").
    initialize = function(type = NULL) {

      private$validate_input(
        type,
        ENCHARTER_STANDARD,
        "series type"
      )

      type <- normalize_encharter_type(type)
      self$type <- type
      self$xml <- read_xml(
        '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart"
                        xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                        xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006">
           <c:date1904 val="0" /><c:roundedCorners val="0" />
           <c:chart></c:chart>
         </c:chartSpace>'
      )
      # <c:lang val="en-GB" />
      # <mc:AlternateContent>
      #   <mc:Choice Requires="c14" xmlns:c14="http://schemas.microsoft.com/office/drawing/2007/8/2/chart">
      #     <c14:style val="102" />
      #   </mc:Choice>
      #   <mc:Fallback><c:style val="2" /></mc:Fallback>
      # </mc:AlternateContent>
    },

    #' @description Set the secondary X-axis title.
    #'
    #' Only takes effect if at least one series has been assigned to the
    #' secondary X-axis via `add_series(secondary = "x")`. Issues a warning
    #' and returns `self` silently otherwise.
    #'
    #' @param text Title string.
    #' @param font_size Numeric font size in points.
    #' @param font_name Font typeface name.
    #' @param font_color Six-digit hex color for the title text.
    #' @param bold,italic Logical font style.
    #' @param fill Six-digit hex color for the title background box.
    #' @param line Six-digit hex color for the title border.
    #' @param line_width Numeric border width in points.
    #' @examples
    #' ec("scatter")$
    #'   add_series(data = "Sheet1!A1:A10", secondary = "x")$
    #'   set_x2_title("Secondary X", font_color = "888888")
    set_x2_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL,
                            bold = NULL, italic = NULL, fill = NULL, line = NULL,
                            line_width = NULL) {
      has_secondary <- any(vapply(self$series_data, function(s) s$sec_type == "x", NA))

      if (!has_secondary) {
        warning("Secondary axis title ignored: no series is assigned to a secondary X-axis.", call. = FALSE)
        return(invisible(self))
      }
      if (!inherits(text, "fmt_txt")) text <- private$sanitize_xml(text)
      self$x2_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
      invisible(self)
    },

    #' @description Set the secondary Y-axis title.
    #'
    #' Only takes effect if at least one series has been assigned to the
    #' secondary Y-axis via `add_series(secondary = TRUE)` or
    #' `secondary = "y"`. Issues a warning otherwise.
    #'
    #' @param text Title string.
    #' @param font_size Numeric font size in points.
    #' @param font_name Font typeface name.
    #' @param font_color Six-digit hex color for the title text.
    #' @param bold,italic Logical font style.
    #' @param fill Six-digit hex color for the title background box.
    #' @param line Six-digit hex color for the title border.
    #' @param line_width Numeric border width in points.
    #' @examples
    #' ec("line")$
    #'   add_series(data = "Sheet1!A1:A10")$
    #'   add_series(data = "Sheet1!B1:B10", secondary = TRUE)$
    #'   set_y2_title("Growth Rate (%)")
    set_y2_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL,
                            bold = NULL, italic = NULL, fill = NULL, line = NULL,
                            line_width = NULL) {
      has_secondary <- any(vapply(self$series_data, function(s) s$sec_type == "y", NA))

      if (!has_secondary) {
        warning("Secondary axis title ignored: no series is assigned to a secondary Y-axis.", call. = FALSE)
        return(invisible(self))
      }
      if (!inherits(text, "fmt_txt")) text <- private$sanitize_xml(text)
      self$y2_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
      invisible(self)
    },

    #' @description Set Secondary Y-axis scaling, units, and format.
    #' @param min Minimum value for the axis.
    #' @param max Maximum value for the axis.
    #' @param major Numeric value for major unit interval.
    #' @param minor Numeric value for minor unit interval.
    #' @param major_time Time unit for major steps ("days", "months", "years"). Used for date axes.
    #' @param minor_time Time unit for minor steps ("days", "months", "years"). Used for date axes.
    #' @param major_tick,minor_tick Tick marks for major and minor ("cross", "in", "none", "out").
    #' @param base_time Base time unit for date axes ("days", "months", "years").
    #' @param format A number format string (e.g., "#,##0" or "yyyy-mm-dd").
    #' @param log_base Base for logarithmic scaling (e.g., 10).
    #' @param color,font_color Hex color for the axis lines and label (or independent label color).
    #' @param font_size Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param font_name Font typeface name (e.g., "Arial", "Calibri").
    #' @param rotation Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the grid lines.
    #' @param grid_lines,minor_grid_lines Logical. Show or hide grid lines.
    #' @param line_width,grid_width,minor_grid_width Numeric. Change the width of the axis and grid lines.
    #' @param cross_between Specifies how the value axis crosses the category axis ('between' or 'midCat').
    #' @param crosses Intersection: "autoZero" (default), "min" (start), or "max" (end).
    #' @param crosses_at Numeric axis value for intersection. Overrides 'crosses'.
    #' @param label_pos Label position: "nextTo" (default), "low" (edge of chart), "high" (opposite edge), or "none".
    set_y2_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                           major_time = NULL, minor_time = NULL, base_time = NULL,
                           major_tick = NULL, minor_tick = NULL,
                           format = NULL, log_base = NULL, color = NULL,
                           font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                           font_color = NULL, rotation =  NULL,
                           grid_color = NULL, grid_lines = NULL,
                           minor_grid_color = NULL, minor_grid_lines = NULL, cross_between = NULL,
                           line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                           crosses = "max", crosses_at = NULL, label_pos = NULL) {
        private$set_axis_params(
          "y2",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rotation = rotation, grid_color = grid_color, grid_lines = grid_lines,
          minor_grid_color = minor_grid_color, minor_grid_lines = minor_grid_lines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos
        )
    },

    #' @description Set Secondary X-axis scaling, units, and format.
    #' @param min Minimum value for the axis.
    #' @param max Maximum value for the axis.
    #' @param major Numeric value for major unit interval.
    #' @param minor Numeric value for minor unit interval.
    #' @param major_time Time unit for major steps ("days", "months", "years"). Used for date axes.
    #' @param minor_time Time unit for minor steps ("days", "months", "years"). Used for date axes.
    #' @param major_tick,minor_tick Tick marks for major and minor ("cross", "in", "none", "out").
    #' @param base_time Base time unit for date axes ("days", "months", "years").
    #' @param format A number format string (e.g., "#,##0" or "yyyy-mm-dd").
    #' @param log_base Base for logarithmic scaling (e.g., 10).
    #' @param color,font_color Hex color for the axis lines and label (or independent label color).
    #' @param font_size Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param font_name Font typeface name (e.g., "Arial", "Calibri").
    #' @param rotation Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the grid lines.
    #' @param grid_lines,minor_grid_lines Logical. Show or hide grid lines.
    #' @param line_width,grid_width,minor_grid_width Numeric. Change the width of the axis and grid lines.
    #' @param cross_between Specifies how the value axis crosses the category axis ('between' or 'midCat').
    #' @param crosses Intersection: "autoZero" (default), "min" (start), or "max" (end).
    #' @param crosses_at Numeric axis value for intersection. Overrides 'crosses'.
    #' @param label_pos Label position: "nextTo" (default), "low" (edge of chart), "high" (opposite edge), or "none".
    set_x2_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                           major_time = NULL, minor_time = NULL, base_time = NULL,
                           major_tick = NULL, minor_tick = NULL,
                           format = NULL, log_base = NULL, color = NULL,
                           font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                           font_color = NULL, rotation =  NULL,
                           grid_color = NULL, grid_lines = NULL,
                           minor_grid_color = NULL, minor_grid_lines = NULL, cross_between = NULL,
                           line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                           crosses = "max", crosses_at = NULL, label_pos = NULL) {

        private$set_axis_params(
          "x2",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rotation = rotation, grid_color = grid_color, grid_lines = grid_lines,
          minor_grid_color = minor_grid_color, minor_grid_lines = minor_grid_lines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos
        )
    },

    #' @description Set the data table.
    #' @param show Logical TRUE or FALSE.
    set_data_table = function(show = TRUE) {
      self$show_data_table <- show
      invisible(self)
    },

    #' @param rotation The angle of the first slice in degrees, from 0 to 360.
    #' This rotates the chart clockwise.
    #' @param expansion Sets the expansion, from 0 to 400.
    #' @param hole_size Set the hole size of (only doughnut charts), from 0 to 90.
    set_pie_options  = function(rotation = NULL, expansion = NULL, hole_size = NULL) {

      if (!is.null(rotation)) {
        self$first_slice_ang <- rotation
      }
      if (!is.null(expansion)) {
        self$expansion <- expansion
      }
      if (!is.null(hole_size)) {
        self$hole_size <- hole_size
      }

      invisible(self)
    },

    #' @param scale The scale factor for bubbles, from 0 to 300 (expressed as a percentage).
    #' @param show_neg Logical; if `TRUE`, bubbles with negative values will be displayed on the chart.
    set_bubble_options = function(scale = 100, show_neg = FALSE) {
      self$bubble_scale <- scale
      self$show_neg_bubbles <- show_neg
      invisible(self)
    },

    #' @description Set missing value behavior ("gap", "span", "zero").
    #' @param val Character. One of "gap" (break), "span" (continue), or "zero" (drop).
    set_disp_blanks = function(val = "gap") {
      self$disp_blanks_as <- private$validate_input(val, c("gap", "span", "zero"), "disp_blanks_as")
      invisible(self)
    },

    #' @description Add a data series to the chart with independent styling.
    #' @param name Cell range or string for series name.
    #' @param data Cell range for series values.
    #' @param label Cell range for category labels.
    #' @param weight Cell range for bubble sizes (bubbleChart only).
    #' @param color Primary Hex color for the series (used as default for line and markers).
    #' @param type Chart type for this specific series (for combo charts).
    #' @param secondary Logical. Set to TRUE to move series to secondary axis.
    #' @param dir Bar direction ("col" or "bar").
    #' @param grouping Chart grouping ("standard", "stacked", "percentStacked").
    #' @param smooth Logical. Enable line smoothing for line/scatter charts.
    #' @param show_line Logical. Show the line connecting points.
    #' @param marker Marker type ("none", "circle", "square", "diamond", "triangle").
    #' @param marker_size Integer size of marker.
    #' @param marker_fill Hex color for the interior of the marker. Defaults to `color`.
    #' @param marker_line Hex color for the marker border. Defaults to `color`.
    #' @param marker_line_width Numeric width of the marker border.
    #' @param show_val Logical. Override global label settings for this series (show value).
    #' @param show_cat Logical. Override global label settings for this series (show category).
    #' @param overlap Integer between -100 and 100 for bar charts.
    #' @param gap_width Integer between 0 and 500 for bar charts.
    #' @param line_type Line style: "dashed", "dotted", "dashDot", or "solid".
    #' @param line_width Numeric width of the connecting line.
    #' @param line_color Hex color for the connecting line. Defaults to `color`.
    #' @param filled Logical; for radar charts, fills the interior area. Default FALSE.
    #' @param error_bars A list of error bar properties:
    #'
    #'   * `type`: The error value type (`ST_ErrValType`).
    #'     Must be one of: `"fixedVal"` (Fixed Value), `"percentage"` (Percentage),
    #'     `"stdDev"` (Standard Deviation), `"stdErr"` (Standard Error),
    #'     or `"cust"` (Custom).
    #'   * `value`: The numeric value for the error bars (e.g., 10 for 10% or 5 for fixed units).
    #'   * `direction`: Direction of bars. One of `"both"`, `"plus"`, or `"minus"`.
    #'   * `color`: Hex color code for the bars (e.g., "FF0000").
    #'
    #' @param trendline A list of regression line properties:
    #'
    #'   * `type`: The regression type (`ST_TrendlineType`).
    #'     Must be one of: `"linear"` (Linear), `"exp"` (Exponential),
    #'     `"log"` (Logarithmic), `"movingAvg"` (Moving Average),
    #'     `"poly"` (Polynomial), or `"power"` (Power).
    #'   * `order`: Required for `"poly"`; an integer between 2 and 6.
    #'   * `period`: Required for `"movingAvg"`; an integer representing the window size.
    #'   * `color`: Hex color code for the line.
    #'   * `show_r2`: Logical; if `TRUE`, displays the R-squared value on the chart.
    #'
    add_series = function(name = NULL, data, label = NULL, weight = NULL,
                          color = "4472C4", type = NULL,
                          secondary = FALSE, dir = "col", grouping = "standard",
                          overlap = NULL, gap_width = NULL,
                          smooth = FALSE, show_line = TRUE,
                          marker = "none", marker_size = 5,
                          marker_fill = NULL, marker_line = NULL,
                          marker_line_width = 0.75,
                          show_val = NULL, show_cat = NULL,
                          line_type = NULL, line_width = 1, line_color = NULL,
                          filled = FALSE, error_bars = FALSE, trendline = FALSE) {

      type <- normalize_encharter_type(type)
      private$validate_input(
        type,
        ENCHARTER_STANDARD,
        "series type"
      )

      marker <- private$validate_input(
        marker,
        c("none", "circle", "dash", "diamond", "dot", "plus", "square", "star", "triangle", "x"),
        "marker"
      )

      dir <- normalize_encharter_string(dir)
      dir <- private$validate_input(dir, c("col", "bar"), "dir")

      grouping <- private$validate_input(
        grouping,
        c("standard", "clustered", "stacked", "percentStacked"),
        "grouping"
      )

      # 2. Validate Line Type (Dash Style)
      # OOXML presetDash values
      private$validate_input(
        line_type,
        c("solid", "dash", "dot", "dashDot", "lgDash", "lgDashDot", "sysDash", "sysDot", "dashed", "dotted"),
        "line_type"
      )

      sec_val <- if (isTRUE(secondary)) "y"
        else if (isFALSE(secondary)) "none"
        else match.arg(secondary, c("x", "y", "xy", "none"))

      series_type <- type %||% self$type %||% "barChart"
      series_type <- normalize_encharter_type(series_type)
      self$type <- series_type
      if (!is.null(color) && length(color) > 1 && series_type %in% c("bubbleChart", "pieChart", "doughnutChart")) self$palette <- color

      h_expr <- substitute(name)
      c_expr <- substitute(label)

      if (is.null(color)) {
        color_idx <- (length(self$series_data) %% length(self$palette)) + 1
        color <- self$palette[color_idx]
      }

      data_vals <- NULL
      cat_vals <- NULL
      z_vals <- NULL
      if (inherits(data, "wb_data")) {
        wb_sheet   <- attr(data, "sheet")
        dims_mat   <- attr(data, "dims")
        col_names  <- names(data)

        # Deterministic name detection based on row counts
        has_header <- nrow(dims_mat) > length(attr(data, "row.names"))

        h_label <- tryCatch(if (is.symbol(h_expr)) deparse1(h_expr) else name, error = function(e) NULL)
        c_label <- tryCatch(if (is.symbol(c_expr)) deparse1(c_expr) else label, error = function(e) NULL)

        # For weight, we handle the NSE expression locally
        z_expr  <- substitute(weight)
        z_label <- tryCatch(if (is.symbol(z_expr)) deparse1(z_expr) else weight, error = function(e) NULL)

        start_row <- if (has_header) 2 else 1
        wd_orig <- data

        # 1. Resolve Column Index for Y-Data and Header
        col_idx <- which(col_names == h_label)
        if (length(col_idx) > 0) {
          col_idx   <- col_idx[1]
          data_vals <- wd_orig[[h_label]]
          name      <- if (has_header) sprintf("%s!%s", wb_sheet, dims_mat[1, col_idx]) else NULL
          data      <- sprintf("%s!%s:%s", wb_sheet, dims_mat[start_row, col_idx], dims_mat[nrow(dims_mat), col_idx])
        }

        # 2. Resolve Category (label / X-Axis)
        cat_idx <- which(col_names == c_label)
        if (length(cat_idx) > 0) {
          cat_idx  <- cat_idx[1]
          cat_vals <- wd_orig[[c_label]]
          label    <- sprintf("%s!%s:%s", wb_sheet, dims_mat[start_row, cat_idx], dims_mat[nrow(dims_mat), cat_idx])
        }

        # 3. Resolve Z-Data (Bubble Size)
        z_idx <- which(col_names == z_label)
        if (length(z_idx) > 0) {
          z_idx  <- z_idx[1]
          z_vals <- wd_orig[[z_label]]
          weight <- sprintf("%s!%s:%s", wb_sheet, dims_mat[start_row, z_idx], dims_mat[nrow(dims_mat), z_idx])
        }
      }

      # Apply absolute reference wrapper to all potential range strings
      name <- to_abs_ref(name)
      data   <- to_abs_ref(data)
      label  <- to_abs_ref(label)
      weight <- to_abs_ref(weight)

      if (!is.null(data) && !grepl("!", data)) {
        stop("Series data must be a sheet reference (e.g., 'Sheet1!A1:A10').", call. = FALSE)
      }

      # Create the clean object
      self$series_data[[length(self$series_data) + 1]] <- list(
        name      = name,
        data      = data,
        label     = label,
        weight    = weight,
        data_cache = data_vals,
        cat_cache = cat_vals,
        z_cache   = z_vals,
        type      = series_type,
        sec_type  = sec_val,
        smooth    = smooth,
        filled    = filled,
        dir       = dir,
        grouping  = grouping,
        overlap   = overlap,
        gap_width = gap_width,
        error_bars  = error_bars,
        trendline = trendline,

        # GROUPED STYLING: Line
        line = list(
          color = line_color %||% color,
          width = line_width,
          type  = line_type,
          show  = show_line
        ),

        # GROUPED STYLING: Marker
        marker = list(
          symbol = marker,
          size   = marker_size,
          fill   = marker_fill %||% color,
          line   = list(
            color = marker_line %||% color,
            width = marker_line_width,
            show  = TRUE
          )
        ),

        # Other params
        show_val    = show_val %||% self$label_params$show_val,
        show_cat    = show_cat %||% self$label_params$show_cat,
        label_pos   = self$label_params$pos
        #  label_style = self$label_params$style currently unused?
      )

      invisible(self)
    },

    #' @description Generate the final XML string for the chart.
    #' @return A character string containing the OOXML chart definition.
    #' @param u_ids five unique ids
    render = function(u_ids = c("53178645", "60812428", "64752656", "81893617", "90007639")) {

      if (length(self$series_data) == 0) {
        stop(
          "The chart contains no data. You must add at least one series using $add_series() before rendering.",
          call. = FALSE
        )
      }

      self$type <- self$type %||% "barChart"
      xml_remove(xml_find_all(self$xml, "c:spPr"))
      private$apply_sp_pr(self$xml, self$chart_style)

      chart_root <- xml_find_first(self$xml, "//c:chart")
      xml_remove(xml_children(chart_root))

      if (!is.null(self$chart_title$text)) {
        t_node <- xml_add_child(chart_root, "c:title")
        private$add_title_content(t_node, self$chart_title$text, self$chart_title$style, default_sz = 1400)
        xml_add_child(t_node, "c:overlay", val = "0")
      }
      xml_add_child(chart_root, "c:autoTitleDeleted", val = if (is.null(self$chart_title$text)) "1" else "0")

      if (self$type == "surfaceChart") {
        v3d <- xml_add_child(chart_root, "c:view3D")
        xml_add_child(v3d, "c:rotX", val = "90")
        xml_add_child(v3d, "c:rotY", val = "0")
        xml_add_child(v3d, "c:rAngAx", val = "0")
        xml_add_child(v3d, "c:perspective", val = "0")
      }

      plot_area <- xml_add_child(chart_root, "c:plotArea")
      xml_add_child(plot_area, "c:layout")

      id_prim_cat <- u_ids[1]
      id_prim_val <- u_ids[2]
      id_sec_cat  <- u_ids[3]
      id_sec_val  <- u_ids[4]
      id_ser_ax   <- u_ids[5]


      private$current_idx <- 0
      combos <- unique(lapply(self$series_data, function(x) list(type = x$type, sec_type = x$sec_type)))

      has_axes <- FALSE
      for (combo in combos) {
        sub_series <- Filter(function(x) x$type == combo$type && x$sec_type == combo$sec_type, self$series_data)

        # CASE: "x" or "xy" triggers the Secondary X-Axis (Top)
        cat_id <- if (combo$sec_type %in% c("x", "xy")) id_sec_cat else id_prim_cat

        # CASE: "y" or "xy" triggers the Secondary Y-Axis (Right)
        # Note: sec_type is "none" or "y"/"x"/"xy" based on your add_series logic
        val_id <- if (combo$sec_type %in% c("y", "xy")) id_sec_val else id_prim_val

        # Keep your surface logic
        ser_ax_id <- if (self$type == "surfaceChart") id_ser_ax else NULL

        private$render_series_node(plot_area, sub_series, combo$type, cat_id, val_id, ser_ax_id)

        if (!combo$type %in% c("pieChart", "doughnutChart")) has_axes <- TRUE
      }

      if (has_axes) {
        # 1. Pre-scan
        needs_sec_y <- any(vapply(self$series_data, function(x) x$sec_type %in% c("y", "xy"), FALSE))
        needs_sec_x <- any(vapply(self$series_data, function(x) x$sec_type %in% c("x", "xy"), FALSE))

        # 2. Primary X-Axis (Bottom)
        # Always rendered
        if (self$type %in% c("scatterChart", "bubbleChart")) {
          private$render_val_ax(plot_area, id_prim_cat, id_prim_val, "b", title_obj = self$x_title, params = self$axis_params$x)
        } else {
          private$render_cat_ax(plot_area, id_prim_cat, id_prim_val, "b", delete = "0", title_obj = self$x_title, params = self$axis_params$x)
        }

        # 3. Primary Y-Axis (Left)
        # Always rendered
        if (self$type == "surfaceChart") {
          private$render_val_ax(plot_area, id_prim_val, id_prim_cat, "l", delete = "1", title_obj = self$y_title, params = self$axis_params$y)
        } else {
          private$render_val_ax(plot_area, id_prim_val, id_prim_cat, "l", title_obj = self$y_title, params = self$axis_params$y)
        }
        # 3. Primary Y-Axis (Left / Vertical Height)

        # 4. Secondary Y-Axis (Right)
        if (needs_sec_y || !is.null(self$y2_title$text)) {
          # Secondary Y crosses the Primary X at its maximum (the right side)
          private$render_val_ax(plot_area, id_sec_val, id_prim_cat, "r", title_obj = self$y2_title, crosses = "max", params = self$axis_params$y2)
        }

        # 5. Secondary X-Axis (Top)
        if (needs_sec_x || !is.null(self$x2_title$text)) {
          # IMPORTANT: To get the X-axis to the TOP, it must cross the Y-axis at its MAX value.
          # We cross the Primary Y (id_prim_val) unless we specifically want a fully independent system.

          if (self$type %in% c("scatterChart", "bubbleChart")) {
            private$render_val_ax(plot_area, id_sec_cat, id_prim_val, "t", title_obj = self$x2_title, crosses = "max", params = self$axis_params$x2)
          } else {
            # Note: for catAx/dateAx, the 'crosses val="max"' attribute moves the axis to the top.
            private$render_cat_ax(plot_area, id_sec_cat, id_prim_val, "t", delete = "0", title_obj = self$x2_title, crosses = "max", params = self$axis_params$x2)
          }
        }

        if (self$type == "surfaceChart") {
          private$render_ser_ax(plot_area, id_ser_ax, id_prim_val)
        }
      }

      if (isTRUE(self$show_data_table)) {
        dTable <- xml_add_child(plot_area, "c:dTable")

        # Standard visibility flags
        xml_add_child(dTable, "c:showHorzBorder", val = "1")
        xml_add_child(dTable, "c:showVertBorder", val = "1")
        xml_add_child(dTable, "c:showOutline",    val = "1")
        xml_add_child(dTable, "c:showKeys",       val = "1")

        private$apply_text_style(dTable, self$axis_params$x) # size is a little smaller
      }

      private$apply_sp_pr(plot_area, self$plot_style)

      l_pos <- self$legend_params$pos %||% "t"
      if (l_pos != "none") {
        legend <- xml_add_child(chart_root, "c:legend")
        xml_add_child(legend, "c:legendPos", val = self$legend_params$pos)
        xml_add_child(legend, "c:overlay", val = self$legend_params$overlay)
        if (length(self$legend_params$style) > 0) private$apply_text_style(legend, self$legend_params$style)
      }
      xml_add_child(chart_root, "c:dispBlanksAs", val = self$disp_blanks_as)

      read_xml(as.character(self$xml), pointer = FALSE)
    }
  ),

  private = list(
    current_idx = 0,

    is_ref = function(x) {
      if (is.null(x) || x == "") return(FALSE)
      # Check if '!' exists and is not at the very end (i.e., has a cell ref after it)
      grepl("!.+", x)
    },

    # Unified Line Styler
    render_line_style = function(node, settings) {
      if (isFALSE(settings$show)) {
        xml_add_child(node, "a:noFill")
        return()
      }
      # Set Width (Points to EMUs)
      w_emu <- as.character(round((settings$width %||% 1) * 12700))
      ln <- xml_add_child(node, "a:ln", w = w_emu)

      # Set Color
      private$render_color_core(xml_add_child(ln, "a:solidFill"), settings$color %||% "000000")

      # Set Dash/Line Type
      if (!is.null(settings$type)) {
        private$apply_line_style(ln, settings$type)
      }
    },

    # Simplified Fill Styler
    render_fill_style = function(node, color) {
      if (is.null(color) || color == "none") {
        xml_add_child(node, "a:noFill")
      } else {
        private$render_color_core(xml_add_child(node, "a:solidFill"), color)
      }
    },

    apply_line_style = function(ln_node, style_val) {
      if (is.character(style_val)) {
        # Mapping common names to OOXML presets
        val <- switch(style_val,
                      "dashed"  = "dash",
                      "dotted"  = "dot",
                      style_val # Fallback to literal string
        )
        xml_add_child(ln_node, "a:prstDash", val = val)
      }
    },

    apply_sp_pr = function(node, style) {
      if (is.null(style$fill) && is.null(style$line)) return()
      spPr <- xml_add_child(node, "c:spPr")
      if (!is.null(style$fill)) private$render_color_core(xml_add_child(spPr, "a:solidFill"), style$fill)
      if (!is.null(style$line)) {
        ln <- xml_add_child(spPr, "a:ln", w = as.character(round(style$line_width * 12700)))
        private$render_color_core(xml_add_child(ln, "a:solidFill"), style$line)
      } else {
        xml_add_child(xml_add_child(spPr, "a:ln"), "a:noFill")
      }
    },

    render_series_node = function(plot_area, sub_series, type, cat_id, val_id, ser_id) {
      c_node <- xml_add_child(plot_area, paste0("c:", type))

      # 1. INITIAL PROPERTIES (Must come before <c:ser>)
      if (type == "scatterChart") {
        xml_add_child(c_node, "c:scatterStyle", val = "lineMarker")
      }

      if (type == "barChart") {
        xml_add_child(c_node, "c:barDir", val = sub_series[[1]]$dir %||% "col")
        xml_add_child(c_node, "c:grouping", val = sub_series[[1]]$grouping %||% "standard")
      }

      if (type == "radarChart") {
        radar_val <- if (isTRUE(sub_series[[1]]$filled)) "filled" else "standard"
        xml_add_child(c_node, "c:radarStyle", val = radar_val)
      }

      if (type == "surfaceChart") {
        surface_val <- if (isTRUE(sub_series[[1]]$filled)) "1" else "0"
        xml_add_child(c_node, "c:wireframe", val = surface_val)
      }

      if (!type %in% c("scatterChart", "pieChart", "doughnutChart", "bubbleChart", "barChart", "radarChart", "stockChart", "surfaceChart")) {
        xml_add_child(c_node, "c:grouping", val = sub_series[[1]]$grouping %||% "standard")
      }

      if (!type %in% c("stockChart", "surfaceChart")) {
        vary_val <- if (type %in% c("pieChart", "doughnutChart")) "1" else "0"
        xml_add_child(c_node, "c:varyColors", val = vary_val)
      }

      # 2. THE SERIES LOOP
      for (s in sub_series) {

        ser <- xml_add_child(c_node, "c:ser")
        xml_add_child(ser, "c:idx", val = as.character(private$current_idx))
        xml_add_child(ser, "c:order", val = as.character(private$current_idx))
        private$current_idx <- private$current_idx + 1

        # --- EG_SerShared Start ---
        # tx (Title)
        if (!is.null(s$name) && length(s$name) > 0) {
          tx <- xml_add_child(ser, "c:tx")

          if (private$is_ref(s$name)) {
            # It's a range reference like Sheet1!$A$1
            strRef <- xml_add_child(tx, "c:strRef")
            xml_add_child(strRef, "c:f", s$name)
          } else {
            # It's a literal string
            xml_add_child(tx, "c:v", as.character(s$name))
          }
        }

        # spPr (Series Styling)
        if (!type %in% c("pieChart", "doughnutChart")) {
          sp <- xml_add_child(ser, "c:spPr")
          if (type %in% c("barChart", "areaChart", "bubbleChart")) {
            color <- s$line$color %||% s$color %||% "auto"
            private$render_color_core(xml_add_child(sp, "a:solidFill"), color)
          } else if (type %in% c("lineChart", "scatterChart", "stockChart")) {
            # If show_line is FALSE, we must explicitly tell OOXML not to draw the line
            if (isFALSE(s$line$show)) {
              ln <- xml_add_child(sp, "a:ln")
              xml_add_child(ln, "a:noFill")
            } else {
              private$render_line_style(sp, s$line)
            }
          }
        }
        # --- EG_SerShared End ---

        # 3. Marker (Must be AFTER spPr but BEFORE dPt/dLbls per CT_ScatterSer)
        if (type %in% c("lineChart", "scatterChart", "radarChart", "stockChart")) {
          mkr_symbol <- if (type == "scatterChart" && (is.null(s$marker$symbol) || s$marker$symbol == "none")) "circle" else s$marker$symbol
          mkr <- xml_add_child(ser, "c:marker")
          xml_add_child(mkr, "c:symbol", val = mkr_symbol)
          if (!is.null(mkr_symbol) && mkr_symbol != "none") {
            xml_add_child(mkr, "c:size", val = as.character(s$marker$size))
            m_spPr <- xml_add_child(mkr, "c:spPr")
            # Fill and Line are now separate
            private$render_fill_style(m_spPr, s$marker$fill)
            private$render_line_style(m_spPr, s$marker$line)
          }
        }

        if (type %in% c("pieChart", "doughnutChart")) {
          if (!is.null(self$expansion)) {
            xml_add_child(ser, "c:explosion", val = as.character(self$expansion))
          }
        }

        # 4. dPt (Data Points)
        if (type %in% c("bubbleChart", "pieChart", "doughnutChart")) {
          palette <- s$line$color %||% self$palette
          # for (i in (seq_along(palette) - 1L)) {
          #   dPt <- xml_add_child(ser, "c:dPt")
          #   xml_add_child(dPt, "c:idx", val = as.character(i))
          #   sp_dpt <- xml_add_child(dPt, "c:spPr")
          #   private$render_color_core(xml_add_child(sp_dpt, "a:solidFill"), palette[(i %% length(palette)) + 1])
          #   ln_dpt <- xml_add_child(sp_dpt, "a:ln", w = "9525")
          #   private$render_color_core(xml_add_child(ln_dpt, "a:solidFill"), "FFFFFF")
          # }

          for (i in seq_along(self$palette)) {
            dPt <- xml_add_child(ser, "c:dPt")
            xml_add_child(dPt, "c:idx", val = as.character(i - 1))
            spPr <- xml_add_child(dPt, "c:spPr")
            private$render_color_core(xml_add_child(spPr, "a:solidFill"), self$palette[i])
          }
        } else {
          if (length(s$color) > 1) {
            # If s$color is a vector, apply colors to individual points
              for (i in seq_along(s$color)) {
                dPt <- xml_add_child(ser, "c:dPt")
                xml_add_child(dPt, "c:idx", val = as.character(i - 1))
                spPr <- xml_add_child(dPt, "c:spPr")
                private$render_color_core(xml_add_child(spPr, "a:solidFill"), s$color[i])
              }
          }
        }

        # 5. dLbls (Data Labels)
        lp <- s$label_params %||% self$label_params

        # Only enter if lp exists AND at least one show flag is TRUE
        if (!is.null(lp) && (isTRUE(lp$show_val) || isTRUE(lp$show_cat) || isTRUE(lp$show_legend_key))) {

          dLbls <- xml_add_child(ser, "c:dLbls")

          # A. txPr (Styling)
          if (length(lp$style) > 0) {
            private$apply_text_style(dLbls, lp$style)
          }

          # B. dLblPos
          final_pos <- lp$pos
          if (type == "barChart") {
            if (final_pos == "t")      final_pos <- "outEnd"
            else if (final_pos == "b") final_pos <- "inBase"
          } else if (type %in% c("pieChart", "doughnutChart")) {
            final_pos <- "bestFit"
          }

          if (!is.null(final_pos)) {
            xml_add_child(dLbls, "c:dLblPos", val = final_pos)
          }

          # C. show flags
          xml_add_child(dLbls, "c:showLegendKey",  val = if (isTRUE(lp$show_legend_key)) "1" else "0")
          xml_add_child(dLbls, "c:showVal",        val = if (isTRUE(lp$show_val)) "1" else "0")
          xml_add_child(dLbls, "c:showCatName",    val = if (isTRUE(lp$show_cat)) "1" else "0")
          xml_add_child(dLbls, "c:showSerName",    val = "0")
          xml_add_child(dLbls, "c:showPercent",    val = "0")
          xml_add_child(dLbls, "c:showBubbleSize", val = "0")
        }

        # 1. Trendline (Basic)
        if (is.list(s$trendline)) {
          tl <- xml_add_child(ser, "c:trendline")

          # 1. Name (Optional)
          if (!is.null(s$trendline$name)) {
            xml_add_child(tl, "c:name", s$trendline$name)
          }

          # 2. spPr (STYLING) - Must come BEFORE trendlineType
          if (!is.null(s$trendline$color)) {
            sp_pr <- xml_add_child(tl, "c:spPr")
            ln <- xml_add_child(sp_pr, "a:ln")
            private$render_color_core(xml_add_child(ln, "a:solidFill"), s$trendline$color)
          }

          # 3. trendlineType (MANDATORY)
          xml_add_child(tl, "c:trendlineType", val = s$trendline$type %||% "linear")

          # 4. Polynomial Order / Moving Average Period
          if (!is.null(s$trendline$order)) xml_add_child(tl, "c:order", val = as.character(s$trendline$order))
          if (!is.null(s$trendline$period)) xml_add_child(tl, "c:period", val = as.character(s$trendline$period))

          # 5. dispRSqr (R-Squared) - Must come AFTER trendlineType
          if (isFALSE(s$trendline$show_r2)) {
            xml_add_child(tl, "c:dispRSqr", val = "0")
          }

          # 6. dispEq (Equation)
          if (isFALSE(s$trendline$show_eq)) {
            xml_add_child(tl, "c:dispEq", val = "0")
          }
        }

        # 2. Error Bars (Basic)
        if (is.list(s$error_bars)) {
          eb <- xml_add_child(ser, "c:errBars")

          # Required: direction (y) and types
          xml_add_child(eb, "c:errDir", val = "y")
          xml_add_child(eb, "c:errBarType", val = "both")
          xml_add_child(eb, "c:errValType", val = s$error_bars$type %||% "fixedVal")

          # Required: the value itself
          xml_add_child(eb, "c:val", val = as.character(s$error_bars$value %||% 5))

          # Add Color Styling
          if (!is.null(s$error_bars$color)) {
            sp_pr <- xml_add_child(eb, "c:spPr")
            ln <- xml_add_child(sp_pr, "a:ln")
            private$render_color_core(xml_add_child(ln, "a:solidFill"), s$error_bars$color)
          }
        }

        # 6. Data References (xVal/yVal or label/val)
        if (type %in% c("scatterChart", "bubbleChart")) {
          if (!is.null(s$label)) {
            x_val_node <- xml_add_child(ser, "c:xVal")
            if (!is.null(s$cat_cache)) {
              ref_node <- xml_add_child(x_val_node, "c:numRef")
              xml_add_child(ref_node, "c:f", s$label)
              private$render_num_cache(ref_node, s$cat_cache)
            } else {
              ref_type <- if (grepl("!", s$label)) "c:numRef" else "c:numLit"
              xml_add_child(xml_add_child(x_val_node, ref_type), "c:f", s$label)
            }
          }

          y_val_node <- xml_add_child(ser, "c:yVal")
          if (!is.null(s$data_cache)) {
            ref_node <- xml_add_child(y_val_node, "c:numRef")
            xml_add_child(ref_node, "c:f", s$data)
            private$render_num_cache(ref_node, s$data_cache)
          } else {
            y_ref_type <- if (grepl("!", s$data)) "c:numRef" else "c:numLit"
            xml_add_child(xml_add_child(y_val_node, y_ref_type), "c:f", s$data)
          }

          if (type == "bubbleChart") {
            z_val_node <- xml_add_child(ser, "c:bubbleSize")
            z_ref <- s$weight %||% s$data
            z_cache <- s$z_cache %||% s$data_cache
            if (!is.null(z_cache)) {
              ref_node <- xml_add_child(z_val_node, "c:numRef")
              xml_add_child(ref_node, "c:f", z_ref)
              private$render_num_cache(ref_node, z_cache)
            } else {
              z_ref_type <- if (grepl("!", z_ref)) "c:numRef" else "c:numLit"
              ref_node <- xml_add_child(z_val_node, z_ref_type)
              xml_add_child(ref_node, "c:f", z_ref)
            }
          }
        } else {
          if (!is.null(s$label)) {
            cat_node <- xml_add_child(ser, "c:cat")

            if (!is.null(s$cat_cache) && inherits(s$cat_cache, c("Date", "POSIXt"))) {
              # Date/datetime categories -> numRef with OOXML serial conversion
              ref_node <- xml_add_child(cat_node, "c:numRef")
              xml_add_child(ref_node, "c:f", s$label)
              private$render_num_cache(ref_node, s$cat_cache)
            } else if (!is.null(s$cat_cache) && is.numeric(s$cat_cache)) {
              # Numeric categories (e.g. year, integer axis)
              ref_node <- xml_add_child(cat_node, "c:numRef")
              xml_add_child(ref_node, "c:f", s$label)
              private$render_num_cache(ref_node, s$cat_cache)
            } else if (!is.null(s$cat_cache)) {
              # Character/factor categories
              ref_node <- xml_add_child(cat_node, "c:strRef")
              xml_add_child(ref_node, "c:f", s$label)
              private$render_str_cache(ref_node, s$cat_cache)
            } else {
              ref_clean <- sub("^('([^']|'')+'|[^!]+)!", "", s$label)
              ref_clean <- gsub("\\$", "", ref_clean)
              dims      <- dim(openxlsx2::dims_to_dataframe(ref_clean))
              is_multi  <- length(dims) == 2 && min(dims) > 1
              c_ref_type <- if (is_multi && grepl("!", s$label)) "c:multiLvlStrRef"
                            else if (grepl("!", s$label)) "c:strRef"
                            else "c:strLit"
              xml_add_child(xml_add_child(cat_node, c_ref_type), "c:f", s$label)
            }
          }

          val_node <- xml_add_child(ser, "c:val")
          if (!is.null(s$data_cache)) {
            ref_node <- xml_add_child(val_node, "c:numRef")
            xml_add_child(ref_node, "c:f", s$data)
            private$render_num_cache(ref_node, s$data_cache)
          } else {
            v_ref_type <- if (grepl("!", s$data)) "c:numRef" else "c:numLit"
            xml_add_child(xml_add_child(val_node, v_ref_type), "c:f", s$data)
          }
        }

        # 7. Smooth (Final property for Line/Scatter)
        if (type %in% c("lineChart", "scatterChart", "stockChart")) {
          xml_add_child(ser, "c:smooth", val = if (isTRUE(s$smooth)) "1" else "0")
        }

      }

      # 1. Drop Lines
      if (isTRUE(self$drop_lines)) {
        if (is.null(self$series_data[[1]][["data_cache"]])) {
          message("drop lines require wb_data() input")
        }
        dl <- xml_add_child(c_node, "c:dropLines")
        private$render_line_style(
          xml_add_child(dl, "c:spPr"),
          list(color = "000000", width = 0.75, show = TRUE)
        )
      }

      # 2. High-Low Lines
      if (isTRUE(self$high_low_lines)) {
        if (is.null(self$series_data[[1]][["data_cache"]])) {
          message("high low lines require wb_data() input")
        }
        hl <- xml_add_child(c_node, "c:hiLowLines")
        private$render_line_style(
          xml_add_child(hl, "c:spPr"),
          list(color = "000000", width = 0.75, show = TRUE)
        )
      }

      # 3. Up/Down Bars
      if (isTRUE(self$up_down_bars)) {
        udb <- xml_add_child(c_node, "c:upDownBars")
        gapWidth <- sub_series[[1]]$gap_width %||% 150
        xml_add_child(udb, "c:gapWidth", val = as.character(gapWidth)) # Default gap

        # Style Up Bars (typically white/green)
        up_bars <- xml_add_child(udb, "c:upBars")
        # Style Down Bars (typically black/red)
        down_bars <- xml_add_child(udb, "c:downBars")
      }

      # 3. POST-SERIES PROPERTIES (Sequence Sensitive)

      if (type == "bubbleChart") {
        # xml_add_child(c_node, "c:bubble3D", val = "0")
        xml_add_child(c_node, "c:bubbleScale", val = as.character(self$bubble_scale))
        xml_add_child(c_node, "c:showNegBubbles", val = as.character(as.numeric(self$show_neg_bubbles)))
      }

      # gapWidth and overlap MUST follow <c:ser> but come before <c:axId>
      if (type == "barChart") {
        if (!is.null(sub_series[[1]]$gap_width)) {
          xml_add_child(c_node, "c:gapWidth", val = as.character(sub_series[[1]]$gap_width))
        }
        if (!is.null(sub_series[[1]]$overlap)) {
          xml_add_child(c_node, "c:overlap", val = as.character(sub_series[[1]]$overlap))
        }
      }

      # doughnutChart holeSize
      if (type %in% c("pieChart", "doughnutChart")) {
        if (!is.null(self$first_slice_ang)) {
          xml_add_child(c_node, "c:firstSliceAng", val = as.character(self$first_slice_ang))
        }
      }
      if (type == "doughnutChart") {
        xml_add_child(c_node, "c:holeSize", val = as.character(self$hole_size %||% 75))
      }

      # 4. AXIS IDS (Must be the last elements in Bar/Line/Scatter)
      if (type %in% c("bubbleChart", "lineChart", "areaChart", "barChart", "scatterChart", "radarChart", "stockChart", "surfaceChart")) {
        xml_add_child(c_node, "c:axId", val = as.character(cat_id))
        xml_add_child(c_node, "c:axId", val = as.character(val_id))
        if (type == "surfaceChart") {
          xml_add_child(c_node, "c:axId", val = as.character(ser_id))
        }
      }
    },

    add_title_content = function(node, text, style = list(), default_sz = 1000) {
      tx <- xml_add_child(node, "c:tx")
      rich <- xml_add_child(tx, "c:rich")
      xml_add_child(rich, "a:bodyPr")
      xml_add_child(rich, "a:lstStyle")
      p <- xml_add_child(rich, "a:p")
      if (inherits(text, "fmt_txt")) {
        wrapper <- sprintf('<x xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">%s</x>', fmt_txt2(text))
        xml_add_child(p, xml_find_all(read_xml(wrapper), ".//a:r"))
      } else {
        sz <- if (!is.null(style$font_size)) style$font_size * 100 else default_sz
        r <- xml_add_child(p, "a:r")
        rPr <- xml_add_child(r, "a:rPr", sz = as.character(sz))
        if (isTRUE(style$bold)) xml_set_attr(rPr, "b", "1")
        if (isTRUE(style$italic)) xml_set_attr(rPr, "i", "1")
        if (!is.null(style$font_color)) private$render_color_core(xml_add_child(rPr, "a:solidFill"), style$font_color)
        if (!is.null(style$font_name)) xml_add_child(rPr, "a:latin", typeface = style$font_name)
        xml_add_child(r, "a:t", text)
      }
    },

    render_cat_ax = function(parent, id, cross_id, pos, delete = "0", title_obj = NULL, params = NULL, crosses = "autoZero") {
      is_date <- !is.null(params$major_time) || !is.null(params$minor_time) || !is.null(params$base_time)
      node_name <- if (is_date) "c:dateAx" else "c:catAx"
      ax <- xml_add_child(parent, node_name)

      # 1. Identity and Scaling (EG_AxShared Start)
      xml_add_child(ax, "c:axId", val = id)
      scaling <- xml_add_child(ax, "c:scaling")
      xml_add_child(scaling, "c:orientation", val = "minMax")
      if (!is.null(params$max)) xml_add_child(scaling, "c:max", val = as.character(params$max))
      if (!is.null(params$min)) xml_add_child(scaling, "c:min", val = as.character(params$min))

      # 2. Basic Properties
      xml_add_child(ax, "c:delete", val = delete)
      xml_add_child(ax, "c:axPos", val = pos)

      # 3. Gridlines
      if (!is.null(params$grid_lines) && !isFALSE(params$grid_lines)) {
        g <- xml_add_child(ax, "c:majorGridlines")
        grid_style <- list(color = params$grid_color %||% "D9D9D9", width = params$grid_width, type = params$grid_lines)
        private$render_line_style(xml_add_child(g, "c:spPr"), grid_style)
      }
      if (!is.null(params$minor_grid_lines) && !isFALSE(params$minor_grid_lines)) {
        mg <- xml_add_child(ax, "c:minorGridlines")
        m_style <- list(color = params$minor_grid_color %||% "F2F2F2", width = params$minor_grid_width, type = params$minor_grid_lines)
        private$render_line_style(xml_add_child(mg, "c:spPr"), m_style)
      }

      # 4. Title
      if (!is.null(title_obj$text) && delete == "0") {
        t_node <- xml_add_child(ax, "c:title")
        private$add_title_content(t_node, title_obj$text, title_obj$style)
        xml_add_child(t_node, "c:layout")
        xml_add_child(t_node, "c:overlay", val = "0")
      }

      # 5. Number Format & Tick Labels
      if (!is.null(params$format)) {
        xml_add_child(ax, "c:numFmt", formatCode = params$format, sourceLinked = "0")
      }
      if (!is.null(params$major_tick)) {
        xml_add_child(ax, "c:majorTickMark", val = params$major_tick)
      }
      if (!is.null(params$minor_tick)) {
        xml_add_child(ax, "c:minorTickMark", val = params$minor_tick)
      }
      xml_add_child(ax, "c:tickLblPos", val = params$label_pos %||% "nextTo")

      # 6. Visual Styles
      ln <- xml_add_child(xml_add_child(ax, "c:spPr"), "a:ln")
      private$render_color_core(xml_add_child(ln, "a:solidFill"), params$color %||% "000000")

      label_style <- params
      label_style$color <- params$font_color %||% params$color %||% "000000"
      private$apply_text_style(ax, label_style)
      # 7. Crossing (EG_AxShared)
      xml_add_child(ax, "c:crossAx", val = cross_id)

      if (!is.null(params$crosses_at)) {
        # Use a specific value (e.g., cross at Y=100)
        xml_add_child(ax, "c:crossesAt", val = as.character(params$crosses_at))
      } else {
        # Use a preset: 'autoZero', 'min', or 'max'
        # Use the 'crosses' argument passed from the render() function
        cross_val <- params$crosses %||% crosses
        xml_add_child(ax, "c:crosses", val = cross_val)
      }

      # 8. Axis Specifics
      if (is_date) {
        # Sequence for DateAx: lblOffset -> baseTimeUnit -> majorUnit -> minorUnit
        xml_add_child(ax, "c:auto", val = "1")
        xml_add_child(ax, "c:lblOffset", val = "100")

        if (!is.null(params$base_time)) {
          private$validate_input(params$base_time, c("days", "months", "years"), "base_time")
          xml_add_child(ax, "c:baseTimeUnit", val = params$base_time)
        }
        if (!is.null(params$major)) {
          xml_add_child(ax, "c:majorUnit", val = as.character(params$major))
          private$validate_input(params$major_time, c("days", "months", "years"), "major_time")
          if (!is.null(params$major_time)) xml_add_child(ax, "c:majorTimeUnit", val = params$major_time)
        }
        if (!is.null(params$minor)) {
          xml_add_child(ax, "c:minorUnit", val = as.character(params$minor))
          private$validate_input(params$minor_time, c("days", "months", "years"), "minor_time")
          if (!is.null(params$minor_time)) xml_add_child(ax, "c:minorTimeUnit", val = params$minor_time)
        }
      } else {
        # Sequence for CatAx: auto -> lblAlgn -> lblOffset -> skip logic
        xml_add_child(ax, "c:auto", val = "1")
        xml_add_child(ax, "c:lblOffset", val = "100")
        if (!is.null(params$tick_lbl_skip)) xml_add_child(ax, "c:tickLblSkip", val = as.character(params$tick_lbl_skip))
        xml_add_child(ax, "c:noMultiLvlLbl", val = "0")
      }
    },

    render_val_ax = function(parent, id, cross_id, pos, title_obj = NULL, delete = "0", crosses = "autoZero", params = NULL) {
      ax <- xml_add_child(parent, "c:valAx")

      # 1. Identity and Scaling
      xml_add_child(ax, "c:axId", val = id)
      scaling <- xml_add_child(ax, "c:scaling")
      xml_add_child(scaling, "c:orientation", val = "minMax")
      if (!is.null(params$max)) xml_add_child(scaling, "c:max", val = as.character(params$max))
      if (!is.null(params$min)) xml_add_child(scaling, "c:min", val = as.character(params$min))

      # 2. Delete and Position
      xml_add_child(ax, "c:delete", val = delete)
      xml_add_child(ax, "c:axPos", val = pos)

      # 3. Gridlines (MUST come here, before Title and NumFmt)
      if (!is.null(params$grid_lines) && !isFALSE(params$grid_lines)) {
        g <- xml_add_child(ax, "c:majorGridlines")
        style <- list(color = params$grid_color %||% "D9D9D9", width = params$grid_width, type = params$grid_lines)
        private$render_line_style(xml_add_child(g, "c:spPr"), style)
      }
      if (!is.null(params$minor_grid_lines) && !isFALSE(params$minor_grid_lines)) {
        mg <- xml_add_child(ax, "c:minorGridlines")
        m_style <- list(color = params$minor_grid_color %||% "F2F2F2", width = params$minor_grid_width, type = params$minor_grid_lines)
        private$render_line_style(xml_add_child(mg, "c:spPr"), m_style)
      }

      # 4. Title
      if (!is.null(title_obj$text)) {
        t_node <- xml_add_child(ax, "c:title")
        private$add_title_content(t_node, title_obj$text, title_obj$style)
        xml_add_child(t_node, "c:layout")
        xml_add_child(t_node, "c:overlay", val = "0")
      }

      # 5. Number Format
      if (!is.null(params$format)) {
        xml_add_child(ax, "c:numFmt", formatCode = params$format, sourceLinked = "0")
      }
      if (!is.null(params$major_tick)) {
        xml_add_child(ax, "c:majorTickMark", val = params$major_tick)
      }
      if (!is.null(params$minor_tick)) {
        xml_add_child(ax, "c:minorTickMark", val = params$minor_tick)
      }

      xml_add_child(ax, "c:tickLblPos", val = params$label_pos %||% "nextTo")

      # 6. Shape and Text Properties
      ax_style <- list(color = params$color %||% "000000", width = params$line_width)
      private$render_line_style(xml_add_child(ax, "c:spPr"), ax_style)

      label_style <- params
      label_style$color <- params$font_color %||% params$color %||% "000000"
      private$apply_text_style(ax, label_style)

      # 7. Crossing Properties (End of EG_AxShared)
      xml_add_child(ax, "c:crossAx", val = cross_id)
      cross_val <- params$crosses %||% crosses
      if (!is.null(params$crosses_at)) {
        # If a specific value is provided, it overrides the 'crosses' string
        xml_add_child(ax, "c:crossesAt", val = as.character(params$crosses_at))
      } else {
        xml_add_child(ax, "c:crosses", val = cross_val)
      }
      cb_val <- params$cross_between %||% "between"
      xml_add_child(ax, "c:crossBetween", val = cb_val)

      # 8. Units (End of ValAx)
      if (!is.null(params$major)) xml_add_child(ax, "c:majorUnit", val = as.character(params$major))
      if (!is.null(params$minor)) xml_add_child(ax, "c:minorUnit", val = as.character(params$minor))
    },

    render_ser_ax = function(parent, id, cross_id) {
      ax <- xml_add_child(parent, "c:serAx")
      xml_add_child(ax, "c:axId", val = as.character(id))

      scaling <- xml_add_child(ax, "c:scaling")
      xml_add_child(scaling, "c:orientation", val = "minMax")

      xml_add_child(ax, "c:delete", val = "0")
      xml_add_child(ax, "c:axPos", val = "b")
      xml_add_child(ax, "c:tickLblPos", val = "nextTo")
      xml_add_child(ax, "c:crossAx", val = as.character(cross_id))
      xml_add_child(ax, "c:crosses", val = "autoZero")
    },

    # Emit a c:numCache block into ref_node.
    # Date/POSIXt values are converted to OOXML serials via convert_to_excel_date.
    # Plain numeric values are written as-is.
    render_num_cache = function(ref_node, vals) {
      cache <- xml_add_child(ref_node, "c:numCache")
      if (inherits(vals, c("Date", "POSIXt"))) {
        vals <- openxlsx2::convert_to_excel_date(data.frame(d = vals))[[1]]

        fmt <- if (inherits(vals, "POSIXt"))
          getOption("openxlsx2.datetimeFormat", "yyyy-mm-dd hh:mm:ss")
        else
          getOption("openxlsx2.dateFormat", "mm/dd/yyyy")
        xml_add_child(cache, "c:formatCode", fmt)
      }
      xml_add_child(cache, "c:ptCount", val = as.character(length(vals)))
      for (i in seq_along(vals)) {
        if (!is.na(vals[[i]])) {
          pt <- xml_add_child(cache, "c:pt", idx = as.character(i - 1))
          xml_add_child(pt, "c:v", as.character(vals[[i]]))
        }
      }
    },

    # Emit a c:strCache block into ref_node for character/factor categories.
    render_str_cache = function(ref_node, vals) {
      cache <- xml_add_child(ref_node, "c:strCache")
      xml_add_child(cache, "c:ptCount", val = as.character(length(vals)))
      for (i in seq_along(vals)) {
        if (!is.na(vals[[i]])) {
          pt <- xml_add_child(cache, "c:pt", idx = as.character(i - 1))
          xml_add_child(pt, "c:v", as.character(vals[[i]]))
        }
      }
    },

    apply_text_style = function(node, s) {
      txPr <- xml_add_child(node, "c:txPr")

      # 1. Create body properties and apply rotation
      bodyPr <- xml_add_child(
        txPr, "a:bodyPr",
        lIns = "0", tIns = "0", rIns = "0", bIns = "0", wrap = "square"
      )
      if (!is.null(s$rotation)) {
        # rotation = degrees * 60000
        xml_set_attr(bodyPr, "rot", as.character(round(s$rotation * 60000)))
        xml_set_attr(bodyPr, "vert", "horz")
      }

      # 2. Add required list style
      xml_add_child(txPr, "a:lstStyle")

      # 3. Build the text run properties (defRPr)
      p      <- xml_add_child(txPr, "a:p")
      pPr    <- xml_add_child(p, "a:pPr")
      defRPr <- xml_add_child(pPr, "a:defRPr")

      # Apply font size (OOXML uses 1/100th of a point)
      sz <- if (!is.null(s$font_size)) s$font_size * 100 else 1000
      xml_set_attr(defRPr, "sz", as.character(sz))

      if (isTRUE(s$bold)) xml_set_attr(defRPr, "b", "1")
      if (isTRUE(s$italic)) xml_set_attr(defRPr, "i", "1")

      f_color <- s$font_color %||% s$color %||% "000000"
      private$render_color_core(xml_add_child(defRPr, "a:solidFill"), f_color)

      if (!is.null(s$font_name)) {
        xml_add_child(defRPr, "a:latin", typeface = s$font_name)
      }
    }
  )
)
