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
    #' @field of_pie_type Character; subtype of `ofPieChart`: "pie" or "bar".
    of_pie_type = "pie",
    #' @field second_pie_size Integer; size of the second pie/bar plot as a
    #'   percentage (5-200) for `ofPieChart`.
    second_pie_size = NULL,
    #' @field split_type Character; how points are split into the second plot
    #'   for `ofPieChart`: "auto", "cust", "percent", "pos", or "val".
    split_type = NULL,
    #' @field split_pos Numeric; split threshold, or point indices (0-based)
    #'   when `split_type = "cust"`.
    split_pos = NULL,
    #' @field view3d Named list of 3D view parameters (`rot_x`, `rot_y`,
    #'   `perspective`, `depth_percent`, `h_percent`, `right_angle_axes`).
    view3d = list(rot_x = NULL, rot_y = NULL, perspective = NULL,
                  depth_percent = NULL, h_percent = NULL, right_angle_axes = NULL),
    #' @field gap_depth Integer; gap depth percentage (0-500) for 3D charts.
    gap_depth = NULL,
    #' @field bar_shape Character; bar shape for `bar3DChart`: "box",
    #'   "cylinder", "cone", "coneToMax", "pyramid", or "pyramidToMax".
    bar_shape = NULL,
    #' @field size_represents Character; bubble size meaning, "area" or "w".
    size_represents = NULL,
    #' @field template List of series styles from a chart template; see
    #'   [encharter_from_crtx()].
    template = list(),
    #' @field plot_layout Fixed position of the plot area as fractions of the
    #'   chart (`x`, `y`, `w`, `h`, `target`), or `NULL` for automatic layout.
    plot_layout = NULL,
    #' @field text_style Default text properties of the chart (`font_size`,
    #'   `font_name`, `font_color`, `bold`, `italic`), used by text without
    #'   its own.
    text_style = list(),

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
    #' @param rev Logical to reverse the value order
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
    #' @param tick_lbl_skip,tick_mark_skip Integer (>= 1); label/tick every n-th category (category axes only).
    #' @param disp_units Display units: a built-in unit string (e.g. "thousands") or a positive number (value axes only).
    set_y2_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                           major_time = NULL, minor_time = NULL, base_time = NULL,
                           major_tick = NULL, minor_tick = NULL,
                           format = NULL, log_base = NULL, rev = NULL, color = NULL,
                           font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                           font_color = NULL, rotation =  NULL,
                           grid_color = NULL, grid_lines = NULL,
                           minor_grid_color = NULL, minor_grid_lines = NULL, cross_between = NULL,
                           line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                           crosses = "max", crosses_at = NULL, label_pos = NULL,
                           tick_lbl_skip = NULL, tick_mark_skip = NULL, disp_units = NULL) {
        private$set_axis_params(
          "y2",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, rev = rev, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rotation = rotation, grid_color = grid_color, grid_lines = grid_lines,
          minor_grid_color = minor_grid_color, minor_grid_lines = minor_grid_lines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos,
          tick_lbl_skip = tick_lbl_skip, tick_mark_skip = tick_mark_skip,
          disp_units = disp_units
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
    #' @param rev Logical to reverse the value order
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
    #' @param tick_lbl_skip,tick_mark_skip Integer (>= 1); label/tick every n-th category (category axes only).
    #' @param disp_units Display units: a built-in unit string (e.g. "thousands") or a positive number (value axes only).
    set_x2_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                           major_time = NULL, minor_time = NULL, base_time = NULL,
                           major_tick = NULL, minor_tick = NULL,
                           format = NULL, log_base = NULL, rev = NULL, color = NULL,
                           font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                           font_color = NULL, rotation =  NULL,
                           grid_color = NULL, grid_lines = NULL,
                           minor_grid_color = NULL, minor_grid_lines = NULL, cross_between = NULL,
                           line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                           crosses = "max", crosses_at = NULL, label_pos = NULL,
                           tick_lbl_skip = NULL, tick_mark_skip = NULL, disp_units = NULL) {

        private$set_axis_params(
          "x2",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, rev = rev, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rotation = rotation, grid_color = grid_color, grid_lines = grid_lines,
          minor_grid_color = minor_grid_color, minor_grid_lines = minor_grid_lines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos,
          tick_lbl_skip = tick_lbl_skip, tick_mark_skip = tick_mark_skip,
          disp_units = disp_units
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

      check_num(rotation, "rotation", min = 0, max = 360, integer = TRUE)
      check_num(expansion, "expansion", min = 0, integer = TRUE)

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

    #' @description Configure the Pie of Pie / Bar of Pie chart
    #'   (`ofPieChart`).
    #' @param type Subtype: `"pie"` (Pie of Pie, default) or `"bar"`
    #'   (Bar of Pie).
    #' @param second_size Size of the second plot as a percentage of the main
    #'   pie, from 5 to 200. Default 75.
    #' @param split_type How data points are assigned to the second plot:
    #'   `"auto"` (default), `"percent"`, `"pos"` (last n
    #'   points), `"val"` (values below threshold), or `"cust"`.
    #' @param split_pos Numeric split threshold for `"percent"`,
    #'   `"pos"`, and `"val"`; for `"cust"` a vector of
    #'   0-based point indices to move to the second plot.
    #' @examples
    #' ec("ofPieChart")$set_of_pie_options(type = "bar", split_type = "pos", split_pos = 3)
    set_of_pie_options = function(type = NULL, second_size = NULL,
                                  split_type = NULL, split_pos = NULL) {
      type <- check_choice(type, c("pie", "bar"), "type")
      check_num(second_size, "second_size", min = 5, max = 200)
      split_type <- check_choice(split_type, c("auto", "cust", "percent", "pos", "val"), "split_type")

      if (!is.null(split_pos)) {
        if (identical(split_type %||% self$split_type, "cust")) {
          if (!is.numeric(split_pos) || anyNA(split_pos) || any(split_pos < 0) ||
              any(split_pos != trunc(split_pos))) {
            stop("'split_pos' must be a vector of non-negative point indices for split_type = \"cust\"", call. = FALSE)
          }
        } else {
          check_num(split_pos, "split_pos", min = 0, exclusive_min = TRUE)
        }
      }

      if (!is.null(type))        self$of_pie_type     <- type
      if (!is.null(second_size)) self$second_pie_size <- second_size
      if (!is.null(split_type))  self$split_type      <- split_type
      if (!is.null(split_pos))   self$split_pos       <- split_pos
      invisible(self)
    },

    #' @description Configure the 3D view and 3D-only chart options. Only
    #'   takes effect for the 3D chart types (`bar3DChart`,
    #'   `line3DChart`, `pie3DChart`, `area3DChart`,
    #'   `surface3DChart`) and `surfaceChart`.
    #' @param rot_x Rotation around the X-axis in degrees, from -90 to 90.
    #' @param rot_y Rotation around the Y-axis in degrees, from 0 to 360.
    #' @param perspective Perspective in half-degrees, from 0 to 240 (ignored
    #'   when `right_angle_axes = TRUE`).
    #' @param depth_percent Depth as a percentage of chart width, 20 to 2000.
    #' @param h_percent Height as a percentage of chart width, 5 to 500.
    #' @param right_angle_axes Logical; render axes at right angles instead of
    #'   in perspective.
    #' @param gap_depth Gap depth percentage between series, 0 to 500
    #'   (bar/line/area 3D).
    #' @param shape Bar shape for `bar3DChart`: `"box"` (default),
    #'   `"cylinder"`, `"cone"`, `"coneToMax"`,
    #'   `"pyramid"`, or `"pyramidToMax"`.
    #' @examples
    #' ec("bar3DChart")$set_3d_options(rot_x = 20, rot_y = 30, shape = "cylinder")
    set_3d_options = function(rot_x = NULL, rot_y = NULL, perspective = NULL,
                              depth_percent = NULL, h_percent = NULL,
                              right_angle_axes = NULL,
                              gap_depth = NULL, shape = NULL) {
      check_num(rot_x, "rot_x", min = -90, max = 90, integer = TRUE)
      check_num(rot_y, "rot_y", min = 0, max = 360, integer = TRUE)
      check_num(perspective, "perspective", min = 0, max = 240, integer = TRUE)
      check_num(depth_percent, "depth_percent", min = 20, max = 2000, integer = TRUE)
      check_num(h_percent, "h_percent", min = 5, max = 500, integer = TRUE)
      check_bool(right_angle_axes, "right_angle_axes")
      check_num(gap_depth, "gap_depth", min = 0, max = 500, integer = TRUE)
      shape <- check_choice(shape, c("box", "cylinder", "cone", "coneToMax", "pyramid", "pyramidToMax"), "shape")

      new_view <- list(rot_x = rot_x, rot_y = rot_y, perspective = perspective,
                       depth_percent = depth_percent, h_percent = h_percent,
                       right_angle_axes = right_angle_axes)
      self$view3d <- modifyList(self$view3d, Filter(Negate(is.null), new_view))
      if (!is.null(gap_depth)) self$gap_depth <- gap_depth
      if (!is.null(shape))     self$bar_shape <- shape
      invisible(self)
    },

    #' @param scale The scale factor for bubbles, from 0 to 300 (expressed as a percentage).
    #' @param show_neg Logical; if `TRUE`, bubbles with negative values will be displayed on the chart.
    #' @param size_represents What the bubble size encodes: `"area"`
    #'   (default in Excel) or `"w"` (width/diameter). `NULL` omits the
    #'   element.
    set_bubble_options = function(scale = 100, show_neg = FALSE, size_represents = NULL) {
      check_num(scale, "scale", min = 0, max = 300)
      check_bool(show_neg, "show_neg")
      self$size_represents <- check_choice(size_represents, c("area", "w"), "size_represents")
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
    #'   * `axis`: Error direction axis, `"y"` (default) or `"x"` (horizontal
    #'     bars, scatter charts).
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
    #'   * `forward`, `backward`: Numeric; extrapolate the line n periods
    #'     forwards/backwards.
    #'   * `intercept`: Numeric; force the line through a fixed y-intercept.
    #'   * `color`: Hex color code for the line.
    #'   * `show_r2`: Logical; if `TRUE`, displays the R-squared value on the chart.
    #'
    #' @param invert_if_negative Logical; bar charts only. Invert the fill for
    #'   negative values. Default `FALSE`.
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
                          filled = FALSE, error_bars = FALSE, trendline = FALSE,
                          invert_if_negative = FALSE) {

      # styling from a chart template for the arguments not given
      if (length(self$template)) {
        tpl <- self$template[[(length(self$series_data) %% length(self$template)) + 1]]
        if (missing(color) && !is.null(tpl$line$color)) color <- tpl$line$color
        if (missing(line_width) && !is.null(tpl$line$width)) line_width <- tpl$line$width
        if (missing(line_type) && !is.null(tpl$line$type)) line_type <- tpl$line$type
        if (missing(show_line) && !is.null(tpl$line$show)) show_line <- tpl$line$show
        if (missing(marker) && !is.null(tpl$marker$symbol)) marker <- tpl$marker$symbol
        if (missing(marker_size) && !is.null(tpl$marker$size)) marker_size <- tpl$marker$size
        if (missing(marker_fill) && !is.null(tpl$marker$fill)) marker_fill <- tpl$marker$fill
        if (missing(marker_line) && !is.null(tpl$marker$line$color)) marker_line <- tpl$marker$line$color
        if (missing(marker_line_width) && !is.null(tpl$marker$line$width)) marker_line_width <- tpl$marker$line$width
        if (missing(smooth) && !is.null(tpl$smooth)) smooth <- tpl$smooth
        if (missing(invert_if_negative) && !is.null(tpl$invert_if_negative)) invert_if_negative <- tpl$invert_if_negative
        if (missing(type) && !is.null(tpl$type)) type <- tpl$type
        if (missing(secondary) && !is.null(tpl$sec_type)) secondary <- switch(tpl$sec_type, none = FALSE, y = TRUE, tpl$sec_type)
        if (missing(dir) && !is.null(tpl$dir)) dir <- tpl$dir
        if (missing(grouping) && !is.null(tpl$grouping)) grouping <- tpl$grouping
        if (missing(overlap) && !is.null(tpl$overlap)) overlap <- tpl$overlap
        if (missing(gap_width) && !is.null(tpl$gap_width)) gap_width <- tpl$gap_width
        if (missing(filled) && !is.null(tpl$filled)) filled <- tpl$filled
      }

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

      # Schema range validation (ECMA-376 dml-chart.xsd)
      check_num(overlap, "overlap", min = -100, max = 100, integer = TRUE)   # ST_Overlap
      check_num(gap_width, "gap_width", min = 0, max = 500, integer = TRUE) # ST_GapAmount
      check_num(marker_size, "marker_size", min = 2, max = 72, integer = TRUE) # ST_MarkerSize
      check_num(line_width, "line_width", min = 0)
      check_num(marker_line_width, "marker_line_width", min = 0)
      check_trendline(trendline)
      check_bool(invert_if_negative, "invert_if_negative")
      check_error_bars(error_bars)
      color       <- check_color(color, "color")
      line_color  <- check_color(line_color, "line_color")
      marker_fill <- check_color(marker_fill, "marker_fill")
      marker_line <- check_color(marker_line, "marker_line")

      sec_val <- if (isTRUE(secondary)) "y"
        else if (isFALSE(secondary)) "none"
        else match.arg(secondary, c("x", "y", "xy", "none"))

      series_type <- type %||% self$type %||% "barChart"
      series_type <- normalize_encharter_type(series_type)
      self$type <- series_type
      if (!is.null(color) && length(color) > 1 && series_type %in% c("bubbleChart", "pieChart", "doughnutChart")) self$palette <- color

      h_label <- tryCatch(if (is.symbol(substitute(name))) deparse1(substitute(name)) else name, error = function(e) NULL)
      c_label <- tryCatch(if (is.symbol(substitute(label))) deparse1(substitute(label)) else label, error = function(e) NULL)
      z_label <- tryCatch(if (is.symbol(substitute(weight))) deparse1(substitute(weight)) else weight, error = function(e) NULL)

      if (is.null(color)) {
        color_idx <- (length(self$series_data) %% length(self$palette)) + 1
        color <- self$palette[color_idx]
      }

      data_vals <- NULL
      cat_vals <- NULL
      z_vals <- NULL
      name_vals <- NULL
      if (inherits(data, "wb_data")) {
        res <- private$resolve_wb_data(data, h_label, c_label, z_label)
        name_vals <- if (!is.null(res$name)) h_label
        name      <- res$name
        data      <- res$data
        label     <- res$label
        weight    <- res$weight
        data_vals <- res$data_vals
        cat_vals  <- res$cat_vals
        z_vals    <- res$z_vals

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
        name_cache = name_vals,
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
        invert_if_negative = invert_if_negative,

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
      if (length(self$template)) {
        n <- length(self$series_data)
        self$series_data[[n]]$border <- tpl$border
        self$series_data[[n]]$label_params <- tpl$label_params
      }

      invisible(self)
    },

    #' @description Apply the styling of a chart template (`.crtx`) to this
    #'   chart: chart and plot area, title and legend style, axes, and the
    #'   styling of the series in order. Series data, ranges and titles are
    #'   kept. Series added afterwards take the template styling as well.
    #' @param path Path of the `.crtx` file.
    #' @return The chart, invisibly.
    apply_crtx = function(path) {
      tpl <- encharter_from_crtx(path)
      self$chart_style <- tpl$chart_style
      self$plot_style <- tpl$plot_style
      self$text_style <- tpl$text_style
      self$legend_params <- tpl$legend_params
      self$label_params <- tpl$label_params
      self$chart_title$style <- tpl$chart_title$style
      for (nm in c("x_title", "y_title", "x2_title", "y2_title")) {
        if (!is.null(self[[nm]]$text)) self[[nm]]$style <- tpl[[nm]]$style
      }
      for (nm in names(self$axis_params)) {
        self$axis_params[[nm]] <- tpl$axis_params[[nm]]
      }
      self$palette <- tpl$palette
      self$template <- tpl$template
      if (length(tpl$template)) {
        for (i in seq_along(self$series_data)) {
          t <- tpl$template[[(i - 1) %% length(tpl$template) + 1]]
          s <- self$series_data[[i]]
          s$line <- t$line
          s$border <- t$border
          s$marker <- t$marker
          s$smooth <- t$smooth
          s$invert_if_negative <- t$invert_if_negative
          s$label_params <- t$label_params
          for (f in c("dir", "grouping", "overlap", "gap_width", "filled")) s[f] <- t[f]
          self$series_data[[i]] <- s
        }
      }
      invisible(self)
    },

    #' @description Change an existing series. Takes the arguments of
    #'   `add_series()`; arguments that are not supplied keep their current
    #'   value. With a `wb_data()` object as `data` and no `name`, the column
    #'   is found from the series' current header (or data) cell, so
    #'   `update_series(data = wb_data(wb), label = Month)` re-points every
    #'   series at the current extent of the data.
    #' @param index Integer vector of series to update. Default: all series.
    #' @param name,data,label,weight,color,type,secondary,dir,grouping,overlap,gap_width,smooth,show_line,marker,marker_size,marker_fill,marker_line,marker_line_width,line_type,line_width,line_color,filled,error_bars,trendline,invert_if_negative
    #'   See `add_series()`.
    #' @examples
    #' wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(
    #'   x = data.frame(Month = month.abb[1:6], Sales = 1:6))
    #' chart <- ec("line")$add_series(name = Sales, data = openxlsx2::wb_data(wb), label = Month)
    #' wb$add_data(x = data.frame(Month = "Jul", Sales = 7), dims = "A8", col_names = FALSE)
    #' chart$update_series(data = openxlsx2::wb_data(wb), label = Month, color = "C00000")
    update_series = function(index = NULL, name = NULL, data = NULL, label = NULL, weight = NULL,
                             color = NULL, type = NULL, secondary = NULL, dir = NULL, grouping = NULL,
                             overlap = NULL, gap_width = NULL, smooth = NULL, show_line = NULL,
                             marker = NULL, marker_size = NULL, marker_fill = NULL, marker_line = NULL,
                             marker_line_width = NULL, line_type = NULL, line_width = NULL,
                             line_color = NULL, filled = NULL, error_bars = NULL, trendline = NULL,
                             invert_if_negative = NULL) {

      n <- length(self$series_data)
      if (n == 0) stop("The chart has no series to update.", call. = FALSE)
      if (is.null(index)) index <- seq_len(n)
      if (!is.numeric(index) || anyNA(index) || any(index < 1) || any(index > n)) {
        stop(sprintf("'index' must be between 1 and %d.", n), call. = FALSE)
      }

      h_label <- tryCatch(if (is.symbol(substitute(name))) deparse1(substitute(name)) else name, error = function(e) NULL)
      c_label <- tryCatch(if (is.symbol(substitute(label))) deparse1(substitute(label)) else label, error = function(e) NULL)
      z_label <- tryCatch(if (is.symbol(substitute(weight))) deparse1(substitute(weight)) else weight, error = function(e) NULL)

      if (!is.null(type)) {
        type <- normalize_encharter_type(type)
        private$validate_input(type, ENCHARTER_STANDARD, "series type")
      }
      if (!is.null(marker)) {
        marker <- private$validate_input(marker, c("none", "circle", "dash", "diamond", "dot", "plus", "square", "star", "triangle", "x"), "marker")
      }
      if (!is.null(dir)) {
        dir <- private$validate_input(normalize_encharter_string(dir), c("col", "bar"), "dir")
      }
      if (!is.null(grouping)) {
        grouping <- private$validate_input(grouping, c("standard", "clustered", "stacked", "percentStacked"), "grouping")
      }
      if (!is.null(line_type)) {
        private$validate_input(line_type, c("solid", "dash", "dot", "dashDot", "lgDash", "lgDashDot", "sysDash", "sysDot", "dashed", "dotted"), "line_type")
      }
      check_num(overlap, "overlap", min = -100, max = 100, integer = TRUE)
      check_num(gap_width, "gap_width", min = 0, max = 500, integer = TRUE)
      check_num(marker_size, "marker_size", min = 2, max = 72, integer = TRUE)
      check_num(line_width, "line_width", min = 0)
      check_num(marker_line_width, "marker_line_width", min = 0)
      if (!is.null(trendline)) check_trendline(trendline)
      if (!is.null(error_bars)) check_error_bars(error_bars)
      check_bool(smooth, "smooth")
      check_bool(show_line, "show_line")
      check_bool(filled, "filled")
      check_bool(invert_if_negative, "invert_if_negative")
      color       <- check_color(color, "color")
      line_color  <- check_color(line_color, "line_color")
      marker_fill <- check_color(marker_fill, "marker_fill")
      marker_line <- check_color(marker_line, "marker_line")
      sec_val <- if (is.null(secondary)) NULL else if (isTRUE(secondary)) "y"
        else if (isFALSE(secondary)) "none"
        else match.arg(secondary, c("x", "y", "xy", "none"))

      for (i in index) {
        s <- self$series_data[[i]]
        for (nm in c("name", "data", "label", "weight")) {
          if (!nm %in% names(s)) s[nm] <- list(NULL)
        }

        if (inherits(data, "wb_data")) {
          # without a name the column is taken from the series' own header
          # (or first data) cell
          this_h <- h_label
          if (is.null(this_h)) {
            src <- if (!is.null(s$name) && grepl("!.+", s$name)) s$name else s$data
            if (!is.null(src)) {
              cell <- gsub("\\$", "", sub("^.*!", "", src))
              cell <- sub(":.*$", "", cell)
              col  <- sub("[0-9]+$", "", cell)
              dims_cols <- sub("[0-9]+$", "", attr(data, "dims")[1, ])
              this_h <- names(data)[match(col, dims_cols)]
            }
          }
          res <- private$resolve_wb_data(data, this_h, c_label, z_label)
          if (is.null(res$data)) {
            stop(sprintf("series %d: object '%s' not found in the wb_data object", i, this_h %||% ""), call. = FALSE)
          }
          if (!is.null(res$data)) {
            s$name       <- to_abs_ref(res$name)
            s$name_cache <- if (!is.null(res$name)) this_h
            s$data       <- to_abs_ref(res$data)
            s$data_cache <- res$data_vals
          }
          if (!is.null(res$label)) {
            s$label     <- to_abs_ref(res$label)
            s$cat_cache <- res$cat_vals
          }
          if (!is.null(res$weight)) {
            s$weight  <- to_abs_ref(res$weight)
            s$z_cache <- res$z_vals
          }
        } else {
          if (!is.null(name)) {
            s$name <- to_abs_ref(name)
            s["name_cache"] <- list(NULL)
          }
          if (!is.null(data)) {
            if (!grepl("!", data)) stop("Series data must be a sheet reference (e.g., 'Sheet1!A1:A10').", call. = FALSE)
            s$data <- to_abs_ref(data)
            s["data_cache"] <- list(NULL)
          }
          if (!is.null(label)) {
            s$label <- to_abs_ref(label)
            s["cat_cache"] <- list(NULL)
          }
          if (!is.null(weight)) {
            s$weight <- to_abs_ref(weight)
            s["z_cache"] <- list(NULL)
          }
        }

        if (!is.null(color)) {
          s$line$color        <- line_color %||% color
          s$marker$fill       <- marker_fill %||% color
          s$marker$line$color <- marker_line %||% color
        }
        if (!is.null(line_color))        s$line$color <- line_color
        if (!is.null(line_width))        s$line$width <- line_width
        if (!is.null(line_type))         s$line$type  <- line_type
        if (!is.null(show_line))         s$line$show  <- show_line
        if (!is.null(marker))            s$marker$symbol <- marker
        if (!is.null(marker_size))       s$marker$size   <- marker_size
        if (!is.null(marker_fill))       s$marker$fill   <- marker_fill
        if (!is.null(marker_line))       s$marker$line$color <- marker_line
        if (!is.null(marker_line_width)) s$marker$line$width <- marker_line_width
        if (!is.null(type))              s$type      <- type
        if (!is.null(sec_val))           s$sec_type  <- sec_val
        if (!is.null(dir))               s$dir       <- dir
        if (!is.null(grouping))          s$grouping  <- grouping
        if (!is.null(overlap))           s$overlap   <- overlap
        if (!is.null(gap_width))         s$gap_width <- gap_width
        if (!is.null(smooth))            s$smooth    <- smooth
        if (!is.null(filled))            s$filled    <- filled
        if (!is.null(error_bars))        s$error_bars <- error_bars
        if (!is.null(trendline))         s$trendline  <- trendline
        if (!is.null(invert_if_negative)) s$invert_if_negative <- invert_if_negative

        self$series_data[[i]] <- s
      }
      if (!is.null(type)) self$type <- type
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
      xml_remove(xml_find_all(self$xml, "c:txPr"))
      private$apply_sp_pr(self$xml, self$chart_style)
      if (length(self$text_style) > 0) private$apply_text_style(self$xml, self$text_style)

      chart_root <- xml_find_first(self$xml, "//c:chart")
      xml_remove(xml_children(chart_root))

      if (!is.null(self$chart_title$text)) {
        t_node <- xml_add_child(chart_root, "c:title")
        private$add_title_content(t_node, self$chart_title$text, self$chart_title$style, default_sz = 1400)
        xml_add_child(t_node, "c:overlay", val = "0")
      }
      xml_add_child(chart_root, "c:autoTitleDeleted", val = if (is.null(self$chart_title$text)) "1" else "0")

      if (self$type %in% c("surfaceChart", ENCHARTER_3D)) {
        private$render_view3d(chart_root)
      }

      plot_area <- xml_add_child(chart_root, "c:plotArea")
      layout <- xml_add_child(plot_area, "c:layout")
      # a fixed plot area position (fractions of the chart), as loaded from
      # a file
      if (!is.null(self$plot_layout)) {
        ml <- xml_add_child(layout, "c:manualLayout")
        xml_add_child(ml, "c:layoutTarget", val = self$plot_layout$target %||% "inner")
        xml_add_child(ml, "c:xMode", val = "edge")
        xml_add_child(ml, "c:yMode", val = "edge")
        xml_add_child(ml, "c:x", val = as.character(self$plot_layout$x))
        xml_add_child(ml, "c:y", val = as.character(self$plot_layout$y))
        xml_add_child(ml, "c:w", val = as.character(self$plot_layout$w))
        xml_add_child(ml, "c:h", val = as.character(self$plot_layout$h))
      }

      id_prim_cat <- u_ids[1]
      id_prim_val <- u_ids[2]
      id_sec_cat  <- u_ids[3]
      id_sec_val  <- u_ids[4]
      id_ser_ax   <- u_ids[5]


      private$current_idx <- 0
      combos <- unique(lapply(self$series_data, function(x) list(type = x$type, sec_type = x$sec_type)))

      # Structural sanity checks: schema-valid combinations that Excel
      # nevertheless refuses to display.
      ser_types <- vapply(self$series_data, function(x) x$type, character(1))
      is_3d <- any(ser_types %in% ENCHARTER_3D)
      if (any(ser_types %in% ENCHARTER_PIE_FAMILY) && !all(ser_types %in% ENCHARTER_PIE_FAMILY)) {
        warning("Excel cannot combine pie-type charts (pie, doughnut, ofPie) with axis-based chart types in one plot area.", call. = FALSE)
      }
      n_stock <- sum(ser_types == "stockChart")
      if (n_stock > 0 && (n_stock < 3 || n_stock > 4)) {
        warning(sprintf("stockChart requires 3 or 4 series (High-Low-Close or Open-High-Low-Close), got %d. Excel will refuse to display the chart.", n_stock), call. = FALSE)
      }
      if (is_3d && length(unique(ser_types)) > 1) {
        warning("3D chart types cannot be combined with other chart types in one plot area.", call. = FALSE)
      }
      if (is_3d && any(vapply(self$series_data, function(x) !identical(x$sec_type, "none"), logical(1)))) {
        warning("Secondary axes are not supported for 3D chart types; the series are placed on the primary axes.", call. = FALSE)
      }
      for (ax_name in c("x", "y", "x2", "y2")) {
        p <- self$axis_params[[ax_name]]
        if (!is.null(p$log_base) && !is.null(p$min) && p$min <= 0) {
          warning(sprintf("Axis '%s': a logarithmic scale requires a positive 'min'.", ax_name), call. = FALSE)
        }
      }

      has_axes <- FALSE
      for (combo in combos) {
        sub_series <- Filter(function(x) x$type == combo$type && x$sec_type == combo$sec_type, self$series_data)

        # CASE: "x" or "xy" triggers the Secondary X-Axis (Top)
        cat_id <- if (combo$sec_type %in% c("x", "xy")) id_sec_cat else id_prim_cat

        # CASE: "y" or "xy" triggers the Secondary Y-Axis (Right)
        # Note: sec_type is "none" or "y"/"x"/"xy" based on your add_series logic
        val_id <- if (combo$sec_type %in% c("y", "xy")) id_sec_val else id_prim_val

        # 3D charts only support the primary axis system
        if (combo$type %in% ENCHARTER_3D) {
          cat_id <- id_prim_cat
          val_id <- id_prim_val
        }

        # Surface and 3D chart groups reference a third (series) axis
        ser_ax_id <- if (combo$type %in% c("surfaceChart", "bar3DChart", "line3DChart", "area3DChart", "surface3DChart")) id_ser_ax else NULL

        private$render_series_node(plot_area, sub_series, combo$type, cat_id, val_id, ser_ax_id)

        if (!combo$type %in% ENCHARTER_PIE_FAMILY) has_axes <- TRUE
      }

      if (has_axes) {
        # 1. Pre-scan (3D charts only support the primary axis system)
        needs_sec_y <- !is_3d && any(vapply(self$series_data, function(x) x$sec_type %in% c("y", "xy"), FALSE))
        needs_sec_x <- !is_3d && any(vapply(self$series_data, function(x) x$sec_type %in% c("x", "xy"), FALSE))

        # with horizontal bars the category axis sits on the left and the
        # value axis at the bottom
        horizontal <- all(vapply(self$series_data, function(x) x$type %in% c("barChart", "bar3DChart") && identical(x$dir, "bar"), logical(1)))
        cat_pos <- if (horizontal) "l" else "b"
        val_pos <- if (horizontal) "b" else "l"

        # 2. Primary X-Axis
        # Always rendered
        if (self$type %in% c("scatterChart", "bubbleChart")) {
          private$render_val_ax(plot_area, id_prim_cat, id_prim_val, cat_pos, title_obj = self$x_title, params = self$axis_params$x)
        } else {
          private$render_cat_ax(plot_area, id_prim_cat, id_prim_val, cat_pos, delete = "0", title_obj = self$x_title, params = self$axis_params$x)
        }

        # 3. Primary Y-Axis
        # Always rendered
        if (self$type == "surfaceChart") {
          private$render_val_ax(plot_area, id_prim_val, id_prim_cat, val_pos, delete = "1", title_obj = self$y_title, params = self$axis_params$y)
        } else {
          private$render_val_ax(plot_area, id_prim_val, id_prim_cat, val_pos, title_obj = self$y_title, params = self$axis_params$y)
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

        if (self$type %in% c("surfaceChart", "bar3DChart", "line3DChart", "area3DChart", "surface3DChart")) {
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

    # Turns a wb_data() object plus column names into sheet references and
    # cached values. Columns that are not found return NULL.
    resolve_wb_data = function(data, h_label, c_label, z_label) {
      wb_sheet  <- attr(data, "sheet")
      dims_mat  <- attr(data, "dims")
      col_names <- names(data)
      has_header <- nrow(dims_mat) > length(attr(data, "row.names"))
      start_row  <- if (has_header) 2 else 1

      out <- list(name = NULL, data = NULL, label = NULL, weight = NULL,
                  data_vals = NULL, cat_vals = NULL, z_vals = NULL)

      for (lbl in c(h_label, c_label, z_label)) {
        if (!lbl %in% col_names) stop(sprintf("object '%s' not found in the wb_data object", lbl), call. = FALSE)
      }

      col_idx <- which(col_names == h_label)
      if (length(col_idx) > 0) {
        col_idx <- col_idx[1]
        out$data_vals <- data[[h_label]]
        out$name <- if (has_header) sprintf("%s!%s", wb_sheet, dims_mat[1, col_idx]) else NULL
        out$data <- sprintf("%s!%s:%s", wb_sheet, dims_mat[start_row, col_idx], dims_mat[nrow(dims_mat), col_idx])
      }

      cat_idx <- which(col_names == c_label)
      if (length(cat_idx) > 0) {
        cat_idx <- cat_idx[1]
        out$cat_vals <- data[[c_label]]
        out$label <- sprintf("%s!%s:%s", wb_sheet, dims_mat[start_row, cat_idx], dims_mat[nrow(dims_mat), cat_idx])
      }

      z_idx <- which(col_names == z_label)
      if (length(z_idx) > 0) {
        z_idx <- z_idx[1]
        out$z_vals <- data[[z_label]]
        out$weight <- sprintf("%s!%s:%s", wb_sheet, dims_mat[start_row, z_idx], dims_mat[nrow(dims_mat), z_idx])
      }
      out
    },

    # Emits <c:view3D> for surfaceChart and the 3D chart types.
    # Sequence per CT_View3D: rotX, hPercent, rotY, depthPercent, rAngAx,
    # perspective. User values from self$view3d override the per-type defaults.
    render_view3d = function(chart_root) {
      v <- self$view3d

      defaults <- switch(self$type,
        "surfaceChart"   = list(rot_x = 90, rot_y = 0,  right_angle_axes = FALSE, perspective = 0),
        "pie3DChart"     = list(rot_x = 30, rot_y = 0,  right_angle_axes = FALSE, perspective = 30),
        "surface3DChart" = list(rot_x = 15, rot_y = 20, right_angle_axes = FALSE, perspective = 30),
        # bar3DChart, line3DChart, area3DChart
        list(rot_x = 15, rot_y = 20, right_angle_axes = TRUE, perspective = NULL)
      )

      rot_x <- v$rot_x %||% defaults$rot_x
      rot_y <- v$rot_y %||% defaults$rot_y
      r_ang <- v$right_angle_axes %||% defaults$right_angle_axes
      persp <- v$perspective %||% defaults$perspective

      v3d <- xml_add_child(chart_root, "c:view3D")
      xml_add_child(v3d, "c:rotX", val = as.character(rot_x))
      if (!is.null(v$h_percent)) {
        xml_add_child(v3d, "c:hPercent", val = as.character(v$h_percent))
      }
      xml_add_child(v3d, "c:rotY", val = as.character(rot_y))
      if (!is.null(v$depth_percent)) {
        xml_add_child(v3d, "c:depthPercent", val = as.character(v$depth_percent))
      }
      xml_add_child(v3d, "c:rAngAx", val = if (isTRUE(r_ang)) "1" else "0")
      if (!is.null(persp)) {
        xml_add_child(v3d, "c:perspective", val = as.character(persp))
      }
    },

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
      if (identical(style$fill, "none")) {
        xml_add_child(spPr, "a:noFill")
      } else if (!is.null(style$fill)) {
        private$render_color_core(xml_add_child(spPr, "a:solidFill"), style$fill)
      }
      if (!is.null(style$line) && !identical(style$line, "none")) {
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
        any_smooth <- any(vapply(sub_series, function(x) isTRUE(x$smooth), logical(1)))
        xml_add_child(c_node, "c:scatterStyle", val = if (any_smooth) "smoothMarker" else "lineMarker")
      }

      if (type == "ofPieChart") {
        # CT_OfPieChart: ofPieType is the mandatory first element
        xml_add_child(c_node, "c:ofPieType", val = self$of_pie_type %||% "pie")
      }

      if (type %in% c("barChart", "bar3DChart")) {
        xml_add_child(c_node, "c:barDir", val = sub_series[[1]]$dir %||% "col")
        xml_add_child(c_node, "c:grouping", val = sub_series[[1]]$grouping %||% "standard")
      }

      if (type == "radarChart") {
        radar_val <- if (isTRUE(sub_series[[1]]$filled)) "filled" else "standard"
        xml_add_child(c_node, "c:radarStyle", val = radar_val)
      }

      if (type %in% c("surfaceChart", "surface3DChart")) {
        surface_val <- if (isTRUE(sub_series[[1]]$filled)) "1" else "0"
        xml_add_child(c_node, "c:wireframe", val = surface_val)
      }

      if (!type %in% c("scatterChart", "pieChart", "doughnutChart", "bubbleChart", "barChart", "radarChart", "stockChart", "surfaceChart",
                       "bar3DChart", "pie3DChart", "ofPieChart", "surface3DChart")) {
        # lineChart, areaChart, line3DChart, area3DChart
        xml_add_child(c_node, "c:grouping", val = sub_series[[1]]$grouping %||% "standard")
      }

      if (!type %in% c("stockChart", "surfaceChart", "surface3DChart")) {
        vary_val <- if (type %in% ENCHARTER_PIE_FAMILY) "1" else "0"
        xml_add_child(c_node, "c:varyColors", val = vary_val)
      }

      # 2. THE SERIES LOOP
      for (s in sub_series) {
        # `s$data <- NULL` drops the element and `s$data` would then partially
        # match `data_cache`; keep the names present.
        for (nm in c("name", "data", "label", "weight")) {
          if (!nm %in% names(s)) s[nm] <- list(NULL)
        }

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
            if (!is.null(s$name_cache)) private$render_str_cache(strRef, s$name_cache)
          } else {
            # It's a literal string
            xml_add_child(tx, "c:v", as.character(s$name))
          }
        }

        # spPr (Series Styling)
        if (!type %in% ENCHARTER_PIE_FAMILY) {
          sp <- xml_add_child(ser, "c:spPr")
          if (type %in% c("barChart", "areaChart", "bubbleChart", "bar3DChart", "area3DChart")) {
            color <- s$line$color %||% s$color %||% "auto"
            private$render_color_core(xml_add_child(sp, "a:solidFill"), color)
            if (is.list(s$border)) private$render_line_style(sp, s$border)
          } else if (type %in% c("lineChart", "scatterChart", "stockChart", "line3DChart")) {
            # If show_line is FALSE, we must explicitly tell OOXML not to draw the line
            if (isFALSE(s$line$show)) {
              ln <- xml_add_child(sp, "a:ln")
              xml_add_child(ln, "a:noFill")
            } else {
              private$render_line_style(sp, s$line)
            }
          }
        }

        # CT_BarSer: invertIfNegative follows spPr. Written for every bar
        # series: without the element Excel inverts negative bars
        if (type %in% c("barChart", "bar3DChart")) {
          xml_add_child(ser, "c:invertIfNegative", val = if (isTRUE(s$invert_if_negative)) "1" else "0")
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

        if (type %in% ENCHARTER_PIE_FAMILY) {
          if (!is.null(self$expansion)) {
            xml_add_child(ser, "c:explosion", val = as.character(self$expansion))
          }
        }

        # 4. dPt (Data Points)
        if (type %in% c("bubbleChart", ENCHARTER_PIE_FAMILY)) {
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
          # per-point formatting (c:dPt): fill colour, or "none" for an
          # invisible point
          for (pt in s$points) {
            dPt <- xml_add_child(ser, "c:dPt")
            xml_add_child(dPt, "c:idx", val = as.character(pt$idx))
            if (type %in% c("barChart", "bar3DChart")) {
              xml_add_child(dPt, "c:invertIfNegative", val = if (isTRUE(s$invert_if_negative)) "1" else "0")
            }
            xml_add_child(dPt, "c:bubble3D", val = "0")
            if (!is.null(pt$marker)) {
              mk <- xml_add_child(dPt, "c:marker")
              if (!is.null(pt$marker$symbol)) xml_add_child(mk, "c:symbol", val = pt$marker$symbol)
              if (!is.null(pt$marker$size)) xml_add_child(mk, "c:size", val = as.character(pt$marker$size))
              if (!is.null(pt$marker$fill)) {
                private$render_color_core(xml_add_child(xml_add_child(mk, "c:spPr"), "a:solidFill"), pt$marker$fill)
              }
            }
            if (!is.null(pt$color) || !is.null(pt$border)) {
              spPr <- xml_add_child(dPt, "c:spPr")
              if (identical(pt$color, "none")) {
                xml_add_child(spPr, "a:noFill")
              } else if (!is.null(pt$color)) {
                private$render_color_core(xml_add_child(spPr, "a:solidFill"), pt$color)
              }
              if (identical(pt$border, "none")) {
                xml_add_child(xml_add_child(spPr, "a:ln"), "a:noFill")
              } else if (is.list(pt$border)) {
                private$render_line_style(spPr, pt$border)
              }
            }
          }
        }

        # 5. dLbls (Data Labels)
        lp <- s$label_params %||% self$label_params

        # Only enter if lp exists AND at least one show flag is TRUE, or
        # single points carry labels of their own
        if (!is.null(lp) && (isTRUE(lp$show_val) || isTRUE(lp$show_cat) || isTRUE(lp$show_legend_key) ||
                             isTRUE(lp$show_ser_name) || isTRUE(lp$show_percent) || isTRUE(lp$show_bubble_size) ||
                             length(s$point_labels) > 0)) {

          dLbls <- xml_add_child(ser, "c:dLbls")

          # per-point labels (c:dLbl) come first: a deleted label, or the
          # settings of that one point
          for (pl in s$point_labels) {
            dLbl <- xml_add_child(dLbls, "c:dLbl")
            xml_add_child(dLbl, "c:idx", val = as.character(pl$idx))
            if (isTRUE(pl$delete)) {
              xml_add_child(dLbl, "c:delete", val = "1")
              next
            }
            if (!is.null(pl$dx) || !is.null(pl$dy)) {
              ml <- xml_add_child(xml_add_child(dLbl, "c:layout"), "c:manualLayout")
              xml_add_child(ml, "c:x", val = as.character(pl$dx %||% 0))
              xml_add_child(ml, "c:y", val = as.character(pl$dy %||% 0))
            }
            if (!is.null(pl$format)) xml_add_child(dLbl, "c:numFmt", formatCode = pl$format, sourceLinked = "0")
            if (!is.null(pl$fill)) private$render_color_core(xml_add_child(xml_add_child(dLbl, "c:spPr"), "a:solidFill"), pl$fill)
            if (length(pl$style) > 0) private$apply_text_style(dLbl, pl$style)
            if (!is.null(pl$pos) && !type %in% ENCHARTER_3D) {
              pt_pos <- pl$pos
              if (type == "barChart") {
                if (pt_pos == "t") pt_pos <- "outEnd" else if (pt_pos == "b") pt_pos <- "inBase"
              }
              xml_add_child(dLbl, "c:dLblPos", val = pt_pos)
            }
            xml_add_child(dLbl, "c:showLegendKey",  val = if (isTRUE(pl$show_legend_key)) "1" else "0")
            xml_add_child(dLbl, "c:showVal",        val = if (isTRUE(pl$show_val)) "1" else "0")
            xml_add_child(dLbl, "c:showCatName",    val = if (isTRUE(pl$show_cat)) "1" else "0")
            xml_add_child(dLbl, "c:showSerName",    val = if (isTRUE(pl$show_ser_name)) "1" else "0")
            xml_add_child(dLbl, "c:showPercent",    val = if (isTRUE(pl$show_percent)) "1" else "0")
            xml_add_child(dLbl, "c:showBubbleSize", val = if (isTRUE(pl$show_bubble_size)) "1" else "0")
            if (!is.null(pl$sep)) xml_add_child(dLbl, "c:separator", pl$sep)
          }

          # A. numFmt (must precede spPr/txPr per EG_DLblShared)
          if (!is.null(lp$format)) {
            xml_add_child(dLbls, "c:numFmt", formatCode = lp$format, sourceLinked = "0")
          }

          # B. spPr (label background) and txPr (Styling)
          if (!is.null(lp$fill)) {
            private$render_color_core(xml_add_child(xml_add_child(dLbls, "c:spPr"), "a:solidFill"), lp$fill)
          }
          if (length(lp$style) > 0) {
            private$apply_text_style(dLbls, lp$style)
          }

          # C. dLblPos (not allowed on 3D chart groups)
          final_pos <- lp$pos
          if (type == "barChart") {
            if (final_pos == "t")      final_pos <- "outEnd"
            else if (final_pos == "b") final_pos <- "inBase"
          } else if (type %in% c("pieChart", "doughnutChart", "ofPieChart")) {
            final_pos <- "bestFit"
          }
          if (type %in% ENCHARTER_3D) final_pos <- NULL

          if (!is.null(final_pos)) {
            xml_add_child(dLbls, "c:dLblPos", val = final_pos)
          }

          # D. show flags
          xml_add_child(dLbls, "c:showLegendKey",  val = if (isTRUE(lp$show_legend_key)) "1" else "0")
          xml_add_child(dLbls, "c:showVal",        val = if (isTRUE(lp$show_val)) "1" else "0")
          xml_add_child(dLbls, "c:showCatName",    val = if (isTRUE(lp$show_cat)) "1" else "0")
          xml_add_child(dLbls, "c:showSerName",    val = if (isTRUE(lp$show_ser_name)) "1" else "0")
          xml_add_child(dLbls, "c:showPercent",    val = if (isTRUE(lp$show_percent)) "1" else "0")
          xml_add_child(dLbls, "c:showBubbleSize", val = if (isTRUE(lp$show_bubble_size)) "1" else "0")
          if (!is.null(lp$sep)) xml_add_child(dLbls, "c:separator", lp$sep)
          if (!is.null(lp$leader_lines)) {
            # the c15 extension is what decides for chart types other than pie
            val <- if (isTRUE(lp$leader_lines)) "1" else "0"
            xml_add_child(dLbls, "c:showLeaderLines", val = val)
            ext <- xml_add_child(xml_add_child(dLbls, "c:extLst"), "c:ext",
              uri = "{CE6537A1-D6FC-4f65-9D91-7224C49458BB}",
              `xmlns:c15` = "http://schemas.microsoft.com/office/drawing/2012/chart")
            xml_add_child(ext, "c15:showLeaderLines", val = val)
          }
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

          # 5. forward / backward extrapolation and fixed intercept
          if (!is.null(s$trendline$forward)) xml_add_child(tl, "c:forward", val = as.character(s$trendline$forward))
          if (!is.null(s$trendline$backward)) xml_add_child(tl, "c:backward", val = as.character(s$trendline$backward))
          if (!is.null(s$trendline$intercept)) xml_add_child(tl, "c:intercept", val = as.character(s$trendline$intercept))

          # 6. dispRSqr (R-Squared) - Must come AFTER trendlineType
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

          # Required: direction axis (x for horizontal bars on scatter) and types
          xml_add_child(eb, "c:errDir", val = s$error_bars$axis %||% "y")
          xml_add_child(eb, "c:errBarType", val = s$error_bars$direction %||% "both")
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

        # 6. Data References (xVal/yVal or label/val). A series without a
        # reference but with cached values is written as a literal.
        if (is.null(s$data) && !is.null(s$data_cache)) {
          if (!is.null(s$cat_cache)) {
            cat_node <- xml_add_child(ser, if (type %in% c("scatterChart", "bubbleChart")) "c:xVal" else "c:cat")
            if (is.character(s$cat_cache) || is.factor(s$cat_cache)) {
              private$render_str_cache(xml_add_child(cat_node, "c:strLit"), s$cat_cache, lit = TRUE)
            } else {
              private$render_num_cache(xml_add_child(cat_node, "c:numLit"), s$cat_cache, lit = TRUE)
            }
          }
          val_node <- xml_add_child(ser, if (type %in% c("scatterChart", "bubbleChart")) "c:yVal" else "c:val")
          private$render_num_cache(xml_add_child(val_node, "c:numLit"), s$data_cache, lit = TRUE)
          if (type == "bubbleChart") {
            z_node <- xml_add_child(ser, "c:bubbleSize")
            private$render_num_cache(xml_add_child(z_node, "c:numLit"), s$z_cache %||% s$data_cache, lit = TRUE)
          }
        } else if (type %in% c("scatterChart", "bubbleChart")) {
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
          } else if (!is.null(s$cat_cache)) {
            # literal x values next to referenced y values
            private$render_num_cache(xml_add_child(xml_add_child(ser, "c:xVal"), "c:numLit"), s$cat_cache, lit = TRUE)
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
          } else if (!is.null(s$cat_cache)) {
            # literal categories next to referenced values
            cat_node <- xml_add_child(ser, "c:cat")
            if (is.character(s$cat_cache) || is.factor(s$cat_cache)) {
              private$render_str_cache(xml_add_child(cat_node, "c:strLit"), s$cat_cache, lit = TRUE)
            } else {
              private$render_num_cache(xml_add_child(cat_node, "c:numLit"), s$cat_cache, lit = TRUE)
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
        if (!is.null(self$size_represents)) {
          xml_add_child(c_node, "c:sizeRepresents", val = self$size_represents)
        }
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

      # CT_Bar3DChart: gapWidth?, gapDepth?, shape? (no overlap)
      if (type == "bar3DChart") {
        if (!is.null(sub_series[[1]]$gap_width)) {
          xml_add_child(c_node, "c:gapWidth", val = as.character(sub_series[[1]]$gap_width))
        }
        if (!is.null(self$gap_depth)) {
          xml_add_child(c_node, "c:gapDepth", val = as.character(self$gap_depth))
        }
        if (!is.null(self$bar_shape)) {
          xml_add_child(c_node, "c:shape", val = self$bar_shape)
        }
      }

      if (type %in% c("line3DChart", "area3DChart") && !is.null(self$gap_depth)) {
        xml_add_child(c_node, "c:gapDepth", val = as.character(self$gap_depth))
      }

      # CT_OfPieChart: gapWidth?, splitType?, splitPos?, custSplit?,
      # secondPieSize?, serLines*
      if (type == "ofPieChart") {
        gw <- sub_series[[1]]$gap_width %||% 100
        xml_add_child(c_node, "c:gapWidth", val = as.character(gw))
        if (!is.null(self$split_type)) {
          xml_add_child(c_node, "c:splitType", val = self$split_type)
          if (identical(self$split_type, "cust")) {
            if (!is.null(self$split_pos)) {
              cs <- xml_add_child(c_node, "c:custSplit")
              for (idx in self$split_pos) {
                xml_add_child(cs, "c:secondPiePt", val = as.character(idx))
              }
            }
          } else if (!is.null(self$split_pos)) {
            xml_add_child(c_node, "c:splitPos", val = as.character(self$split_pos))
          }
        }
        xml_add_child(c_node, "c:secondPieSize", val = as.character(self$second_pie_size %||% 75))
        xml_add_child(c_node, "c:serLines")
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
      if (type %in% c("bubbleChart", "lineChart", "areaChart", "barChart", "scatterChart", "radarChart", "stockChart", "surfaceChart",
                      "bar3DChart", "line3DChart", "area3DChart", "surface3DChart")) {
        xml_add_child(c_node, "c:axId", val = as.character(cat_id))
        xml_add_child(c_node, "c:axId", val = as.character(val_id))
        if (type %in% c("surfaceChart", "bar3DChart", "line3DChart", "area3DChart", "surface3DChart")) {
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
      xml_add_child(scaling, "c:orientation", val = ifelse(isTRUE(params$rev), "maxMin", "minMax"))
      if (!is.null(params$max)) xml_add_child(scaling, "c:max", val = format(params$max, scientific = FALSE, digits = 15, trim = TRUE))
      if (!is.null(params$min)) xml_add_child(scaling, "c:min", val = format(params$min, scientific = FALSE, digits = 15, trim = TRUE))
      if (!is.null(params$log_base)) xml_add_child(scaling, "c:logBase", val = as.character(params$log_base))

      # 2. Basic Properties
      if (isTRUE(params$delete)) delete <- "1"
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
      if (identical(params$color, "none")) {
        xml_add_child(ln, "a:noFill")
      } else {
        if (!is.null(params$line_width)) xml_set_attr(ln, "w", as.character(round(params$line_width * 12700)))
        private$render_color_core(xml_add_child(ln, "a:solidFill"), params$color %||% "000000")
      }

      label_style <- params
      label_style$color <- params$font_color %||% (if (identical(params$color, "none")) "000000" else params$color) %||% "000000"
      private$apply_text_style(ax, label_style)
      # 7. Crossing (EG_AxShared)
      xml_add_child(ax, "c:crossAx", val = cross_id)

      if (!is.null(params$crosses_at)) {
        # Use a specific value (e.g., cross at Y=100)
        xml_add_child(ax, "c:crossesAt", val = format(params$crosses_at, scientific = FALSE, digits = 15, trim = TRUE))
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
        xml_add_child(ax, "c:lblOffset", val = as.character(params$label_offset %||% 100))

        if (!is.null(params$base_time)) {
          private$validate_input(params$base_time, c("days", "months", "years"), "base_time")
          xml_add_child(ax, "c:baseTimeUnit", val = params$base_time)
        }
        if (!is.null(params$major)) {
          xml_add_child(ax, "c:majorUnit", val = format(params$major, scientific = FALSE, digits = 15, trim = TRUE))
          private$validate_input(params$major_time, c("days", "months", "years"), "major_time")
          if (!is.null(params$major_time)) xml_add_child(ax, "c:majorTimeUnit", val = params$major_time)
        }
        if (!is.null(params$minor)) {
          xml_add_child(ax, "c:minorUnit", val = format(params$minor, scientific = FALSE, digits = 15, trim = TRUE))
          private$validate_input(params$minor_time, c("days", "months", "years"), "minor_time")
          if (!is.null(params$minor_time)) xml_add_child(ax, "c:minorTimeUnit", val = params$minor_time)
        }
      } else {
        # Sequence for CatAx: auto -> lblAlgn -> lblOffset -> skip logic.
        # auto = 0 keeps a text axis for date categories
        xml_add_child(ax, "c:auto", val = if (isFALSE(params$auto)) "0" else "1")
        xml_add_child(ax, "c:lblOffset", val = as.character(params$label_offset %||% 100))
        if (!is.null(params$tick_lbl_skip)) xml_add_child(ax, "c:tickLblSkip", val = as.character(params$tick_lbl_skip))
        if (!is.null(params$tick_mark_skip)) xml_add_child(ax, "c:tickMarkSkip", val = as.character(params$tick_mark_skip))
        xml_add_child(ax, "c:noMultiLvlLbl", val = "0")
      }
    },

    render_val_ax = function(parent, id, cross_id, pos, title_obj = NULL, delete = "0", crosses = "autoZero", params = NULL) {
      ax <- xml_add_child(parent, "c:valAx")

      # 1. Identity and Scaling
      xml_add_child(ax, "c:axId", val = id)
      scaling <- xml_add_child(ax, "c:scaling")
      xml_add_child(scaling, "c:orientation", val = ifelse(isTRUE(params$rev), "maxMin", "minMax"))
      if (!is.null(params$max)) xml_add_child(scaling, "c:max", val = format(params$max, scientific = FALSE, digits = 15, trim = TRUE))
      if (!is.null(params$min)) xml_add_child(scaling, "c:min", val = format(params$min, scientific = FALSE, digits = 15, trim = TRUE))
      if (!is.null(params$log_base)) xml_add_child(scaling, "c:logBase", val = as.character(params$log_base))

      # 2. Delete and Position
      if (isTRUE(params$delete)) delete <- "1"
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
      if (identical(params$color, "none")) {
        xml_add_child(xml_add_child(xml_add_child(ax, "c:spPr"), "a:ln"), "a:noFill")
      } else {
        ax_style <- list(color = params$color %||% "000000", width = params$line_width)
        private$render_line_style(xml_add_child(ax, "c:spPr"), ax_style)
      }

      label_style <- params
      label_style$color <- params$font_color %||% (if (identical(params$color, "none")) "000000" else params$color) %||% "000000"
      private$apply_text_style(ax, label_style)

      # 7. Crossing Properties (End of EG_AxShared)
      xml_add_child(ax, "c:crossAx", val = cross_id)
      cross_val <- params$crosses %||% crosses
      if (!is.null(params$crosses_at)) {
        # If a specific value is provided, it overrides the 'crosses' string
        xml_add_child(ax, "c:crossesAt", val = format(params$crosses_at, scientific = FALSE, digits = 15, trim = TRUE))
      } else {
        xml_add_child(ax, "c:crosses", val = cross_val)
      }
      cb_val <- params$cross_between %||% "between"
      xml_add_child(ax, "c:crossBetween", val = cb_val)

      # 8. Units (End of ValAx)
      if (!is.null(params$major)) xml_add_child(ax, "c:majorUnit", val = format(params$major, scientific = FALSE, digits = 15, trim = TRUE))
      if (!is.null(params$minor)) xml_add_child(ax, "c:minorUnit", val = format(params$minor, scientific = FALSE, digits = 15, trim = TRUE))
      if (!is.null(params$disp_units)) {
        du <- xml_add_child(ax, "c:dispUnits")
        if (is.numeric(params$disp_units)) {
          xml_add_child(du, "c:custUnit", val = format(params$disp_units, scientific = FALSE, digits = 15, trim = TRUE))
        } else {
          xml_add_child(du, "c:builtInUnit", val = params$disp_units)
        }
      }
    },

    render_ser_ax = function(parent, id, cross_id) {
      ax <- xml_add_child(parent, "c:serAx")
      xml_add_child(ax, "c:axId", val = as.character(id))

      scaling <- xml_add_child(ax, "c:scaling")
      xml_add_child(scaling, "c:orientation", val = "minMax") # needs rev?

      xml_add_child(ax, "c:delete", val = "0")
      xml_add_child(ax, "c:axPos", val = "b")
      xml_add_child(ax, "c:tickLblPos", val = "nextTo")
      xml_add_child(ax, "c:crossAx", val = as.character(cross_id))
      xml_add_child(ax, "c:crosses", val = "autoZero")
    },

    # Emit a c:numCache block into ref_node.
    # Date/POSIXt values are converted to OOXML serials via convert_to_excel_date.
    # Plain numeric values are written as-is.
    render_num_cache = function(ref_node, vals, lit = FALSE) {
      cache <- if (lit) ref_node else xml_add_child(ref_node, "c:numCache")
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
          xml_add_child(pt, "c:v", format(vals[[i]], scientific = FALSE, digits = 15, trim = TRUE))
        }
      }
    },

    # Emit a c:strCache block into ref_node for character/factor categories.
    render_str_cache = function(ref_node, vals, lit = FALSE) {
      cache <- if (lit) ref_node else xml_add_child(ref_node, "c:strCache")
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
      body_attrs <- if (is.null(s$body_pr)) list(lIns = "0", tIns = "0", rIns = "0", bIns = "0", wrap = "square") else s$body_pr
      bodyPr <- do.call(xml_add_child, c(list(txPr, "a:bodyPr"), body_attrs))
      if (!is.null(s$rotation)) {
        # rotation = degrees * 60000
        xml_set_attr(bodyPr, "rot", as.character(round(s$rotation * 60000)))
        xml_set_attr(bodyPr, "vert", "horz")
      }
      if (isTRUE(s$auto_fit)) xml_add_child(bodyPr, "a:spAutoFit")

      # 2. Add required list style
      xml_add_child(txPr, "a:lstStyle")

      # 3. Build the text run properties (defRPr)
      p      <- xml_add_child(txPr, "a:p")
      pPr    <- xml_add_child(p, "a:pPr")
      if (!is.null(s$align)) xml_set_attr(pPr, "algn", s$align)
      defRPr <- xml_add_child(pPr, "a:defRPr")

      # font size in 1/100 pt; without one the text takes the chart default
      if (!is.null(s$font_size)) xml_set_attr(defRPr, "sz", as.character(s$font_size * 100))

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
