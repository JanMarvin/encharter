# The 'Super' routing vectors
ENCHARTER_STANDARD <- c(
  "barChart", "lineChart", "areaChart", "scatterChart",
  "pieChart", "doughnutChart", "radarChart", "bubbleChart", "surfaceChart"
)

ENCHARTER_EXTENDED <- c(
  "waterfall", "sunburst", "treemap", "regionMap", "clusteredColumn", "funnel",
  "paretoLine", "boxWhisker"
)

#' Create an Encharter Chart (The Factory)
#' @param type A character string specifying the chart type
#' @export
encharter <- function(type = "lineChart") {

  match.arg(as.character(type), choices = c(ENCHARTER_STANDARD, ENCHARTER_EXTENDED))

  if (type %in% ENCHARTER_EXTENDED) {
    # Returns the ChartEx child
    ec <- ChartEx$new(type = type)
  }

  if (type %in% ENCHARTER_STANDARD) {
    # Returns the Chart child
    ec <- Chart$new(type = type)
  }

  ec
}

#' Alias for encharter()
#' @rdname encharter
#' @export
ec <- encharter

#' Encharter Base R6 Class
#' @import R6
#' @importFrom xml2 read_xml xml_remove xml_add_child xml_find_first xml_find_all xml_set_attr
#' @importFrom openxlsx2 wb_color dims_to_dataframe
EncharterBase <- R6::R6Class(
  "EncharterBase",
  public = list(
    #' @field xml The raw xml2 object containing the chart space.
    xml = NULL,
    #' @field series_data A list containing all added data series and their styles.
    series_data = list(),
    #' @field type The default chart type for the object (e.g., "lineChart").
    type = NULL,
    #' @field palette A vector of hex colors to use for series.
    palette = c("4472C4", "ED7D31", "A5A5A5", "FFC000", "5B9BD5", "70AD47"),

    # Standardized list structure for titles
    #' @field chart_title List containing text and style for the main title.
    chart_title = list(text = NULL, style = list()),
    #' @field x_title List containing text and style for the X-axis.
    x_title  = list(text = NULL, style = list()),
    #' @field y_title List containing text and style for the primary Y-axis.
    y_title  = list(text = NULL, style = list()),

    #' @field chart_style List for the outer chart area styling.
    chart_style = list(fill = "FFFFFF", line = NULL, line_width = 1),
    #' @field plot_style List for the inner plot area styling.
    plot_style  = list(fill = NULL, line = NULL, line_width = 1),

    #' @field label_params List of global data label configuration settings.
    label_params  = list(show_val = FALSE, show_cat = FALSE, show_legend_key = FALSE, pos = "t", style = list()),
    #' @field legend_params List of legend configuration settings.
    legend_params = list(pos = "r", overlay = "0", style = list()),

    #' @field axis_params Internal list for scaling, units, and formatting.
    axis_params = list(
      x  = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", name = NULL, sz = NULL, bold = NULL, italic = NULL, label_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      x2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", name = NULL, sz = NULL, bold = NULL, italic = NULL, label_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      y  = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", name = NULL, sz = NULL, bold = NULL, italic = NULL, label_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = TRUE,  minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      y2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", name = NULL, sz = NULL, bold = NULL, italic = NULL, label_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo")
    ),

    #' @description Set the chart's main title.
    #' @param text Title text string.
    #' @param ... Style arguments like `sz` (font size), `bold` (TRUE/FALSE), `color`, and `name` (font name).
    set_chart_title = function(text, ...) {
      self$chart_title <- list(text = text, style = list(...))
      invisible(self)
    },

    #' @description Set the X-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_x_title      = function(text, ...) {
      self$x_title      <- list(text = text, style = list(...))
      invisible(self)
    },

    #' @description Set the primary Y-axis title.
    #' @param text Title text string.
    #' @param ... Style arguments.
    set_y_title      = function(text, ...) {
      self$y_title      <- list(text = text, style = list(...))
      invisible(self)
    },

    #' @description Set Primary X-axis scaling, units, and format.
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
    #' @param color,label_color Hex color for the axis lines and label (or independent label color).
    #' @param sz Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param name Font typeface name (e.g., "Arial", "Calibri").
    #' @param rot Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the gridlines.
    #' @param gridlines,minor_gridlines Logical. Show or hide gridlines.
    #' @param line_width,grid_width,minor_grid_width Numeric. Change the width of the axis and gridlines.
    #' @param cross_between Specifies how the value axis crosses the category axis ('between' or 'midCat').
    #' @param crosses Intersection: "autoZero" (default), "min" (start), or "max" (end).
    #' @param crosses_at Numeric axis value for intersection. Overrides 'crosses'.
    #' @param label_pos Label position: "nextTo" (default), "low" (edge of chart), "high" (opposite edge), or "none".
    set_x_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                          major_time = NULL, minor_time = NULL, base_time = NULL,
                          major_tick = NULL, minor_tick = NULL,
                          format = NULL, log_base = NULL, color = NULL,
                          name = NULL, sz = NULL, bold = NULL, italic = NULL,
                          label_color = NULL, rot = NULL,
                          grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL, cross_between = NULL,
                          line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                          crosses = NULL, crosses_at = NULL, label_pos = NULL) {

      crosses   <- private$validate_input(crosses, c("min", "min", "autoZero"), "crosses")
      label_pos <- private$validate_input(label_pos, c("nextTo", "high", "low", "none"), "label_pos")
      major_tick <- private$validate_input(major_tick, c("cross", "in", "out", "none"), "major_tick")
      minor_tick <- private$validate_input(minor_tick, c("cross", "in", "out", "none"), "minor_tick")
      if (is.character(gridlines)) {
        private$validate_input(
          gridlines,
          c("solid", "dash", "dot", "dashDot", "lgDash", "lgDashDot", "sysDash", "sysDot", "dashed", "dotted"),
          "gridlines"
        )
      }
      if (is.character(minor_gridlines)) {
        private$validate_input(
          minor_gridlines,
          c("solid", "dash", "dot", "dashDot", "lgDash", "lgDashDot", "sysDash", "sysDot", "dashed", "dotted"),
          "minor_gridlines"
        )
      }

      params <- list(min = min, max = max, major = major, minor = minor,
                     major_time = major_time, minor_time = minor_time, base_time = base_time,
                     major_tick = major_tick, minor_tick = minor_tick,
                     format = format, log_base = log_base, color = color,
                     name = name, sz = sz, bold = bold, italic = italic,
                     label_color = label_color, rot = rot,
                     grid_color = grid_color, gridlines = gridlines, minor_grid_color = minor_grid_color,
                     minor_gridlines = minor_gridlines, cross_between = cross_between,
                     line_width = line_width, grid_width = grid_width, minor_grid_width = minor_grid_width,
                     crosses = crosses, crosses_at = crosses_at, label_pos = label_pos)
      self$axis_params$x <- modifyList(self$axis_params$x, Filter(Negate(is.null), params))
      invisible(self)
    },

    #' @description Set Primary Y-axis scaling, units, and format.
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
    #' @param color,label_color Hex color for the axis lines and label (or independent label color).
    #' @param sz Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param name Font typeface name (e.g., "Arial", "Calibri").
    #' @param rot Rotation in degrees.
    #' @param grid_color,minor_grid_color Hex color for the gridlines.
    #' @param gridlines,minor_gridlines Logical. Show or hide gridlines.
    #' @param line_width,grid_width,minor_grid_width Numeric. Change the width of the axis and gridlines.
    #' @param cross_between Specifies how the value axis crosses the category axis ('between' or 'midCat').
    #' @param crosses Intersection: "autoZero" (default), "min" (start), or "max" (end).
    #' @param crosses_at Numeric axis value for intersection. Overrides 'crosses'.
    #' @param label_pos Label position: "nextTo" (default), "low" (edge of chart), "high" (opposite edge), or "none".
    set_y_axis = function(min = NULL, max = NULL, major = NULL, minor = NULL,
                          major_time = NULL, minor_time = NULL, base_time = NULL,
                          major_tick = NULL, minor_tick = NULL,
                          format = NULL, log_base = NULL, color = NULL,
                          name = NULL, sz = NULL, bold = NULL, italic = NULL,
                          label_color = NULL, rot = NULL,
                          grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL, cross_between = NULL,
                          line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                          crosses = NULL, crosses_at = NULL, label_pos = NULL) {

      crosses   <- private$validate_input(crosses, c("autoZero", "min", "max"), "crosses")
      label_pos <- private$validate_input(label_pos, c("nextTo", "high", "low", "none"), "label_pos")
      major_tick <- private$validate_input(major_tick, c("cross", "in", "out", "none"), "major_tick")
      minor_tick <- private$validate_input(minor_tick, c("cross", "in", "out", "none"), "minor_tick")
      if (is.character(gridlines)) {
        private$validate_input(
          gridlines,
          c("solid", "dash", "dot", "dashDot", "lgDash", "lgDashDot", "sysDash", "sysDot", "dashed", "dotted"),
          "gridlines"
        )
      }
      if (is.character(minor_gridlines)) {
        private$validate_input(
          minor_gridlines,
          c("solid", "dash", "dot", "dashDot", "lgDash", "lgDashDot", "sysDash", "sysDot", "dashed", "dotted"),
          "minor_gridlines"
        )
      }

      params <- list(min = min, max = max, major = major, minor = minor,
                     major_time = major_time, minor_time = minor_time, base_time = base_time,
                     major_tick = major_tick, minor_tick = minor_tick,
                     format = format, log_base = log_base, color = color,
                     name = name, sz = sz, bold = bold, italic = italic,
                     label_color = label_color, rot = rot,
                     grid_color = grid_color, gridlines = gridlines, minor_grid_color = minor_grid_color,
                     minor_gridlines = minor_gridlines, cross_between = cross_between,
                     line_width = line_width, grid_width = grid_width, minor_grid_width = minor_grid_width,
                     crosses = crosses, crosses_at = crosses_at, label_pos = label_pos)
      self$axis_params$y <- modifyList(self$axis_params$y, Filter(Negate(is.null), params))
      invisible(self)
    },

    #' @description Configure global data label settings.
    #' @param show_val Logical. Show numeric values.
    #' @param show_cat Logical. Show category names.
    #' @param show_legend_key Logical. Show legend key next to label.
    #' @param pos Label position (e.g., 't', 'b', 'ctr', 'l', 'r').
    #' @param ... Font styling for labels (e.g., color, sz, name).
    set_data_label_style = function(show_val = TRUE, show_cat = FALSE, show_legend_key = FALSE, pos = "t", ...) {
      pos <- private$validate_input(pos, c("t", "b", "l", "r", "ctr", "inEnd", "outEnd", "bestFit", "none"), "pos")
      self$label_params <- list(show_val = show_val, show_cat = show_cat, show_legend_key = show_legend_key, pos = pos, style = list(...))
      invisible(self)
    },

    #' @description Set legend properties.
    #' @param pos Position (t, b, l, r, none).
    #' @param align Alignment (ctr, min, max).
    #' @param overlay Logical; overlay legend on chart.
    #' @param sz Size of font.
    #' @param name Name of font.
    #' @param bold Logical.
    #' @param italic Logical.
    #' @param color Hex color.
    set_legend_style = function(pos = "t", align = "ctr", overlay = FALSE, sz = NULL, name = NULL, bold = NULL, italic = NULL, color = NULL) {
      self$legend_params <- list(pos = pos, align = align, overlay = ifelse(overlay, "1", "0"),
                                 style = list(sz = sz, name = name, bold = bold, italic = italic, color = color))
      invisible(self)
    },

    #' @description Style the outer chart background and border.
    #' @param fill Hex color for background.
    #' @param line Hex color for border line.
    #' @param line_width Numeric width of border line.
    set_chart_style = function(fill = "FFFFFF", line = NULL, line_width = 1) {
      self$chart_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    },

    #' @description Style the inner plot area background.
    #' @param fill Hex color for background.
    #' @param line Hex color for border line.
    #' @param line_width Numeric width of border line.
    set_plot_style = function(fill = NULL, line = NULL, line_width = 1) {
      self$plot_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    }
  ),
  private = list(

    validate_input = function(val, choices, arg_name = "Argument") {
      if (is.null(val)) return(choices[1])

      # match.arg works best when choices are provided as a character vector
      res <- try(match.arg(val, choices), silent = TRUE)

      if (inherits(res, "try-error")) {
        stop(sprintf("'%s' must be one of: %s", arg_name, paste(choices, collapse = ", ")), call. = FALSE)
      }
      res
    }
  )
)
