# The 'Super' routing vectors

#' @noRd
ENCHARTER_STANDARD <- c(
  "barChart", "lineChart", "areaChart", "scatterChart",
  "pieChart", "doughnutChart", "radarChart", "bubbleChart", "surfaceChart"
)

#' @noRd
ENCHARTER_EXTENDED <- c(
  "waterfall", "sunburst", "treemap", "regionMap", "clusteredColumn", "funnel",
  "paretoLine", "boxWhisker"
)

#' Create an Encharter Chart
#'
#' @description
#' A factory function to initialize an R6 chart object. It supports both standard
#' OOXML charts (e.g., Bar, Line, Scatter) and modern "Extended" charts
#' (e.g., Waterfall, Treemap).
#'
#' @param type A character string specifying the chart type. Familiar R aliases
#' are supported (see Details).
#'
#' @details
#' \strong{Supported Chart Types:}
#' \itemize{
#'   \item \bold{Bar/Column:} \code{"barChart"}, \code{"barplot"}, \code{"hist"}, \code{"histogram"}
#'   \item \bold{Line/Area:} \code{"lineChart"}, \code{"line"}, \code{"areaChart"}, \code{"area"}
#'   \item \bold{Scatter/Points:} \code{"scatterChart"}, \code{"scatter"}, \code{"point"}
#'   \item \bold{Pie/Doughnut:} \code{"pieChart"}, \code{"pie"}, \code{"doughnutChart"}, \code{"doughnut"}
#'   \item \bold{Extended (ChartEx):} \code{"waterfall"}, \code{"treemap"}, \code{"sunburst"},
#'   \code{"regionMap"}, \code{"boxWhisker"} (or \code{"boxplot"}), \code{"funnel"}
#' }
#'
#' \strong{Direction Handling:}
#' For Bar/Column charts, the orientation is determined by the \code{dir} parameter
#' in \code{add_series()}. You can use \code{"v"}, \code{"vertical"} (Column) or
#' \code{"h"}, \code{"horizontal"} (Bar).
#'
#' @return An R6 object of class \code{Chart} or \code{ChartEx}.
#' @export
encharter <- function(type = "lineChart") {

  type <- normalize_encharter_type(type)
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

#' @rdname encharter
#' @export
ec <- encharter

#' Alias for encharter()
#' @rdname encharter
#' @export
ec <- encharter

#' Encharter Base R6 Class
#' @useDynLib encharter, .registration=TRUE
#'
#' @import R6
#' @importFrom openxlsx2 wb_color dims_to_dataframe read_xml fmt_txt
#  some XML functions from openxlsx2 are used but not imported because of name
#  clashes
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
      x  = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      x2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      y  = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = TRUE,  minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo"),
      y2 = list(min = NULL, max = NULL, major = NULL, minor = NULL, major_time = NULL, minor_time = NULL, base_time = NULL, major_tick = NULL, minor_tick = NULL, format = NULL, log_base = NULL, color = "000000", font_name = NULL, font_size = NULL, bold = NULL, italic = NULL, font_color = "000000", rot = NULL, grid_color = "D9D9D9", gridlines = FALSE, minor_gridlines = FALSE, minor_grid_color = "F2F2F2", cross_between = "between", line_width = 1, grid_width = 1, minor_grid_width = 0.5, crosses = NULL, crosses_at = NULL, label_pos = "nextTo")
    ),

    #' @description Set the chart's main title.
    #' @param text Title text string.
    #' @param font_color Font color for the chart title.
    #' @param font_size Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param font_name Font typeface name (e.g., "Arial", "Calibri").
    #' @param fill Hex color for background.
    #' @param line Hex color for border line.
    #' @param line_width Numeric width of border line.
    set_chart_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL, bold = NULL, italic = NULL, fill = NULL, line = NULL, line_width = NULL) {
      self$chart_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
      invisible(self)
    },

    #' @description Set the X-axis title.
    #' @param text Title text string.
    #' @param font_color Font color for the axis title.
    #' @param font_size Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param font_name Font typeface name (e.g., "Arial", "Calibri").
    #' @param fill Hex color for background.
    #' @param line Hex color for border line.
    #' @param line_width Numeric width of border line.
    set_x_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL, bold = NULL, italic = NULL, fill = NULL, line = NULL, line_width = NULL) {
      self$x_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
      invisible(self)
    },

    #' @description Set the primary Y-axis title.
    #' @param text Title text string.
    #' @param font_color Font color for the axis title.
    #' @param font_size Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param font_name Font typeface name (e.g., "Arial", "Calibri").
    #' @param fill Hex color for background.
    #' @param line Hex color for border line.
    #' @param line_width Numeric width of border line.
    set_y_title = function(text, font_size = NULL, font_name = NULL, font_color = NULL, bold = NULL, italic = NULL, fill = NULL, line = NULL, line_width = NULL) {
      self$y_title <- list(text = text, style = list(font_size = font_size, font_name = font_name, font_color = font_color, bold = bold, italic = italic, fill = fill, line = line, line_width = line_width))
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
    #' @param color,font_color Hex color for the axis lines and label (or independent label color).
    #' @param font_size Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param font_name Font typeface name (e.g., "Arial", "Calibri").
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
                          font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                          font_color = NULL, rot = NULL,
                          grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL, cross_between = NULL,
                          line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                          crosses = NULL, crosses_at = NULL, label_pos = NULL) {
        private$set_axis_params(
          "x",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rot = rot, grid_color = grid_color, gridlines = gridlines,
          minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos
        )
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
    #' @param color,font_color Hex color for the axis lines and label (or independent label color).
    #' @param font_size Font size for the axis labels.
    #' @param bold Logical; if `TRUE`, axis labels will be bold.
    #' @param italic Logical; if `TRUE`, axis labels will be italicized.
    #' @param font_name Font typeface name (e.g., "Arial", "Calibri").
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
                          font_name = NULL, font_size = NULL, bold = NULL, italic = NULL,
                          font_color = NULL, rot = NULL,
                          grid_color = NULL, gridlines = NULL,
                          minor_grid_color = NULL, minor_gridlines = NULL, cross_between = NULL,
                          line_width = NULL, grid_width = NULL, minor_grid_width = NULL,
                          crosses = NULL, crosses_at = NULL, label_pos = NULL) {
        private$set_axis_params(
          "y",
          min = min, max = max, major = major, minor = minor, major_time = major_time,
          minor_time = minor_time, base_time = base_time, major_tick = major_tick,
          minor_tick = minor_tick, format = format, log_base = log_base, color = color,
          font_name = font_name, font_size = font_size, bold = bold, italic = italic,
          font_color = font_color, rot = rot, grid_color = grid_color, gridlines = gridlines,
          minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines,
          cross_between = cross_between, line_width = line_width, grid_width = grid_width,
          minor_grid_width = minor_grid_width, crosses = crosses, crosses_at = crosses_at,
          label_pos = label_pos
        )
    },

    #' @description Configure global data label settings.
    #' @param show_val Logical. Show numeric values.
    #' @param show_cat Logical. Show category names.
    #' @param show_legend_key Logical. Show legend key next to label.
    #' @param pos Label position (e.g., 't', 'b', 'ctr', 'l', 'r').
    #' @param ... Font styling for labels (e.g., color, sz, name).
    set_data_label_style = function(show_val = TRUE, show_cat = FALSE, show_legend_key = FALSE, pos = "t", ...) {
      pos <- normalize_encharter_string(pos)
      pos <- private$validate_input(pos, c("t", "b", "l", "r", "ctr", "inEnd", "outEnd", "bestFit", "none"), "pos")
      self$label_params <- list(show_val = show_val, show_cat = show_cat, show_legend_key = show_legend_key, pos = pos, style = list(...))
      invisible(self)
    },

    #' @description Set legend properties.
    #' @param pos Position (t, b, l, r, none).
    #' @param align Alignment (ctr, min, max).
    #' @param overlay Logical; overlay legend on chart.
    #' @param font_size Size of font.
    #' @param font_name Name of font.
    #' @param bold Logical.
    #' @param italic Logical.
    #' @param color Hex color.
    set_legend_style = function(pos = "t", align = "ctr", overlay = FALSE, font_size = NULL, font_name = NULL, bold = NULL, italic = NULL, color = NULL) {
      pos <- normalize_encharter_string(pos)
      align <- normalize_encharter_string(align)
      self$legend_params <- list(pos = pos, align = align, overlay = ifelse(overlay, "1", "0"),
                                 style = list(font_size = font_size, font_name = font_name, bold = bold, italic = italic, color = color))
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
    },

    #' @description print the summary of the Encharter object
    print = function() {
      nSeries <- length(self$series_data)

      cat("An encharter object\n")
      cat("Number of Series:", nSeries, "\n")

      if (nSeries > 0) {
        cat(rep("-", 30), "\n", sep = "")

        for (i in seq_len(nSeries)) {
          s <- self$series_data[[i]]

          # Logic to determine axis hint
          is_secondary <- s$sec_type %in% c("x", "y", "xy")
          axis_hint <- if (is_secondary) " [Secondary Axis]" else ""

          s_type <- if (!is.null(s$type)) s$type else self$type
          s_name <- if (!is.null(s$name)) s$name else paste("Series", i)

          cat(sprintf("Series %d: %s %s\n", i, s_name, axis_hint))
          cat(sprintf("  - Type: %s\n", s_type))

          if (!is.null(s$data)) {
            cat(sprintf("  - Data: [%s]\n", s$data))
          }

          if (!is.null(s$cat)) {
            cat(sprintf("  - Cat:  [%s]\n", s$cat))
          }

          cat(rep("-", 30), "\n", sep = "")
        }
      }

      invisible(self)
    }
  ),
  private = list(
    set_axis_params = function(which, min, max, major, minor,
                               major_time, minor_time, base_time,
                               major_tick, minor_tick,
                               format, log_base, color,
                               font_name, font_size, bold, italic,
                               font_color, rot,
                               grid_color, gridlines,
                               minor_grid_color, minor_gridlines, cross_between,
                               line_width, grid_width, minor_grid_width,
                               crosses, crosses_at, label_pos) {

      crosses    <- private$validate_input(crosses,    c("autoZero", "min", "max"), "crosses")
      label_pos  <- private$validate_input(label_pos,  c("nextTo", "high", "low", "none"), "label_pos")
      major_tick <- private$validate_input(major_tick, c("cross", "in", "out", "none"), "major_tick")
      minor_tick <- private$validate_input(minor_tick, c("cross", "in", "out", "none"), "minor_tick")

      DASH_TYPES <- c("solid", "dash", "dot", "dashDot", "lgDash",
                      "lgDashDot", "sysDash", "sysDot", "dashed", "dotted")
      if (is.character(gridlines))       private$validate_input(gridlines,       DASH_TYPES, "gridlines")
      if (is.character(minor_gridlines)) private$validate_input(minor_gridlines, DASH_TYPES, "minor_gridlines")

      params <- list(
        min = min, max = max, major = major, minor = minor,
        major_time = major_time, minor_time = minor_time, base_time = base_time,
        major_tick = major_tick, minor_tick = minor_tick,
        format = format, log_base = log_base, color = color,
        font_name = font_name, font_size = font_size, bold = bold, italic = italic,
        font_color = font_color, rot = rot,
        grid_color = grid_color, gridlines = gridlines,
        minor_grid_color = minor_grid_color, minor_gridlines = minor_gridlines,
        cross_between = cross_between,
        line_width = line_width, grid_width = grid_width, minor_grid_width = minor_grid_width,
        crosses = crosses, crosses_at = crosses_at, label_pos = label_pos
      )

      self$axis_params[[which]] <- modifyList(
        self$axis_params[[which]],
        Filter(Negate(is.null), params)
      )
      invisible(self)
    },

    render_color_core = function(target_node, color_val, wrap = FALSE) {
      # Guard: treat NULL and zero-length as no-op
      if (is.null(color_val) || length(color_val) == 0) return()

      # Set the destination node based on the wrap argument
      node <- if (wrap) xml_add_child(target_node, "a:solidFill") else target_node

      # 1. Check for "auto"
      if (length(color_val) == 1 && tolower(as.character(color_val)) == "auto") {
        xml_add_child(node, "a:schemeClr", val = "accent1")
        return()
      }

      type <- names(color_val)

      # 2. Handle wb_color objects (Hex vs Theme)
      if (inherits(color_val, "wbColour")) {
        if (!is.null(type) && type == "auto") {
          xml_add_child(node, "a:schemeClr", val = "accent1")
          return()
        }

        if (!is.null(type) && type == "theme") {
          theme_map <- c(
            "bg1", "tx1", "bg2", "tx2",
            "accent1", "accent2", "accent3", "accent4", "accent5", "accent6",
            "hlink", "folHlink", "phClr",
            "dk1", "lt1", "dk2", "lt2"
          )
          if (as.character(color_val) %in% theme_map) {
            val_name <- as.character(color_val)
          } else {
            theme_idx <- as.integer(color_val)
            val_name <- theme_map[as.numeric(theme_idx) + 1]
          }
          xml_add_child(node, "a:schemeClr", val = val_name)
          return()
        }

        hex <- if (!is.null(type) && type == "rgb") as.character(color_val) else as.character(color_val[1])
      } else {
        hex <- as.character(color_val[1])
      }

      # 3. Clean and add as RGB
      clean <- toupper(gsub("^#", "", hex))

      alpha_val <- NULL
      if (nchar(clean) == 8) {
        aa_hex <- substr(clean, 1, 2)
        aa_dec <- as.numeric(paste0("0x", aa_hex))
        alpha_val <- as.integer(round((aa_dec / 255) * 100000))
        clean <- substr(clean, 3, 8)
      }

      if (nchar(clean) != 6) clean <- "000000"

      color_node <- xml_add_child(node, "a:srgbClr", val = clean)
      if (!is.null(alpha_val)) {
        xml_add_child(color_node, "a:alpha", val = as.character(alpha_val))
      }
    },

    render_color = function(parent_node, color_val) {
      if (is.null(color_val) || identical(color_val, "auto")) return()
      private$render_color_core(
        xml_add_child(parent_node, "a:solidFill"),
        color_val
      )
    },

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
