#' ChartEx R6 Class for Extended Spreadsheet Charts
#'
#' @description
#' An R6 class to create and manipulate Office OpenXML (OOXML) Extended Charts (ChartEx),
#' including Waterfall, Sunburst, Treemap, and Region Maps, which are not
#' supported by standard Office Open XML chart types.
#'
#' @details
#' This class uses `xml2` to manipulate the underlying XML structure and
#' integrates with `openxlsx2` for workbook generation.
#'
#' @rdname encharter
#' @usage NULL
ChartEx <- R6::R6Class(
  "ChartEx",
  public = list(
    #' @field xml The raw XML object for the chart space.
    xml = NULL,
    #' @field series_data A list containing all added series definitions.
    series_data = list(),
    #' @field type The default chart type for the object (e.g., "waterfall").
    type = NULL,
    #' @field chart_title The title text.
    chart_title = NULL,
    #' @field chart_title_style List of styling parameters for the title.
    chart_title_style = list(),
    #' @field x_title The X-axis title text.
    x_title = NULL,
    #' @field x_title_style List of styling parameters for the X-axis title.
    x_title_style = list(),
    #' @field y_title The Y-axis title text.
    y_title = NULL,
    #' @field y_title_style List of styling parameters for the Y-axis title.
    y_title_style = list(),
    #' @field x_axis_style List of styling and axis parameters for the X-axis.
    x_axis_style = list(),
    #' @field y_axis_style List of styling and axis parameters for the Y-axis.
    y_axis_style = list(),
    #' @field x_numfmt Number format for X-axis.
    x_numfmt = NULL,
    #' @field y_numfmt Number format for Y-axis.
    y_numfmt = NULL,
    #' @field legend_params Parameters for legend positioning and style.
    legend_params = list(),
    #' @field data_label_params Parameters for data labels.
    data_label_params = list(),
    #' @field chart_area_style Styling for the outer chart area.
    chart_area_style = list(),
    #' @field plot_area_style Styling for the inner plot area.
    plot_area_style = list(),
    #' @field palette A vector of hex colors to use for series.
    palette = c("4472C4", "ED7D31", "A5A5A5", "FFC000", "5B9BD5", "70AD47"),

    #' @description Create a new ChartEx object.
    #' @return A new `ChartEx` object.
    #' @param type Initial chart type (e.g., "waterfall", "treemap").
    initialize = function(type = NULL) {

      self$type <- type
      self$xml <- xml2::read_xml(
        '<cx:chartSpace xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                        xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                        xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex">
           <cx:chartData/><cx:chart><cx:title pos="t" align="ctr" overlay="0"/><cx:plotArea><cx:plotAreaRegion/>
           <cx:axis id="0"><cx:catScaling gapWidth="0.5"/><cx:tickLabels/></cx:axis>
           <cx:axis id="1"><cx:valScaling/><cx:majorGridlines/><cx:tickLabels/></cx:axis>
           </cx:plotArea><cx:legend pos="t" align="ctr" overlay="0"/></cx:chart></cx:chartSpace>'
      )
      self$legend_params <- list(pos = "t", align = "ctr", overlay = "0", style = list())
      self$data_label_params <- list(show = FALSE)
      self$x_axis_style <- list()
      self$y_axis_style <- list()
    },

    #' @description Set chart area styling.
    #' @param fill Fill color (hex or wbColour).
    #' @param line Line/border color.
    #' @param line_width Width of the border.
    set_chart_style = function(fill = NULL, line = NULL, line_width = 1) {
      self$chart_area_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    },

    #' @description Set plot area styling.
    #' @param fill Fill color (hex or wbColour).
    #' @param line Line/border color.
    #' @param line_width Width of the border.
    set_plot_style = function(fill = NULL, line = NULL, line_width = 1) {
      self$plot_area_style <- list(fill = fill, line = line, line_width = line_width)
      invisible(self)
    },

    #' @description Set the chart title and style.
    #' @param text Title text.
    #' @param ... Styling (sz, b, color, font).
    set_chart_title = function(text, ...) {
      self$chart_title <- text
      self$chart_title_style <- list(...)
      invisible(self)
    },

    #' @description Set the X-axis title and style.
    #' @param text Title text.
    #' @param ... Styling (sz, b, color, font).
    set_x_title = function(text, ...) {
      self$x_title <- text
      self$x_title_style <- list(...)
      invisible(self)
    },

    #' @description Set the Y-axis title and style.
    #' @param text Title text.
    #' @param ... Styling (sz, b, color, font).
    set_y_title = function(text, ...) {
      self$y_title <- text
      self$y_title_style <- list(...)
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
                                 style = list(sz = sz, font = name, b = bold, i = italic, color = color))
      invisible(self)
    },

    #' @description Set data label properties.
    #' @param show Logical.
    #' @param pos Position (outEnd, inEnd, ctr, etc).
    #' @param sz Font size.
    #' @param name Font name.
    #' @param bold Logical.
    #' @param italic Logical.
    #' @param color Hex color.
    #' @param numfmt A number format string.
    set_data_label_style = function(show = TRUE, pos = "outEnd", sz = NULL, name = NULL, bold = NULL, italic = NULL, color = NULL, numfmt = NULL) {
      self$data_label_params <- list(show = show, pos = pos,
                                     style = list(sz = sz, font = name, b = bold, i = italic, color = color),
                                     numfmt = numfmt)
      invisible(self)
    },

    #' @description Set X-axis tick label style, number format, and parameters.
    #' @param numfmt A number format.
    #' @param ... Styling parameters (sz, color, b, font) and axis parameters (gap_width, min, max, grid_color).
    set_x_axis = function(numfmt = NULL, ...) {
      if (!is.null(numfmt)) self$x_numfmt <- numfmt
      self$x_axis_style <- list(...)
      invisible(self)
    },

    #' @description Set Y-axis tick label style, number format, and parameters.
    #' @param numfmt A number format.
    #' @param ... Styling parameters (sz, color, b, font) and axis parameters (gap_width, min, max, grid_color).
    set_y_axis = function(numfmt = NULL, ...) {
      if (!is.null(numfmt)) self$y_numfmt <- numfmt
      self$y_axis_style <- list(...)
      invisible(self)
    },

    #' @description Add a data series to the chart.
    #' @param header Cell range for the series name.
    #' @param data Cell range for the numeric values.
    #' @param cat Cell range for the category labels.
    #' @param type Type of chart (waterfall, sunburst, treemap, regionMap).
    #' @param color Hex color or "auto".
    #' @param line_color Border color.
    #' @param line_width Border width.
    #' @param subtotals Numeric vector of indices to treat as subtotals (Waterfall only).
    add_series = function(header = NULL, data, cat = NULL, type = "waterfall", color = "auto",
                      line_color = NULL, line_width = 1, subtotals = NULL) {

      if (is.null(color)) {
        color_idx <- (length(self$series_data) %% length(self$palette)) + 1
        color <- self$palette[color_idx]
      }

      h_label <- tryCatch(if (is.symbol(substitute(header))) deparse1(substitute(header)) else header, error = function(e) NULL)
      c_label <- tryCatch(if (is.symbol(substitute(cat))) deparse1(substitute(cat)) else cat, error = function(e) NULL)

      if (inherits(data, "wb_data")) {
        wb_dims    <- attr(data, "dims")
        wb_sheet   <- attr(data, "sheet")
        col_names  <- names(data)

        has_header <- nrow(wb_dims) > length(attr(data, "row.names"))
        start_row  <- if (has_header) 2 else 1

        # 2. Resolve Series Data and Header
        h_idx <- which(col_names == h_label)
        if (length(h_idx) > 0) {
          h_idx  <- h_idx[1]
          header <- if (has_header) sprintf("'%s'!%s", wb_sheet, wb_dims[1, h_idx]) else NULL
          data   <- sprintf("'%s'!%s:%s", wb_sheet, wb_dims[start_row, h_idx], wb_dims[nrow(wb_dims), h_idx])
        }

        # 3. Resolve Category (cat)
        c_idx <- which(col_names == c_label)
        if (length(c_idx) > 0) {
          c_idx <- c_idx[1]
          cat   <- sprintf("'%s'!%s:%s", wb_sheet, wb_dims[start_row, c_idx], wb_dims[nrow(wb_dims), c_idx])
        }
      }

      # 4. Clean and Store
      header <- to_abs_ref(header)
      data   <- to_abs_ref(data)
      cat    <- to_abs_ref(cat)

      if (!is.null(data) && !grepl("!", data)) {
        stop("Series data must be a sheet reference (e.g., 'Sheet1!A1:A10').", call. = FALSE)
      }

      if (is.logical(subtotals) && subtotals) {
        subtotals <- 0 # avoid bailing
      }

      header <- if (is.null(header)) NA_character_ else header
      cat    <- if (is.null(cat))    NA_character_ else cat

      series_type <- type %||% self$type %||% "barChart"

      self$series_data[[length(self$series_data) + 1]] <- list(
        header = private$fix_quote(header), data = private$fix_quote(data), cat = private$fix_quote(cat),
        type = series_type, color = color, line_color = line_color, line_width = line_width,
        subtotals = subtotals
      )
      invisible(self)
    },

    #' @description Render the internal XML for writing to a file.
    #' @param id_start Numeric starting ID for XML data references.
    #' @return A list containing the XML and attribute mappings.
    render = function(id_start = 1) {
      chart_data_node <- xml2::xml_find_first(self$xml, "//cx:chartData")
      plot_area_node <- xml2::xml_find_first(self$xml, "//cx:plotArea")
      plot_region_node <- xml2::xml_find_first(self$xml, "//cx:plotAreaRegion")

      xml2::xml_remove(xml2::xml_children(chart_data_node))
      xml2::xml_remove(xml2::xml_find_all(plot_region_node, "cx:series"))

      # 1. Plot Area Background (plotSurface)
      xml2::xml_remove(xml2::xml_find_all(plot_region_node, "cx:plotSurface"))
      if (length(self$plot_area_style) > 0) {
        surf <- xml2::xml_add_child(plot_region_node, "cx:plotSurface", .where = 0)
        spPr <- xml2::xml_add_child(surf, "cx:spPr")
        if (!is.null(self$plot_area_style$fill)) private$render_color(spPr, self$plot_area_style$fill)
        if (!is.null(self$plot_area_style$line)) {
          ln <- xml2::xml_add_child(spPr, "a:ln", w = as.character(round(self$plot_area_style$line_width * 12700)))
          private$render_color(ln, self$plot_area_style$line)
        }
      }

      head_attrs <- character()
      body_attrs <- character()
      v_idx <- id_start
      is_hierarchical <- FALSE

      for (i in seq_along(self$series_data)) {
        s <- self$series_data[[i]]
        if (s$type %in% c("sunburst", "treemap")) is_hierarchical <- TRUE

        h_id <- paste0("_xlchart.v1.", v_idx)
        v_idx <- v_idx + 1
        nf_id <- paste0("_xlchart.v1.", v_idx)
        v_idx <- v_idx + 1
        c_id <- paste0("_xlchart.v1.", v_idx)
        v_idx <- v_idx + 1
        d_id <- paste0("_xlchart.v1.", v_idx)
        v_idx <- v_idx + 1

        dat <- xml2::xml_add_child(chart_data_node, "cx:data", id = as.character(i - 1))
        if (!is.null(s$cat)) {
          cat_node <- xml2::xml_add_child(dat, "cx:strDim", type = "cat")
          xml2::xml_add_child(cat_node, "cx:f", c_id)
          xml2::xml_add_child(cat_node, "cx:nf", nf_id)
          body_attrs[c_id] <- s$cat
        }

        dim_type <- if (s$type == "regionMap") "colorVal" else if (s$type %in% c("sunburst", "treemap")) "size" else "val"
        num_dim <- xml2::xml_add_child(dat, "cx:numDim", type = dim_type)
        xml2::xml_add_child(num_dim, "cx:f", d_id)
        xml2::xml_add_child(num_dim, "cx:nf", nf_id)

        ser <- xml2::xml_add_child(plot_region_node, "cx:series", layoutId = s$type, uniqueId = openxlsx2:::st_guid())
        tx_node <- xml2::xml_add_child(xml2::xml_add_child(ser, "cx:tx"), "cx:txData")

        if (!is.na(s$header)) {
          if (private$is_ref(s$header)) {
            # It's a range reference like Sheet1!$A$1
            xml2::xml_add_child(tx_node, "cx:f", h_id)
            head_attrs[h_id] <- s$header
          } else {
            # It's a literal string like "Foo Bar"
            xml2::xml_add_child(tx_node, "cx:v", as.character(s$header))
          }
        }

        if ((length(s$color) == 1 && s$color != "auto") || !is.null(s$line_color)) {
          spPr_ser <- xml2::xml_add_child(ser, "cx:spPr")
          if (length(s$color) == 1 && s$color != "auto") private$render_color(spPr_ser, s$color)
          if (!is.null(s$line_color)) private$render_color(xml2::xml_add_child(spPr_ser, "a:ln", w = as.character(round(s$line_width * 12700))), s$line_color)
        }

        if (length(s$color) > 1) {
          for (j in seq_along(s$color)) {
            dPt <- xml2::xml_add_child(ser, "cx:dPt", idx = as.character(j - 1))
            spPr <- xml2::xml_add_child(dPt, "cx:spPr")
            private$render_color_core(xml2::xml_add_child(spPr, "a:solidFill"), s$color[j])
          }
        }

        if (isTRUE(self$data_label_params$show)) {
          dlbls <- xml2::xml_add_child(ser, "cx:dataLabels", pos = self$data_label_params$pos %||% "outEnd")
          if (!is.null(self$data_label_params$numfmt)) xml2::xml_add_child(dlbls, "cx:numFmt", formatCode = self$data_label_params$numfmt, sourceLinked = "0")
          if (any(!vapply(self$data_label_params$style, is.null, logical(1)))) private$apply_label_style(dlbls, self$data_label_params$style)
          xml2::xml_add_child(dlbls, "cx:visibility", seriesName = "0", categoryName = "0", value = "1")
        }

        xml2::xml_add_child(ser, "cx:dataId", val = as.character(i - 1))

        if (s$type == "waterfall" && !identical(s$subtotals, FALSE)) {
          lpr <- xml2::xml_add_child(ser, "cx:layoutPr")
          st_node <- xml2::xml_add_child(lpr, "cx:subtotals")

          if (is.null(s$subtotals)) {
            coords <- openxlsx2::dims_to_rowcol(gsub(".*!", "", s$data), as_integer = TRUE)
            last_idx <- max(length(coords$row), length(coords$col)) - 1
            xml2::xml_add_child(st_node, "cx:idx", val = "0")
            xml2::xml_add_child(st_node, "cx:idx", val = as.character(last_idx))
          } else {
            for (idx in s$subtotals) {
              xml2::xml_add_child(st_node, "cx:idx", val = as.character(idx))
            }
          }
        }

        head_attrs[h_id] <- s$header
        body_attrs[d_id] <- s$data
        body_attrs[nf_id] <- s$data
      }

      # --- 3. Axes ---
      if (!is_hierarchical) {
        # Find the plotArea container
        plot_area_node <- xml2::xml_find_first(self$xml, "//cx:plotArea")

        # Wipe existing axes to prevent duplication/nesting
        xml2::xml_remove(xml2::xml_find_all(plot_area_node, "cx:axis"))

        # Build siblings by passing the same plot_area_node as parent
        private$render_axis_full(plot_area_node, self$x_axis_style, self$x_title,
                                 self$x_title_style, self$x_numfmt, type = "cat")

        private$render_axis_full(plot_area_node, self$y_axis_style, self$y_title,
                                 self$y_title_style, self$y_numfmt, type = "val")
      }

      # 2. Legends & Titles
      legend_node <- xml2::xml_find_first(self$xml, "//cx:legend")
      l_pos <- self$legend_params$pos %||% "t"
      if (l_pos == "none") {
        xml2::xml_remove(legend_node)
      } else {
        xml2::xml_set_attr(legend_node, "pos", l_pos)
        xml2::xml_set_attr(legend_node, "align", self$legend_params$align %||% "ctr")
        xml2::xml_set_attr(legend_node, "overlay", self$legend_params$overlay %||% "0")
        if (any(!vapply(self$legend_params$style, is.null, logical(1)))) private$apply_legend_text_style(legend_node, self$legend_params$style)
      }

      if (!is.null(self$chart_title)) private$add_rich_text(xml2::xml_find_first(self$xml, "//cx:chart/cx:title"), self$chart_title, self$chart_title_style)

      # 4. Chart Area Styling
      xml2::xml_remove(xml2::xml_find_all(self$xml, "/cx:chartSpace/cx:spPr"))
      if (length(self$chart_area_style) > 0) {
        spPr_chart <- xml2::xml_add_child(self$xml, "cx:spPr")
        if (!is.null(self$chart_area_style$fill)) private$render_color(spPr_chart, self$chart_area_style$fill)
        if (!is.null(self$chart_area_style$line)) {
          ln_chart <- xml2::xml_add_child(spPr_chart, "a:ln", w = as.character(round(self$chart_area_style$line_width * 12700)))
          private$render_color(ln_chart, self$chart_area_style$line)
        }
      }

      out <- openxlsx2::read_xml(as.character(self$xml), pointer = FALSE)
      attr(out, "head") <- head_attrs
      attr(out, "body") <- body_attrs
      out
    }
  ),
  private = list(

    is_ref = function(x) {
      if (is.null(x) || is.na(x) || x == "") return(FALSE)
      # Check if '!' exists and is not at the very end (i.e., has a cell ref after it)
      grepl("!.+", x)
    },

    fix_quote = function(x) {
      if (is.null(x)) return(NULL)
      if (grepl(".+!.+", x) && !grepl("^'", x)) {
        parts <- strsplit(x, "!", fixed = TRUE)[[1]]
        # Ensure we actually have two parts before joining
        if (length(parts) >= 2) {
          return(paste0("'", parts[1], "'!", parts[2]))
        }
      }
      x
    },

    # Renders gridlines for modern charts
    render_gridlines = function(axis_node, type, params) {
      # type is "majorGridlines" or "minorGridlines"
      prefix <- if (type == "majorGridlines") "" else "minor_"
      style_val <- params[[paste0(prefix, "gridlines")]]

      if (is.null(style_val) || isFALSE(style_val)) return()

      grid_node <- xml2::xml_add_child(axis_node, paste0("cx:", type))
      sp_pr <- xml2::xml_add_child(grid_node, "cx:spPr")

      # Use your existing render_color logic from ChartEx
      width <- params[[paste0(prefix, "grid_width")]] %||% 1
      color <- params[[paste0(prefix, "grid_color")]] %||% "D9D9D9"

      ln <- xml2::xml_add_child(sp_pr, "a:ln", w = as.character(round(width * 12700)))
      private$render_color_core(ln, color)

      # Dash type support
      dash <- switch(as.character(style_val), "dotted" = "dot", "dash" = "dash", NULL)
      if (!is.null(dash)) xml2::xml_add_child(ln, "a:prstDash", val = dash)
    },

    render_axis = function(plot_area, style, type = "val") {
      axis_node <- xml2::xml_add_child(plot_area, "cx:axis", type = type)

      # 1. Units (ST_AxisUnit) MUST COME FIRST
      if (!is.null(style$major)) {
        u <- xml2::xml_add_child(axis_node, "cx:majorUnit")
        xml2::xml_set_attr(u, "val", as.character(style$major))
      }
      if (!is.null(style$minor)) {
        u <- xml2::xml_add_child(axis_node, "cx:minorUnit")
        xml2::xml_set_attr(u, "val", as.character(style$minor))
      }

      # 2. Gridlines MUST COME SECOND
      if (!is.null(style$gridlines)) {
        major_grid <- xml2::xml_add_child(axis_node, "cx:majorGridlines")
        sp_pr <- xml2::xml_add_child(major_grid, "cx:spPr")
        ln <- xml2::xml_add_child(sp_pr, "a:ln", w = as.character(round((style$grid_width %||% 1) * 12700)))

        solid_fill <- xml2::xml_add_child(ln, "a:solidFill")
        private$render_color_core(solid_fill, style$grid_color %||% "D9D9D9")

        dash <- switch(as.character(style$gridlines), "dotted" = "dot", "dash" = "dash", NULL)
        if (!is.null(dash)) xml2::xml_add_child(ln, "a:prstDash", val = dash)
      }

      # 3. Text Properties and Titles MUST COME LAST
      # This is why we call apply_axis_style AFTER the units and grids
      private$apply_axis_style(axis_node, style)

      # Add the Title if it exists (Title must be the very last element in cx:axis)
      title_text <- if (type == "cat") self$x_title else self$y_title
      title_style <- if (type == "cat") self$x_title_style else self$y_title_style
      if (!is.null(title_text)) {
        private$add_rich_text(xml2::xml_add_child(axis_node, "cx:title"), title_text, title_style)
      }
    },

    apply_label_style = function(node, s) {
      txPr <- xml2::xml_add_child(node, "cx:txPr")
      bodyPr <- xml2::xml_add_child(txPr, "a:bodyPr", lIns = "0", tIns = "0", rIns = "0", bIns = "0")
      if (!is.null(s$rot)) {
        xml2::xml_set_attr(bodyPr, "rot", as.character(round(s$rot * 60000)))
        xml2::xml_set_attr(bodyPr, "vert", "horz")
      }
      xml2::xml_add_child(txPr, "a:lstStyle")
      p <- xml2::xml_add_child(txPr, "a:p")
      pPr <- xml2::xml_add_child(p, "a:pPr")
      set_run_attrs <- function(n, st) {
        if (!is.null(st$sz)) xml2::xml_set_attr(n, "sz", as.character(st$sz * 100))
        if (!is.null(st$b)) xml2::xml_set_attr(n, "b", if (isTRUE(st$b)) "1" else "0")
        if (!is.null(st$i)) xml2::xml_set_attr(n, "i", if (isTRUE(st$i)) "1" else "0")
      }
      defRPr <- xml2::xml_add_child(pPr, "a:defRPr")
      set_run_attrs(defRPr, s)
      if (!is.null(s$color)) private$render_color(defRPr, s$color)
      endRPr <- xml2::xml_add_child(p, "a:endParaRPr")
      set_run_attrs(endRPr, s)
      if (!is.null(s$color)) private$render_color(endRPr, s$color)
      if (!is.null(s$font)) xml2::xml_add_child(endRPr, "a:latin", typeface = s$font)
    },

    render_axis_full = function(plot_area, s, title, title_style, numfmt, type = "val") {
      # 1. Create Axis
      ax <- xml2::xml_add_child(plot_area, "cx:axis", id = if (type == "cat") "0" else "1")
      is_x <- (type == "cat")

      # 2. Scaling - Apply major/minor units as ATTRIBUTES here
      scaling_tag <- if (is_x) "cx:catScaling" else "cx:valScaling"
      scaling <- xml2::xml_add_child(ax, scaling_tag)

      if (is_x && !is.null(s$gap_width)) xml2::xml_set_attr(scaling, "gapWidth", as.character(s$gap_width))

      if (!is_x) {
        if (!is.null(s$min)) xml2::xml_set_attr(scaling, "min", as.character(s$min))
        if (!is.null(s$max)) xml2::xml_set_attr(scaling, "max", as.character(s$max))
        if (!is.null(s$major)) xml2::xml_set_attr(scaling, "majorUnit", as.character(s$major))
        if (!is.null(s$minor)) xml2::xml_set_attr(scaling, "minorUnit", as.character(s$minor))
      }

      # 3. Title (Sequence position #2)
      if (!is.null(title)) {
        private$add_rich_text(xml2::xml_add_child(ax, "cx:title"), title, title_style)
      }

      # --- Axis Line Style ---
      axSpPr <- xml2::xml_add_child(ax, "cx:spPr")

      # Use line_width if provided (converted to EMUs), else default 0.75pt
      w_val <- if (!is.null(s$line_width)) as.character(round(s$line_width * 12700)) else "9525"
      ln <- xml2::xml_add_child(axSpPr, "a:ln", w = w_val)

      # Wrap color in solidFill to prevent XML errors
      ln_fill <- xml2::xml_add_child(ln, "a:solidFill")
      private$render_color_core(ln_fill, s$color %||% "000000")

      # 4. Gridlines (Fixed Dash mapping)
      if (!is.null(s$gridlines) && !isFALSE(s$gridlines)) {
        g <- xml2::xml_add_child(ax, "cx:majorGridlines")
        sp <- xml2::xml_add_child(g, "cx:spPr")
        ln <- xml2::xml_add_child(sp, "a:ln", w = as.character(round((s$grid_width %||% 0.75) * 12700)))
        private$render_color_core(xml2::xml_add_child(ln, "a:solidFill"), s$grid_color %||% "D9D9D9")

        # dash/dot logic
        dash_val <- switch(as.character(s$gridlines),
                           "dashed" = "dash", "dash" = "dash",
                           "dotted" = "dot", "dot" = "dot", NULL)
        if (!is.null(dash_val)) xml2::xml_add_child(ln, "a:prstDash", val = dash_val)
      }

      # 5. Ticks and Labels
      if (!is.null(s$major_tick)) xml2::xml_add_child(ax, "cx:majorTickMarks", type = s$major_tick)
      xml2::xml_add_child(ax, "cx:tickLabels")
      if (!is.null(numfmt)) xml2::xml_add_child(ax, "cx:numFmt", formatCode = numfmt, sourceLinked = "0")

      # 6. Styling (txPr) - MUST BE LAST
      private$apply_axis_style(ax, s)
    }
    ,
    apply_legend_text_style = function(node, s) {
      xml2::xml_remove(xml2::xml_find_all(node, "cx:txPr"))
      txPr <- xml2::xml_add_child(node, "cx:txPr")
      xml2::xml_add_child(txPr, "a:bodyPr", lIns = "0", tIns = "0", rIns = "0", bIns = "0", anchor = "ctr", anchorCtr = "1")
      xml2::xml_add_child(txPr, "a:lstStyle")
      p <- xml2::xml_add_child(txPr, "a:p")
      pPr <- xml2::xml_add_child(p, "a:pPr", algn = "ctr")
      defRPr <- xml2::xml_add_child(pPr, "a:defRPr")
      if (!is.null(s$sz)) xml2::xml_set_attr(defRPr, "sz", as.character(s$sz * 100))
      if (!is.null(s$b)) xml2::xml_set_attr(defRPr, "b", if (isTRUE(s$b)) "1" else "0")
      if (!is.null(s$i)) xml2::xml_set_attr(defRPr, "i", if (isTRUE(s$i)) "1" else "0")
      if (!is.null(s$color)) private$render_color_core(defRPr, s$color, wrap = TRUE)
      endRPr <- xml2::xml_add_child(p, "a:endParaRPr")
      if (!is.null(s$sz)) xml2::xml_set_attr(endRPr, "sz", as.character(s$sz * 100))
      if (!is.null(s$b)) xml2::xml_set_attr(endRPr, "b", if (isTRUE(s$b)) "1" else "0")
      if (!is.null(s$i)) xml2::xml_set_attr(endRPr, "i", if (isTRUE(s$i)) "1" else "0")
      if (!is.null(s$color)) private$render_color_core(endRPr, s$color, wrap = TRUE)
      if (!is.null(s$font)) xml2::xml_add_child(endRPr, "a:latin", typeface = s$font)
    },
    render_color = function(parent_node, color_val) {
      if (is.null(color_val) || identical(color_val, "auto")) return()
      fill_node <- xml2::xml_add_child(parent_node, "a:solidFill")
      private$render_color_core(fill_node, color_val)
    },
    render_color_core = function(target_node, color_val, wrap = FALSE) {
      if (is.null(color_val)) return()

      # Set the destination node based on the wrap argument
      node <- if (wrap) xml2::xml_add_child(target_node, "a:solidFill") else target_node

      # 1. Handle NULL or empty input (Logic as requested)
      if (is.null(color_val) || length(color_val) == 0) {
        color_val <- "000000"
      }

      # 2. Check for "auto"
      if (length(color_val) == 1 && tolower(as.character(color_val)) == "auto") {
        xml2::xml_add_child(node, "a:schemeClr", val = "accent1")
        return()
      }

      type <- names(color_val)

      # 3. Handle wb_color objects (Hex vs Theme)
      if (inherits(color_val, "wbColour")) {
        # TODO add tint and indexed

        # If it's a theme color, use schemeClr
        if (!is.null(type) && type == "auto") {
          xml2::xml_add_child(node, "a:schemeClr", val = "accent1")
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
          xml2::xml_add_child(node, "a:schemeClr", val = val_name)
          return()
        }

        # Otherwise, get the hex from the rgb attribute
        hex <- if (!is.null(type) && type == "rgb") as.character(color_val) else as.character(color_val[1])
      } else {
        hex <- as.character(color_val[1])
      }

      # 4. Clean and add as RGB
      clean <- toupper(gsub("^#", "", hex))
      if (nchar(clean) == 8) clean <- substr(clean, 3, 8)

      # Final safety check: if 'clean' is empty/invalid, default to black
      if (nchar(clean) != 6) clean <- "000000"

      xml2::xml_add_child(node, "a:srgbClr", val = clean)
    },
    add_rich_text = function(parent, text, s) {
      xml2::xml_remove(xml2::xml_children(parent))

      # 1. Shape properties for the title background/border
      if (!is.null(s$fill) || !is.null(s$line)) {
        spPr <- xml2::xml_add_child(parent, "cx:spPr")
        if (!is.null(s$fill)) private$render_color(spPr, s$fill)
        if (!is.null(s$line)) {
          ln <- xml2::xml_add_child(spPr, "a:ln", w = "12700")
          private$render_color_core(xml2::xml_add_child(ln, "a:solidFill"), s$line)
        }
      }

      # 2. Text Content
      tx <- xml2::xml_add_child(xml2::xml_add_child(parent, "cx:tx"), "cx:rich")
      bodyPr <- xml2::xml_add_child(tx, "a:bodyPr")
      if (!is.null(s$rot)) {
        xml2::xml_set_attr(bodyPr, "rot", as.character(round(s$rot * 60000)))
        xml2::xml_set_attr(bodyPr, "vert", "horz")
      }

      xml2::xml_add_child(tx, "a:lstStyle")
      p <- xml2::xml_add_child(tx, "a:p")
      r <- xml2::xml_add_child(p, "a:r")
      rPr <- xml2::xml_add_child(r, "a:rPr")

      # --- FONT COLOR FIX ---
      # Color MUST be inside a:solidFill
      # If s$color is NULL, we default to black "000000"
      font_color <- s$color %||% "000000"
      fill_node <- xml2::xml_add_child(rPr, "a:solidFill")
      private$render_color_core(fill_node, font_color)

      # Font Styling
      if (!is.null(s$sz)) xml2::xml_set_attr(rPr, "sz", as.character(s$sz * 100))
      if (!is.null(s$b)) xml2::xml_set_attr(rPr, "b", ifelse(isTRUE(s$b), "1", "0"))
      if (!is.null(s$b)) xml2::xml_set_attr(rPr, "i", ifelse(isTRUE(s$i), "1", "0"))
      if (!is.null(s$font)) xml2::xml_add_child(rPr, "a:latin", typeface = s$font)

      xml2::xml_add_child(r, "a:t", text)
    },
    apply_axis_style = function(node, style) {
      pr <- xml2::xml_add_child(node, "cx:txPr")
      xml2::xml_add_child(pr, "a:bodyPr", lIns = "0", tIns = "0", rIns = "0", bIns = "0")
      xml2::xml_add_child(pr, "a:lstStyle")

      p <- xml2::xml_add_child(pr, "a:p")
      pPr <- xml2::xml_add_child(p, "a:pPr")

      # defRPr: where font size and color live
      defRPr <- xml2::xml_add_child(pPr, "a:defRPr")

      # FIX: Use srgbClr to avoid the washed-out schemeClr
      f_color <- style$label_color %||% style$color %||% "000000"
      fill <- xml2::xml_add_child(defRPr, "a:solidFill")
      private$render_color_core(fill, f_color)

      # sz is 1/100 points
      sz_val <- if (!is.null(style$sz)) as.character(style$sz * 100) else "1000"
      xml2::xml_set_attr(defRPr, "sz", sz_val)
      if (isTRUE(style$b)) xml2::xml_set_attr(defRPr, "b", "1")
      if (isTRUE(style$i)) xml2::xml_set_attr(defRPr, "i", "1")

      if (!is.null(style$font)) xml2::xml_add_child(defRPr, "a:latin", typeface = style$font)

      # The final node in the OOXML paragraph
      end_pr <- xml2::xml_add_child(p, "a:endParaRPr", sz = sz_val)
      if (isTRUE(style$b)) xml2::xml_set_attr(end_pr, "b", "1")
      if (isTRUE(style$i)) xml2::xml_set_attr(end_pr, "i", "1")

      # Color wrap
      private$render_color_core(xml2::xml_add_child(end_pr, "a:solidFill"), f_color)

      # Font typeface sync
      if (!is.null(style$font)) {
        xml2::xml_add_child(end_pr, "a:latin", typeface = style$font)
      }
    }
  )
)
