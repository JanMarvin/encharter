#' ChartEx R6 Class for Extended Spreadsheet Charts
#'
#' @description
#' An R6 class to create and manipulate Office OpenXML (OOXML) Extended Charts (ChartEx),
#' including Waterfall, Sunburst, Treemap, and Region Maps, which are not
#' supported by standard Office Open XML chart types.
#'
#' @details
#' This class uses XML to manipulate the underlying XML structure and
#' integrates with `openxlsx2` for workbook generation.
#'
#' @rdname encharter
#' @usage NULL
ChartEx <- R6::R6Class(
  "ChartEx",
  inherit = EncharterBase,
  public = list(

    #' @field color_xml color
    color_xml = character(),

    #' @field style_xml style
    style_xml = character(),

    #' @description Create a new ChartEx object.
    #' @return A new `ChartEx` object.
    #' @param type Initial chart type (e.g., "waterfall", "treemap").
    initialize = function(type = NULL) {
      self$color_xml <- colors1_xml
      self$style_xml <- styleplot_xml

      type <- normalize_encharter_type(type)
      self$type <- type
      self$xml <- read_xml(
        '<cx:chartSpace xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                        xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                        xmlns:cx="http://schemas.microsoft.com/office/drawing/2014/chartex">
           <cx:chartData/><cx:chart><cx:title pos="t" align="ctr" overlay="0"/><cx:plotArea><cx:plotAreaRegion/>
           <cx:axis id="0"><cx:catScaling gapWidth="0.5"/><cx:tickLabels/></cx:axis>
           <cx:axis id="1"><cx:valScaling/><cx:majorGridlines/><cx:tickLabels/></cx:axis>
           </cx:plotArea><cx:legend pos="t" align="ctr" overlay="0"/></cx:chart></cx:chartSpace>'
      )
      self$legend_params <- list(pos = "t", align = "ctr", overlay = "0", style = list())
      self$label_params <- list(show = FALSE)
    },

    #' @description Add a data series to the chart.
    #' @param name Cell range for the series name.
    #' @param data Cell range for the numeric values.
    #' @param label Cell range for the category labels.
    #' @param type Type of chart (waterfall, sunburst, treemap, regionMap).
    #' @param color Hex color or "auto".
    #' @param line_color Border color.
    #' @param line_width Border width.
    #' @param gap_width Integer between 0 and 500.
    #' @param subtotals Numeric vector of indices to treat as subtotals (Waterfall only).
    #' @param statistics Quartile method: "inclusive" or "exclusive".
    #' @param binning A list for Histogram/BoxWhisker:
    #'   `binSize` (numeric), `binCount` (integer), `intervalClosed` ("left", "right"),
    #'   `underflow` (numeric or "auto"), `overflow` (numeric or "auto").
    #' @param visibility A named list of logicals for BoxWhisker/Waterfall:
    #'   `connectorLines`, `meanLine`, `meanMarker`, `nonoutliers`, `outliers`.
    #' @param parent_label Treemap label style: "overlapping", "banner", or "none".
    add_series = function(name = NULL, data, label = NULL, type = NULL, color = "auto",
                          line_color = NULL, line_width = 1,  gap_width = NULL, subtotals = NULL,
                          statistics = NULL, binning = NULL,
                          visibility = NULL, parent_label = "overlapping") {

      # not sure if changing the type here is a good idea
      type <- normalize_encharter_type(type)
      private$validate_input(
        type,
        ENCHARTER_EXTENDED,
        "series type"
      )

      if (is.null(color)) {
        color_idx <- (length(self$series_data) %% length(self$palette)) + 1
        color <- self$palette[color_idx]
      }

      h_label <- tryCatch(if (is.symbol(substitute(name))) deparse1(substitute(name)) else name, error = function(e) NULL)
      c_label <- tryCatch(if (is.symbol(substitute(label))) deparse1(substitute(label)) else label, error = function(e) NULL)

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
          name <- if (has_header) sprintf("%s!%s", wb_sheet, wb_dims[1, h_idx]) else NULL
          data   <- sprintf("%s!%s:%s", wb_sheet, wb_dims[start_row, h_idx], wb_dims[nrow(wb_dims), h_idx])
        }

        # 3. Resolve Category (label)
        c_idx <- which(col_names == c_label)
        if (length(c_idx) > 0) {
          c_idx <- c_idx[1]
          label <- sprintf("%s!%s:%s", wb_sheet, wb_dims[start_row, c_idx], wb_dims[nrow(wb_dims), c_idx])
        }
      }

      # 4. Clean and Store
      name <- to_abs_ref(name)
      data   <- to_abs_ref(data)
      label  <- to_abs_ref(label)

      if (!is.null(data) && !grepl("!", data)) {
        stop("Series data must be a sheet reference (e.g., 'Sheet1!A1:A10').", call. = FALSE)
      }

      # @param aggregation (unknown, undocumented complex type?)
      aggregation <- NULL
      # @param geography Map projection: "mercator", "miller", "robinson", or "albers". (does not wrok yet)
      geography <- NULL

      if (is.logical(subtotals) && subtotals) {
        subtotals <- 0 # avoid bailing
      }

      if (is.null(binning)) {
        binning <- list()
      }

      if (is.null(visibility)) {
        visibility <- list()
      }

      name <- if (is.null(name)) NA_character_ else name
      label  <- if (is.null(label))    NA_character_ else label

      series_type <- type %||% self$type %||% "waterfall"
      series_type <- normalize_encharter_type(series_type)

      self$series_data[[length(self$series_data) + 1]] <- list(
        name = private$fix_quote(name),
        data = private$fix_quote(data),
        label = private$fix_quote(label),
        type = series_type,
        color = color,
        line_color = line_color,
        line_width = line_width,
        gap_width = gap_width,
        subtotals = subtotals,
        statistics = statistics,
        geography = geography,
        aggregation = aggregation,
        binning = binning,
        visibility = visibility,
        parent_label = parent_label
      )
      invisible(self)
    },

    #' @description Render the internal XML for writing to a file.
    #' @param id_start Numeric starting ID for XML data references.
    #' @param guid a guid
    #' @return A list containing the XML and attribute mappings.
    render = function(id_start = 1, guid = "{C59B1284-E301-0D0F-1B20-FD96A66D6E43}") {
      chart_data_node <- xml_find_first(self$xml, "//cx:chartData")
      plot_area_node <- xml_find_first(self$xml, "//cx:plotArea")
      plot_region_node <- xml_find_first(self$xml, "//cx:plotAreaRegion")

      xml_remove(xml_children(chart_data_node))
      xml_remove(xml_find_all(plot_region_node, "cx:series"))

      # 1. Plot Area Background (plotSurface)
      xml_remove(xml_find_all(plot_region_node, "cx:plotSurface"))
      if (length(self$plot_style) > 0) {
        surf <- xml_add_child(plot_region_node, "cx:plotSurface", .where = 0)
        spPr <- xml_add_child(surf, "cx:spPr")
        if (!is.null(self$plot_style$fill)) private$render_color(spPr, self$plot_style$fill)
        if (!is.null(self$plot_style$line)) {
          ln <- xml_add_child(spPr, "a:ln", w = as.character(round(self$plot_style$line_width * 12700)))
          private$render_color(ln, self$plot_style$line)
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

        dat <- xml_add_child(chart_data_node, "cx:data", id = as.character(i - 1))
        if (!is.null(s$label)) {
          cat_node <- xml_add_child(dat, "cx:strDim", type = "cat")
          xml_add_child(cat_node, "cx:f", c_id)
          xml_add_child(cat_node, "cx:nf", nf_id)
          body_attrs[c_id] <- s$label
        }

        dim_type <- if (s$type == "regionMap") "colorVal" else if (s$type %in% c("sunburst", "treemap")) "size" else "val"
        num_dim <- xml_add_child(dat, "cx:numDim", type = dim_type)
        xml_add_child(num_dim, "cx:f", d_id)
        xml_add_child(num_dim, "cx:nf", nf_id)

        ser <- xml_add_child(plot_region_node, "cx:series", layoutId = s$type, uniqueId = guid)

        if (!is.na(s$name)) {
          tx_node <- xml_add_child(xml_add_child(ser, "cx:tx"), "cx:txData")
          if (private$is_ref(s$name)) {
            # It's a range reference like Sheet1!$A$1
            xml_add_child(tx_node, "cx:f", h_id)
            head_attrs[h_id] <- s$name
          } else {
            # It's a literal string like "Foo Bar"
            xml_add_child(tx_node, "cx:v", as.character(s$name))
          }
        }

        if ((length(s$color) == 1 && s$color != "auto") || !is.null(s$line_color)) {
          spPr_ser <- xml_add_child(ser, "cx:spPr")
          if (length(s$color) == 1 && s$color != "auto") private$render_color(spPr_ser, s$color)
          if (!is.null(s$line_color)) private$render_color(xml_add_child(spPr_ser, "a:ln", w = as.character(round(s$line_width * 12700))), s$line_color)
        }

        if (length(s$color) > 1) {
          for (j in seq_along(s$color)) {
            dPt <- xml_add_child(ser, "cx:dPt", idx = as.character(j - 1))
            spPr <- xml_add_child(dPt, "cx:spPr")
            private$render_color_core(xml_add_child(spPr, "a:solidFill"), s$color[j])
          }
        }

        if (isTRUE(self$label_params$show_cat) || isTRUE(self$label_params$show_val) || isTRUE(self$label_params$show_legend_key)) {
          dlbls <- xml_add_child(ser, "cx:dataLabels", pos = self$label_params$pos %||% "outEnd")
          if (!is.null(self$label_params$format)) xml_add_child(dlbls, "cx:numFmt", formatCode = self$label_params$format, sourceLinked = "0")
          if (any(!vapply(self$label_params$style, is.null, logical(1)))) private$apply_label_style(dlbls, self$label_params$style)
          show_cat <- ifelse(isTRUE(self$label_params$show_cat), "1", "0")
          show_val <- ifelse(isTRUE(self$label_params$show_val), "1", "0")
          show_key <- ifelse(isTRUE(self$label_params$show_legend_key), "1", "0")
          xml_add_child(dlbls, "cx:visibility", seriesName = show_key, categoryName = show_cat, value = show_val)
        }

        xml_add_child(ser, "cx:dataId", val = as.character(i - 1))

        # --- Series Layout Properties (layoutPr) ---
        has_lpr <- !is.null(s$statistics) || !is.null(s$subtotals) ||
                   !is.null(s$geography)  || !is.null(s$aggregation) ||
                   !is.null(s$binning)    || length(s$visibility) > 0 ||
                   s$type %in% c("treemap", "sunburst")

        if (has_lpr) {
          lpr <- xml_add_child(ser, "cx:layoutPr")

          # 1. Parent Label Layout (ST_ParentLabelLayout)
          # Values: "none", "overlapping", "banner"
          if (s$type %in% c("treemap", "sunburst")) {
            label_val <- s$parent_label %||% "overlapping"
            xml_add_child(lpr, "cx:parentLabelLayout", val = as.character(label_val))
          }

          # 2. Region Label Layout (For Maps)
          if (s$type == "regionMap") {
            xml_add_child(lpr, "cx:regionLabelLayout", val = "bestFitOnly")
          }

          # 3. visibility
          if (length(s$visibility) > 0) {
            vis <- xml_add_child(lpr, "cx:visibility")
            for (attr_name in names(s$visibility)) {
              val <- if (isTRUE(s$visibility[[attr_name]])) "true" else "false"
              xml_set_attr(vis, attr_name, val)
            }
          }

          # 4. aggregation (Empty Tag)
          if (isTRUE(s$aggregation)) {
            xml_add_child(lpr, "cx:aggregation")

          # 3. Binning (Choice: binSize or binCount)
          } else if (length(s$binning) > 0) {
            # Mapping full names to OOXML's internal single-char codes
            int_closed <- switch(as.character(s$binning$intervalClosed %||% ""),
                                "left"  = "l",
                                "right" = "r",
                                as.character(s$binning$intervalClosed))

            bn <- xml_add_child(lpr, "cx:binning")
            if (nzchar(int_closed)) xml_set_attr(bn, "intervalClosed", int_closed)

            if (!is.null(s$binning$underflow)) {
              xml_set_attr(bn, "underflow", as.character(s$binning$underflow))
            }
            if (!is.null(s$binning$overflow)) {
              xml_set_attr(bn, "overflow", as.character(s$binning$overflow))
            }

            # Child Elements use 'val' attribute per spreadsheet XML sample
            if (!is.null(s$binning$binSize)) {
              xml_add_child(bn, "cx:binSize", val = as.character(s$binning$binSize))
            } else if (!is.null(s$binning$binCount)) {
              xml_add_child(bn, "cx:binCount", val = as.character(s$binning$binCount))
            }
          }

          # 3. geography (CT_Geography)
          if (!is.null(s$geography)) {
            message("currently not implemented")
          #   # Requires culture and attribution attributes to load correctly
          #   xml_add_child(lpr, "cx:geography",
          #                       projectionType = as.character(s$geography),
          #                       cultureLanguage = "en-US",
          #                       cultureRegion = "US",
          #                       attribution = "Bing")
          }

          # 4. statistics (CT_Statistics)
          if (!is.null(s$statistics)) {
            # Attribute MUST be quartileMethod (ST_QuartileMethod: inclusive/exclusive)
            xml_add_child(lpr, "cx:statistics",
                                quartileMethod = as.character(s$statistics))
          }

          # 5. subtotals (CT_Subtotals)
          if (s$type == "waterfall" && !is.null(s$subtotals) && !identical(s$subtotals, FALSE)) {
            sub_node <- xml_add_child(lpr, "cx:subtotals")
            if (is.numeric(s$subtotals)) {
              for (idx in s$subtotals) {
                xml_add_child(sub_node, "cx:idx", val = as.character(idx))
              }
            } else {
              coords <- openxlsx2::dims_to_rowcol(gsub(".*!", "", s$data), as_integer = TRUE)
              last_idx <- max(length(coords$row), length(coords$col)) - 1
              xml_add_child(sub_node, "cx:idx", val = as.character(last_idx))
            }
          }
        }

        head_attrs[h_id] <- s$name
        body_attrs[d_id] <- s$data
        body_attrs[nf_id] <- s$data
      }

      # --- 3. Axes ---
      if (!is_hierarchical) {
        # Find the plotArea container
        plot_area_node <- xml_find_first(self$xml, "//cx:plotArea")

        # Wipe existing axes to prevent duplication/nesting
        xml_remove(xml_find_all(plot_area_node, "cx:axis"))

        # Build siblings by passing the same plot_area_node as parent
        private$render_axis_full(
          plot_area_node,
          s = self$axis_params$x,
          gap_width = if (length(self$series_data)) self$series_data[[1]]$gap_width else NULL,
          title = self$x_title$text,
          title_style = self$x_title$style,
          type = "cat"
        )

        private$render_axis_full(
          plot_area_node,
          s = self$axis_params$y,
          gap_width = NULL,
          title = self$y_title$text,
          title_style = self$y_title$style,
          type = "val"
        )
      } else {
        # there are no axis required for hierarchical charts
        xml_remove(xml_find_all(self$xml, "//cx:axis"))
      }

      # 2. Legends & Titles
      legend_node <- xml_find_first(self$xml, "//cx:legend")
      l_pos <- self$legend_params$pos %||% "t"
      if (l_pos == "none") {
        xml_remove(legend_node)
      } else {
        xml_set_attr(legend_node, "pos", l_pos)
        xml_set_attr(legend_node, "align", self$legend_params$align %||% "ctr")
        xml_set_attr(legend_node, "overlay", self$legend_params$overlay %||% "0")
        if (any(!vapply(self$legend_params$style, is.null, logical(1)))) private$apply_legend_text_style(legend_node, self$legend_params$style)
      }

      if (!is.null(self$chart_title$text)) private$add_rich_text(xml_find_first(self$xml, "//cx:chart/cx:title"), self$chart_title$text, self$chart_title$style)

      # 4. Chart Area Styling
      xml_remove(xml_find_all(self$xml, "/cx:chartSpace/cx:spPr"))
      if (length(self$chart_style) > 0) {
        spPr_chart <- xml_add_child(self$xml, "cx:spPr")
        if (!is.null(self$chart_style$fill)) private$render_color(spPr_chart, self$chart_style$fill)
        if (!is.null(self$chart_style$line)) {
          ln_chart <- xml_add_child(spPr_chart, "a:ln", w = as.character(round(self$chart_style$line_width * 12700)))
          private$render_color(ln_chart, self$chart_style$line)
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

    # Renders grid lines for modern charts
    render_grid_lines = function(axis_node, type, params) {
      # type is "majorGridlines" or "minorGridlines"
      prefix <- if (type == "majorGridlines") "" else "minor_"
      style_val <- params[[paste0(prefix, "grid_lines")]]

      if (is.null(style_val) || isFALSE(style_val)) return()

      grid_node <- xml_add_child(axis_node, paste0("cx:", type))
      sp_pr <- xml_add_child(grid_node, "cx:spPr")

      # Use your existing render_color logic from ChartEx
      width <- params[[paste0(prefix, "grid_width")]] %||% 1
      color <- params[[paste0(prefix, "grid_color")]] %||% "D9D9D9"

      ln <- xml_add_child(sp_pr, "a:ln", w = as.character(round(width * 12700)))
      private$render_color_core(xml_add_child(ln, "a:solidFill"), color)

      # Dash type support
      dash <- switch(as.character(style_val), "dotted" = "dot", "dash" = "dash", NULL)
      if (!is.null(dash)) xml_add_child(ln, "a:prstDash", val = dash)
    },

    apply_label_style = function(node, s) {
      txPr <- xml_add_child(node, "cx:txPr")
      bodyPr <- xml_add_child(
        txPr, "a:bodyPr",
        lIns = "0", tIns = "0", rIns = "0", bIns = "0", wrap = "square"
      )
      if (!is.null(s$rotation)) {
        xml_set_attr(bodyPr, "rot", as.character(round(s$rotation * 60000)))
        xml_set_attr(bodyPr, "vert", "horz")
      }
      xml_add_child(txPr, "a:lstStyle")
      p <- xml_add_child(txPr, "a:p")
      pPr <- xml_add_child(p, "a:pPr")
      set_run_attrs <- function(n, st) {
        if (!is.null(st$font_size)) xml_set_attr(n, "sz", as.character(st$font_size * 100))
        if (!is.null(st$bold)) xml_set_attr(n, "b", if (isTRUE(st$bold)) "1" else "0")
        if (!is.null(st$italic)) xml_set_attr(n, "i", if (isTRUE(st$italic)) "1" else "0")
      }
      defRPr <- xml_add_child(pPr, "a:defRPr")
      set_run_attrs(defRPr, s)
      if (!is.null(s$font_color) || !is.null(s$color))
        private$render_color_core(defRPr, s$font_color %||% s$color, wrap = TRUE)
      endRPr <- xml_add_child(p, "a:endParaRPr")
      set_run_attrs(endRPr, s)
      if (!is.null(s$font_color) || !is.null(s$color))
        private$render_color_core(endRPr, s$font_color %||% s$color, wrap = TRUE)
      if (!is.null(s$font_name)) xml_add_child(endRPr, "a:latin", typeface = s$font_name)
    },

    render_axis_full = function(plot_area, s, title, title_style, gap_width = NULL, type = "val") {
      # 1. Create Axis with correct ID (0 for X/Category, 1 for Y/Value)
      ax <- xml_add_child(plot_area, "cx:axis", id = if (type == "cat") "0" else "1")
      is_x <- (type == "cat")

      # 2. Scaling (ST_AxisUnit & ST_Scaling)
      # In ChartEx, Units and Min/Max are often attributes of the scaling node
      scaling_tag <- if (is_x) "cx:catScaling" else "cx:valScaling"
      scaling <- xml_add_child(ax, scaling_tag)

      if (is_x && !is.null(gap_width)) xml_set_attr(scaling, "gapWidth", as.character(gap_width))

      if (!is_x) {
        if (!is.null(s$min)) xml_set_attr(scaling, "min", as.character(s$min))
        if (!is.null(s$max)) xml_set_attr(scaling, "max", as.character(s$max))
        # Standard Chart uses 'major', map it to OOXML 'majorUnit'
        if (!is.null(s$major)) xml_set_attr(scaling, "majorUnit", as.character(s$major))
        if (!is.null(s$minor)) xml_set_attr(scaling, "minorUnit", as.character(s$minor))
      }

      # 3. Title (Must follow scaling in sequence)
      if (!is.null(title)) {
        private$add_rich_text(xml_add_child(ax, "cx:title"), title, title_style)
      }

      # 4. Axis Line Style (spPr)
      axSpPr <- xml_add_child(ax, "cx:spPr")
      # Use line_width if provided (converted to EMUs), else default 0.75pt
      w_val <- if (!is.null(s$line_width)) as.character(round(s$line_width * 12700)) else "9525"
      ln <- xml_add_child(axSpPr, "a:ln", w = w_val)
      # Wrap color in solidFill to prevent XML errors
      ln_fill <- xml_add_child(ln, "a:solidFill")
      private$render_color_core(ln_fill, s$color %||% "000000")

      # 5. Gridlines (Aligned with Chart logic)
      if (!is.null(s$grid_lines) && !isFALSE(s$grid_lines)) {
        g <- xml_add_child(ax, "cx:majorGridlines")
        sp <- xml_add_child(g, "cx:spPr")
        ln <- xml_add_child(sp, "a:ln", w = as.character(round((s$grid_width %||% 0.75) * 12700)))
        private$render_color_core(xml_add_child(ln, "a:solidFill"), s$grid_color %||% "D9D9D9")

        # dash/dot logic
        dash_val <- switch(as.character(s$grid_lines),
                           "dashed" = "dash", "dash" = "dash",
                           "dotted" = "dot", "dot" = "dot", NULL)
        if (!is.null(dash_val)) xml_add_child(ln, "a:prstDash", val = dash_val)
      }

      # 6. Ticks and Labels
      if (!is.null(s$major_tick)) xml_add_child(ax, "cx:majorTickMarks", type = s$major_tick)
      xml_add_child(ax, "cx:tickLabels")

      # 7. Styling (txPr) - MUST BE LAST
      # Number Format (NEW: mapping s$format from axis_params)
      if (!is.null(s$format)) {
        xml_add_child(ax, "cx:numFmt", formatCode = as.character(s$format), sourceLinked = "0")
      }

      # 8. Text Styling (txPr) - MUST BE LAST
      private$apply_axis_style(ax, s)
    },

    apply_legend_text_style = function(node, s) {
      xml_remove(xml_find_all(node, "cx:txPr"))
      txPr <- xml_add_child(node, "cx:txPr")
      xml_add_child(txPr, "a:bodyPr", lIns = "0", tIns = "0", rIns = "0", bIns = "0", anchor = "ctr", anchorCtr = "1")
      xml_add_child(txPr, "a:lstStyle")
      p <- xml_add_child(txPr, "a:p")
      pPr <- xml_add_child(p, "a:pPr", algn = "ctr")
      defRPr <- xml_add_child(pPr, "a:defRPr")
      if (!is.null(s$font_size)) xml_set_attr(defRPr, "sz", as.character(s$font_size * 100))
      if (!is.null(s$bold)) xml_set_attr(defRPr, "b", if (isTRUE(s$bold)) "1" else "0")
      if (!is.null(s$italic)) xml_set_attr(defRPr, "i", if (isTRUE(s$italic)) "1" else "0")
      if (!is.null(s$font_color)) private$render_color_core(defRPr, s$font_color, wrap = TRUE)
      endRPr <- xml_add_child(p, "a:endParaRPr")
      if (!is.null(s$font_size)) xml_set_attr(endRPr, "sz", as.character(s$font_size * 100))
      if (!is.null(s$bold)) xml_set_attr(endRPr, "b", if (isTRUE(s$bold)) "1" else "0")
      if (!is.null(s$italic)) xml_set_attr(endRPr, "i", if (isTRUE(s$italic)) "1" else "0")
      if (!is.null(s$font_color)) private$render_color_core(endRPr, s$font_color, wrap = TRUE)
      if (!is.null(s$font_name)) xml_add_child(endRPr, "a:latin", typeface = s$font_name)
    },

    add_rich_text = function(parent, text, s) {
      xml_remove(xml_children(parent))

      # 1. Shape properties for the title background/border
      if (!is.null(s$fill) || !is.null(s$line)) {
        spPr <- xml_add_child(parent, "cx:spPr")
        if (!is.null(s$fill)) private$render_color(spPr, s$fill)
        if (!is.null(s$line)) {
          ln <- xml_add_child(spPr, "a:ln", w = as.character(round(s$line_width %||% 1L * 12700)))
          private$render_color_core(xml_add_child(ln, "a:solidFill"), s$line)
        }
      }

      # 2. Text Content
      tx <- xml_add_child(xml_add_child(parent, "cx:tx"), "cx:rich")
      bodyPr <- xml_add_child(tx, "a:bodyPr")
      if (!is.null(s$rotation)) {
        xml_set_attr(bodyPr, "rot", as.character(round(s$rotation * 60000)))
        xml_set_attr(bodyPr, "vert", "horz")
      }

      xml_add_child(tx, "a:lstStyle")
      p <- xml_add_child(tx, "a:p")
      pPr <- xml_add_child(p, "a:pPr", algn = "ctr")
      if (inherits(text, "fmt_txt")) {
        wrapper <- sprintf('<x xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">%s</x>', fmt_txt2(text))
        xml_add_child(p, xml_find_all(read_xml(wrapper), ".//a:r"))
      } else {
        r <- xml_add_child(p, "a:r")
        rPr <- xml_add_child(r, "a:rPr")

        # Color MUST be inside a:solidFill
        # If s$color is NULL, we default to black "000000"
        color <- s$font_color %||% "000000"
        fill_node <- xml_add_child(rPr, "a:solidFill")
        private$render_color_core(fill_node, color)

        # Font Styling
        if (!is.null(s$font_size)) xml_set_attr(rPr, "sz", as.character(s$font_size * 100))
        if (!is.null(s$bold)) xml_set_attr(rPr, "b", ifelse(isTRUE(s$bold), "1", "0"))
        if (!is.null(s$italic)) xml_set_attr(rPr, "i", ifelse(isTRUE(s$italic), "1", "0"))
        if (!is.null(s$font_name)) xml_add_child(rPr, "a:latin", typeface = s$font_name)

        xml_add_child(r, "a:t", text)
      }
    },
    apply_axis_style = function(node, style) {
      pr <- xml_add_child(node, "cx:txPr")
      xml_add_child(pr, "a:bodyPr", lIns = "0", tIns = "0", rIns = "0", bIns = "0")
      xml_add_child(pr, "a:lstStyle")

      p <- xml_add_child(pr, "a:p")
      pPr <- xml_add_child(p, "a:pPr")

      # defRPr: where font size and color live
      defRPr <- xml_add_child(pPr, "a:defRPr")

      # FIX: Use srgbClr to avoid the washed-out schemeClr
      f_color <- style$font_color %||% style$color %||% "000000"
      fill <- xml_add_child(defRPr, "a:solidFill")
      private$render_color_core(fill, f_color)

      # sz is 1/100 points
      sz_val <- if (!is.null(style$font_size)) as.character(style$font_size * 100) else "1000"
      xml_set_attr(defRPr, "sz", sz_val)
      if (isTRUE(style$bold)) xml_set_attr(defRPr, "b", "1")
      if (isTRUE(style$italic)) xml_set_attr(defRPr, "i", "1")

      if (!is.null(style$font_name)) xml_add_child(defRPr, "a:latin", typeface = style$font_name)

      # The final node in the OOXML paragraph
      end_pr <- xml_add_child(p, "a:endParaRPr", sz = sz_val)
      if (isTRUE(style$bold)) xml_set_attr(end_pr, "b", "1")
      if (isTRUE(style$italic)) xml_set_attr(end_pr, "i", "1")

      # Color wrap
      private$render_color_core(xml_add_child(end_pr, "a:solidFill"), f_color)

      # Font typeface sync
      if (!is.null(style$font_name)) {
        xml_add_child(end_pr, "a:latin", typeface = style$font_name)
      }
    }
  )
)
