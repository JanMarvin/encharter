# nolint start: object_usage_linter.
# Real-data showcase using the built-in Seatbelts time series. Computes a
# simple 12-month rolling mean of deaths/total_casualties and a distance-
# weighted version, then draws a 4-series chart on a dedicated chart sheet:
# Deaths bars (primary), a faint gray bar on the secondary marking the seat-
# belt-law dummy, and two rolling-rate lines (orange + dark green) on the
# secondary axis. Title is rich (fmt_txt). Y2-axis is formatted as percent
# (0.06-0.08). Requires dplyr, slider, zoo, scales.

seatbelts_chart <- function() {
  require(openxlsx2)
  require(encharter)

  for (p in c("dplyr", "slider", "zoo", "scales")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("install '", p, "' to run this example")
  }
  library(dplyr)
  library(slider)

  seatbelts_clean <- Seatbelts |>
    as.data.frame() |>
    as_tibble() |>
    mutate(date = zoo::as.Date(zoo::as.yearmon(time(Seatbelts))),
           .before = 1,
           law = as.integer(!law)) |>
    rename(
      total_casualties = drivers,
      deaths           = DriversKilled,
      distance_driven  = kms
    )

  seatbelts_final <- seatbelts_clean |>
    mutate(
      fatality_rate_12m = slide_mean(
        x        = deaths / total_casualties,
        before   = 11,
        complete = TRUE
      ),
      fatality_rate_12m_weighted = slide_dbl(
        .x = seatbelts_clean,
        .f = ~ {
          numerator   <- sum(.x$distance_driven * (.x$deaths / .x$total_casualties))
          denominator <- sum(.x$distance_driven)
          numerator / denominator
        },
        .before   = 11,
        .complete = TRUE
      )
    ) |>
    rename(
      Deaths                       = deaths,
      `Before law change`          = law,
      `Weighted 12m Fatality Rate` = fatality_rate_12m_weighted,
      `12m Fatality Rate`          = fatality_rate_12m
    )

  wb <- wb_workbook()$add_worksheet()$add_data(x = seatbelts_final)
  wd <- wb_data(wb)

  txt <- fmt_txt(
    "Ratio of Deaths to Total Serious Injuries and Weighted by Distance Driven (KMs)",
    bold = TRUE, size = 18
  ) +
    fmt_txt(
      "\nData: UK Driver Seatbelts Dataset (1969-1984)",
      italic = TRUE, color = wb_color("black"), size = 14
    )

  e <- ec(type = "barplot")
  e$add_series(data = wd, name = Deaths, label = date)
  e$add_series(data = wd, name = `Before law change`, label = date,
               type = "barplot", secondary = TRUE,
               line_color = wb_color(hex   = scales::alpha("lightgray", alpha = 0.2),
                                     format = "RGBA"),
               gap_width = 0, overlap = 100)
  e$add_series(data = wd, name = `Weighted 12m Fatality Rate`, label = date,
               secondary = TRUE, type = "line",
               line_color = wb_color("orange"), line_width = 2)
  e$add_series(data = wd, name = `12m Fatality Rate`, label = date,
               secondary = TRUE, type = "line",
               line_color = wb_color("darkgreen"), line_width = 2)

  e$set_x_axis(
    format     = "YYYY",
    minor      = 4,
    major      = 12,
    minor_time = "months",
    major_time = "months",
    min        = unlist(convert_to_excel_date(data.frame(date = as.Date("1975-01-01"))))
  )
  e$set_y_axis(min = 0, max = 200)
  e$set_y2_axis(format = "0.0%", min = 0.06, max = 0.08)
  e$set_chart_title(txt)
  e$set_y2_title("Rolling 12-Month (Weighted) Driver Fatality Rate")
  e$set_y_title("Total Amount of Deaths")
  e$set_legend_style(pos = "bottom")

  wb$add_chartsheet()
  wb$add_encharter(graph = e)

  if (interactive()) wb$open()
  invisible(wb)
}

seatbelts_chart()

# nolint end
