# Treemap with two-level labels (Dept + SubDept), parent_label = "banner"
# (vs the default "overlapping" or "none"), and gray data labels showing only
# the category name (show_val = FALSE). Position "outEnd". Chart at D2:L25.

treemap_with_args <- function() {
  require(openxlsx2)
  require(encharter)

  df_tree <- data.frame(
    Dept    = c("Tech", "Tech", "Office", "Office", "Furniture"),
    SubDept = c("Laptops", "Tablets", "Pens", "Paper", "Chairs"),
    Sales   = c(5000, 2000, 300, 150, 1200)
  )

  ce_tree <- ec("treemap")

  ce_tree$add_series(
    name         = "Total Sales",
    data         = "Sheet1!$C$2:$C$6",
    label        = "Sheet1!$A$2:$B$6",
    parent_label = "banner"
  )$
    set_data_label_style(
      show_cat        = TRUE,
      show_val        = FALSE,
      show_legend_key = FALSE,
      pos             = "outEnd",
      font_size       = 10,
      bold            = TRUE,
      color           = wb_color("gray")
    )

  ce_tree$set_chart_title("Treemap Aggregation")

  wb <- wb_workbook() |>
    wb_add_worksheet("Sheet1") |>
    wb_add_data(x = df_tree) |>
    wb_add_encharter(dims = "D2:L25", graph = ce_tree)

  if (interactive()) wb$open()
  invisible(wb)
}

treemap_with_args()
