library(openxlsx2)

# 1. Dummy Data: Sales by Department (with duplicates to test aggregation)
df_tree <- data.frame(
  Dept = c("Tech", "Tech", "Office", "Office", "Furniture"),
  SubDept = c("Laptops", "Tablets", "Pens", "Paper", "Chairs"),
  Sales = c(5000, 2000, 300, 150, 1200)
)

ce_tree <- ec("treemap")

# 2. Add Series with Aggregation
ce_tree$add_series(
  header = "Total Sales",
  data = "Sheet1!$C$2:$C$6",
  cat = "Sheet1!$A$2:$B$6",
  parent_label = "banner"
)$
  set_data_label_style(
    show_cat = TRUE,
    show_val = FALSE,
    show_legend_key = FALSE,
    pos = "outEnd",       # or "ctr", "inEnd", "inBase"
    sz = 10,
    bold = TRUE,
    color = wb_color("gray")
  )

ce_tree$set_chart_title("Treemap Aggregation")

wb <- wb_workbook() |>
  wb_add_worksheet("Sheet1") |>
  wb_add_data(x = df_tree) |>
  openxlsx2::wb_add_encharter(dims = "D2:L25", graph = ce_tree)

if (interactive()) wb$open()
