test_that("wb_data resolution and NSE support", {
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("DataSheet") |>
    openxlsx2::wb_add_data(x = data.frame(Revenue = 10:15, Month = 1:6))

  dat <- openxlsx2::wb_data(wb, sheet = "DataSheet", col_names = TRUE)

  chart <- Chart$new("lineChart")

  # Test NSE (unquoted names)
  chart$add_series(data = dat, name = Revenue, label = Month)

  expect_equal(chart$series_data[[1]]$name, "'DataSheet'!$A$1")
  expect_equal(chart$series_data[[1]]$data,   "'DataSheet'!$A$2:$A$7")
  expect_equal(chart$series_data[[1]]$label,    "'DataSheet'!$B$2:$B$7")

  # Test helpful error message for typos
  expect_error(chart$add_series(data = dat, name = Revnue), "object 'Revnue' not found")

  wb$add_chart_xml(xml = chart$render())
})

test_that("wb_data resolution and NSE support", {
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("DataSheet") |>
    openxlsx2::wb_add_data(x = data.frame(Revenue = 10:15, Month = 1:6), col_names = FALSE)

  dat <- openxlsx2::wb_data(wb, sheet = "DataSheet", col_names = FALSE)

  chart <- Chart$new("lineChart")

  # Test NSE (unquoted names)
  chart$add_series(data = dat, name = A)

  expect_equal(chart$series_data[[1]]$name, NULL)
  expect_equal(chart$series_data[[1]]$data,   "'DataSheet'!$A$1:$A$6")
  expect_equal(chart$series_data[[1]]$label,    NULL)

  # Test helpful error message for typos
  expect_error(chart$add_series(data = dat, name = Revnue), "object 'Revnue' not found")

  wb$add_chart_xml(xml = chart$render())
})

test_that("ChartEx handles wb_data", {
  wb <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("WF") |>
    openxlsx2::wb_add_data(x = data.frame(Label = c("A", "B"), Val = c(10, 20)))

  dat <- openxlsx2::wb_data(wb, sheet = "WF")
  ce <- ChartEx$new()
  ce$add_series(data = dat, name = Val, label = Label, type = "waterfall")

  expect_equal(ce$series_data[[1]]$name, "'WF'!$B$1")

  wb <- openxlsx2::wb_add_encharter(wb, graph = ce)
})

# ---- helpers ----------------------------------------------------------------

make_wb <- function(df, sheet = "Sheet1") {
  openxlsx2::wb_workbook()$add_worksheet(sheet)$add_data(x = df, sheet = sheet)
}

xml_str <- function(chart) {
  as.character(chart$render())
}

stock_df <- data.frame(
  Date  = seq(as.Date("2020-01-01"), by = "day", length.out = 5),
  High  = c(10.5, 11.0, 10.8, 11.2, 11.5),
  Low   = c(9.5,  10.0, 9.8,  10.2, 10.5),
  Close = c(10.0, 10.5, 10.2, 10.8, 11.0)
)

num_df <- data.frame(
  x = 1:5,
  y = c(2.1, 3.2, 4.3, 5.4, 6.5)
)

na_df <- data.frame(
  x = 1:5,
  y = c(1.0, NA, 3.0, NA, 5.0)
)

# ---- wb_data: cache is stored on series -------------------------------------

test_that("wb_data stores data_cache and cat_cache on series", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("stockChart")
  ch$add_series(name = High, label = Date, data = wd)

  s <- ch$series_data[[1]]
  expect_false(is.null(s$data_cache))
  expect_false(is.null(s$cat_cache))
  expect_equal(length(s$data_cache), 5L)
  expect_equal(length(s$cat_cache),  5L)
})

test_that("wb_data data_cache contains correct numeric values", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("stockChart")
  ch$add_series(name = High, label = Date, data = wd)

  expect_equal(ch$series_data[[1]]$data_cache, stock_df$High)
})

test_that("wb_data cat_cache contains correct Date values", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("stockChart")
  ch$add_series(name = High, label = Date, data = wd)

  expect_equal(ch$series_data[[1]]$cat_cache, stock_df$Date)
})

test_that("non-wb_data add_series leaves data_cache and cat_cache NULL", {
  ch <- ec("line")
  ch$add_series(
    data   = "'Sheet1'!$B$2:$B$6",
    label  = "'Sheet1'!$A$2:$A$6",
    name = "'Sheet1'!$B$1"
  )
  expect_null(ch$series_data[[1]]$data_cache)
  expect_null(ch$series_data[[1]]$cat_cache)
})

# ---- cache XML emission: val ------------------------------------------------

test_that("data_cache emits c:numCache inside c:numRef for val", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  expect_match(x, "<c:numRef>")
  expect_match(x, "<c:numCache>")
  expect_match(x, "<c:ptCount")
  expect_match(x, "<c:pt ")
})

test_that("data_cache ptCount matches series length", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  expect_match(x, 'ptCount val="5"')
})

test_that("data_cache values are correctly serialised in XML", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  expect_match(x, ">10.5<")
  expect_match(x, ">11<|>11.0<")
})

test_that("data_cache numCache is child of numRef not val", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  # numCache must appear after <c:f> inside <c:numRef>, not directly in <c:val>
  # If cache were a child of val, we'd see </c:val> before <c:numCache>
  val_end   <- regexpr("</c:val>",    x)
  cache_pos <- regexpr("<c:numCache", x)
  expect_true(cache_pos < val_end)
})

# ---- cache XML emission: label (dates) ----------------------------------------

test_that("date cat_cache emits c:numRef (not strRef) for label", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  # label should use numRef for dates, not strRef
  expect_match(x, "<c:cat>.*<c:numRef>", perl = TRUE)
})

test_that("date cat_cache emits correct Excel serial numbers", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  # 2020-01-01 = Excel serial 43831
  expected_serial <- as.character(
    openxlsx2::convert_to_excel_date(data.frame(d = stock_df$Date[1]))[[1]]
  )
  expect_match(x, expected_serial)
})

test_that("date cat_cache formatCode is m/d/yy", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  expect_match(x, "formatCode.*mm/dd/yyyy.*formatCode")
})

# ---- missing values ---------------------------------------------------------

test_that("NA values in data_cache are omitted from pt nodes", {
  wb <- make_wb(na_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = y, label = x, data = wd)
  x_str <- xml_str(ch)

  # ptCount = 5 (total length including NAs)
  expect_match(x_str, 'ptCount val="5"')

  # Only 3 pt nodes (indices 0, 2, 4) — NAs at index 1 and 3 are skipped
  pt_matches <- gregexpr("<c:pt ", x_str)[[1]]
  # Filter to only the val cache pts (not label pts)
  expect_equal(length(pt_matches[pt_matches > 0]), 3L + length(na_df$x))
  # idx="1" and idx="3" should not appear in the val cache
  # (they may appear in label cache so check by value)
  expect_false(grepl(">NA<", x_str))
})

test_that("NA values in cat_cache are omitted from pt nodes", {
  na_cat_df <- data.frame(
    x = c(as.Date("2020-01-01"), NA, as.Date("2020-01-03")),
    y = c(1.0, 2.0, 3.0)
  )
  wb <- make_wb(na_cat_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = y, label = x, data = wd)
  x_str <- xml_str(ch)

  expect_false(grepl(">NA<", x_str))
})

# ---- cell reference strings are still correct --------------------------------

test_that("wb_data still produces correct absolute cell references", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("stockChart")
  ch$add_series(name = High, label = Date, data = wd)

  s <- ch$series_data[[1]]
  expect_match(s$data, "\\$B\\$")
  expect_match(s$label,  "\\$A\\$")
  expect_match(s$name, "\\$B\\$1")
})

test_that("wb_data cell references appear inside c:f nodes in XML", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("line")
  ch$add_series(name = High, label = Date, data = wd)
  x <- xml_str(ch)

  expect_match(x, "<c:f>")
  expect_match(x, "Sheet1")
})

# ---- multiple series: each gets its own cache --------------------------------

test_that("multiple wb_data series each get independent caches", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("stockChart")
  ch$add_series(name = High,  label = Date, data = wd)
  ch$add_series(name = Low,   label = Date, data = wd)
  ch$add_series(name = Close, label = Date, data = wd)

  expect_equal(ch$series_data[[1]]$data_cache, stock_df$High)
  expect_equal(ch$series_data[[2]]$data_cache, stock_df$Low)
  expect_equal(ch$series_data[[3]]$data_cache, stock_df$Close)

  # All label caches identical (same Date column)
  expect_equal(ch$series_data[[1]]$cat_cache, ch$series_data[[2]]$cat_cache)
})

test_that("stockChart with wb_data renders without error", {
  wb <- make_wb(stock_df)
  wd <- openxlsx2::wb_data(wb, sheet = "Sheet1")

  ch <- ec("stockChart")
  ch$add_series(name = High,  label = Date, data = wd)
  ch$add_series(name = Low,   label = Date, data = wd)
  ch$add_series(name = Close, label = Date, data = wd)
  ch$high_low_lines <- TRUE
  ch$drop_lines     <- TRUE

  expect_no_error(ch$render())
  x <- xml_str(ch)
  expect_match(x, "c:stockChart")
  expect_match(x, "c:hiLowLines")
  expect_match(x, "c:dropLines")
  expect_match(x, "c:numCache")
})
