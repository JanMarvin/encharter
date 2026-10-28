test_that("to_abs_ref() works", {
  expect_equal(to_abs_ref("Sheet1"), "Sheet1")
  expect_equal(to_abs_ref("Sheet1!"), "'Sheet1'!NA") # this is broken
  expect_equal(to_abs_ref("Sheet1!A2:A10"), "'Sheet1'!$A$2:$A$10")
  expect_equal(to_abs_ref("'Sheet 1'!A2:A10"), "'Sheet 1'!$A$2:$A$10")
})

test_that("normalize_encharter_type() works", {
  expect_equal(normalize_encharter_type("barChart"), "barChart")
  expect_equal(normalize_encharter_type("barplot"), "barChart")
  expect_equal(normalize_encharter_type(NULL), NULL)
})

test_that("normalize_encharter_string() works", {
  expect_equal(normalize_encharter_string("outEnd"), "outEnd")
  expect_equal(normalize_encharter_string("left"), "l")
  expect_equal(normalize_encharter_string(NULL), NULL)
})

test_that("to_abs_ref() quotes sheet names with apostrophes and '!'", {
  expect_equal(to_abs_ref("O&apos;Brien &amp; Co!$B$1"), "'O&apos;&apos;Brien &amp; Co'!$B$1")
  expect_equal(to_abs_ref("'O''Brien & Co'!B1"), "'O&apos;&apos;Brien &amp; Co'!$B$1")
  expect_equal(to_abs_ref("'O&apos;&apos;Brien &amp; Co'!$B$1"), "'O&apos;&apos;Brien &amp; Co'!$B$1")
  expect_equal(to_abs_ref("a!b!A1"), "'a!b'!$A$1")
  expect_equal(to_abs_ref("'a!b'!A1"), "'a!b'!$A$1")
  x <- to_abs_ref("O&apos;Brien &amp; Co!$B$1")
  expect_equal(to_abs_ref(x), x)
})

test_that("fmt_txt2() keeps fractional font sizes", {
  expect_match(fmt_txt2(openxlsx2::fmt_txt("abc", size = 10.5)), 'sz="1050"', fixed = TRUE)
  expect_match(fmt_txt2(openxlsx2::fmt_txt("abc", size = 12)), 'sz="1200"', fixed = TRUE)

  ch <- ec("bar")$set_chart_title(openxlsx2::fmt_txt("T", size = 10.5))
  ch$add_series(name = "S!$B$1", data = "S!$B$2:$B$4")
  expect_match(as.character(ch$render()), 'sz="1050"', fixed = TRUE)
})
