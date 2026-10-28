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
