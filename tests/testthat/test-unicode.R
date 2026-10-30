chart_xml_bytes <- function(fl) {
  td <- tempfile()
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(fl, exdir = td)
  ch <- list.files(file.path(td, "xl", "charts"), pattern = "^chart[0-9]+\\.xml$", full.names = TRUE)
  readBin(ch[1], "raw", file.size(ch[1]))
}

has_bytes <- function(haystack, needle) {
  length(grepRaw(charToRaw(enc2utf8(needle)), haystack, fixed = TRUE)) > 0
}

test_that("pugi helpers return non-ASCII text as UTF-8", {
  txt <- "Ums\u00e4tze M\u00fcller \u20ac"
  nam <- "Gr\u00f6\u00dfe"

  doc <- openxlsx2::read_xml("<root/>")
  node <- xml_add_child(doc, "item", name = nam, .value = txt)

  expect_identical(xml_text(node), txt)
  expect_identical(Encoding(xml_text(node)), "UTF-8")
  expect_identical(xml_attr(node, "name"), nam)
  expect_identical(Encoding(xml_attr(node, "name")), "UTF-8")

  xml_set_attr(node, "label", txt)
  expect_identical(xml_attr(node, "label"), txt)
  expect_true(xml_has_attr(node, "label"))

  hit <- xml_find_first(doc, sprintf("//item[@name='%s']", nam))
  expect_identical(xml_text(hit), txt)
  expect_length(xml_find_all(doc, sprintf("//item[@label='%s']", txt)), 1)

  ser <- as.character(doc)
  expect_identical(Encoding(ser), "UTF-8")
  expect_true(has_bytes(charToRaw(ser), txt))
})

test_that("pugi helpers translate latin1 input to UTF-8", {
  txt <- iconv("Ums\u00e4tze M\u00fcller", "UTF-8", "latin1")
  xpath <- iconv("//item[@name='Ums\u00e4tze M\u00fcller']", "UTF-8", "latin1")
  expect_identical(Encoding(txt), "latin1")
  expect_identical(Encoding(xpath), "latin1")

  doc <- openxlsx2::read_xml("<root/>")
  node <- xml_add_child(doc, "item", name = txt, .value = txt)

  expect_identical(charToRaw(xml_text(node)), charToRaw(enc2utf8(txt)))
  expect_identical(charToRaw(xml_attr(node, "name")), charToRaw(enc2utf8(txt)))
  expect_length(xml_find_all(doc, xpath), 1)
  expect_true(validUTF8(as.character(doc)))
})

test_that("non-ASCII chart titles are written as UTF-8 and load back", {
  titles <- list(
    utf8   = "Ums\u00e4tze M\u00fcller \u20ac",
    latin1 = iconv("Gr\u00f6\u00dfe in \u00b0C", "UTF-8", "latin1")
  )
  axis <- "Monat \u00e0 \u00e9t\u00e9"

  for (title in titles) {
    wb <- openxlsx2::wb_workbook()$add_worksheet("Data")$add_data(
      x = data.frame(Month = month.abb[1:6], Sales = c(120, 135, 128, 160, 175, 190))
    )
    chart <- ec("bar")$
      set_chart_title(title)$
      set_x_title(axis)$
      add_series(name = "Data!$B$1", data = "Data!$B$2:$B$7", label = "Data!$A$2:$A$7")
    wb$add_encharter(dims = "D2:K18", graph = chart)

    stored <- wb$charts$chart[[1]]
    expect_true(validUTF8(stored))
    expect_true(has_bytes(charToRaw(stored), title))

    fl <- tempfile(fileext = ".xlsx")
    wb$save(fl)
    xml <- chart_xml_bytes(fl)
    expect_true(validUTF8(rawToChar(xml)))
    expect_true(has_bytes(xml, title))
    expect_true(has_bytes(xml, axis))

    loaded <- ec_load(openxlsx2::wb_load(fl))
    expect_identical(enc2utf8(loaded$chart_title$text), enc2utf8(title))
    expect_identical(enc2utf8(loaded$x_title$text), axis)
    unlink(fl)
  }
})

test_that("non-ASCII titles survive a chart template", {
  skip_if(Sys.which("zip") == "")
  title <- iconv("Ums\u00e4tze M\u00fcller", "UTF-8", "latin1")
  chart <- ec("bar")$set_chart_title(title)$add_series(data = "Data!$B$2:$B$7")

  tmp <- tempfile(fileext = ".crtx")
  ec_to_crtx(chart, tmp)
  td <- tempfile()
  utils::unzip(tmp, files = "chart/chart.xml", exdir = td)
  fl <- file.path(td, "chart", "chart.xml")
  xml <- readBin(fl, "raw", file.size(fl))
  unlink(c(tmp, td), recursive = TRUE)

  expect_true(validUTF8(rawToChar(xml)))
  expect_true(has_bytes(xml, title))
})
