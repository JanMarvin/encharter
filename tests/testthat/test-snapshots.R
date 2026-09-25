# Visual snapshots of the reference charts. Their rendering was compared
# with Excel, so a changed snapshot means a changed picture: review with
# testthat::snapshot_review() and accept only intended changes.

snapshot_set <- function(prefix, set) {
  skip_if_not_installed("vdiffr")
  r <- set()
  for (i in seq_along(r$charts)) {
    chart <- r$charts[[i]]
    title <- tolower(gsub("[^A-Za-z0-9]+", "-", sub("^[0-9]+ ", "", chart$chart_title$text)))
    vdiffr::expect_doppelganger(sprintf("%s-%02d-%s", prefix, i, title), function() plot(chart, wb = r$wb))
  }
}

test_that("standard charts render as before", {
  snapshot_set("standard", reference_standard)
})
test_that("extended charts render as before", {
  snapshot_set("chartex", reference_chartex)
})
test_that("stock, of-pie, 3D and surface charts render as before", {
  snapshot_set("threed", reference_threed)
})
test_that("chart options render as before", {
  snapshot_set("options", reference_options)
})
