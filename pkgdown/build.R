build_pkgdown_site <- function(pkg = ".") {
  pkg         <- normalizePath(pkg)
  readme_path <- file.path(pkg, "README.md")
  fig_dir     <- file.path(pkg, "man", "figures")
  highres     <- file.path(pkg, "inst", "img", "highres")

  for (cand in c("index.md", file.path("pkgdown", "index.md"))) {
    if (file.exists(file.path(pkg, cand)))
      stop(cand, " exists; pkgdown will use it instead of README.md.")
  }

  if (!file.exists(readme_path)) stop("README.md not found at ", readme_path)
  if (!dir.exists(highres))      stop("inst/img/highres not found at ", highres)

  imgs <- list.files(highres, pattern = "\\.(png|jpe?g|gif|svg|webp)$",
                     ignore.case = TRUE, full.names = TRUE)
  if (!length(imgs)) stop("no image files found in ", highres)

  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(imgs, fig_dir, overwrite = TRUE)
  if (!all(ok)) warning("some highres images failed to copy")
  message("copied ", sum(ok), " image(s) to man/figures/")

  ext_by_stem <- setNames(tools::file_ext(imgs),
                          tools::file_path_sans_ext(basename(imgs)))

  original <- readLines(readme_path, warn = FALSE)
  patched  <- original
  for (stem in names(ext_by_stem)) {
    new_ext <- ext_by_stem[[stem]]
    rx <- paste0("inst/img/", stem, "\\.[A-Za-z0-9]+")
    patched <- gsub(rx, paste0("man/figures/", stem, ".", new_ext), patched)
  }

  n_changed <- sum(original != patched)
  if (n_changed == 0) warning("no image paths rewritten in README.md")
  else                message("patched ", n_changed, " line(s) in README.md")

  writeLines(patched, readme_path)

  on.exit({
    writeLines(original, readme_path)
    unlink(file.path(fig_dir, basename(imgs)))
    message("restored README.md and cleaned man/figures/")
  })

  pkgdown::build_site_github_pages(pkg, new_process = FALSE, install = FALSE)

  invisible(NULL)
}
