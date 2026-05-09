run_all_examples <- function(dir = system.file("examples", package = "encharter")) {
  if (!nzchar(dir) || !dir.exists(dir))
    stop("examples directory not found: ", dir)

  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  if (!length(files))
    stop("no .R files found in ", dir)

  for (i in seq_along(files)) {
    f <- files[[i]]
    cat(sprintf("\n[%d/%d] %s\n", i, length(files), basename(f)))
    cat(strrep("-", 60), "\n", sep = "")

    err <- tryCatch(
      {
        source(f, local = TRUE)
        NULL
      },
      error = function(e) e
    )

    if (!is.null(err))
      cat("ERROR: ", conditionMessage(err), "\n", sep = "")

    if (i < length(files)) {
      ans <- readline("press enter for next, q to quit: ")
      if (identical(tolower(ans), "q")) break
    }
  }

  invisible(NULL)
}
