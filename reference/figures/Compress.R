library(magick)

fls <- dir(path = "inst/img/highres/", pattern = ".png", full.names = TRUE)

for (fl in fls) {
  message(fl)
  path <- paste0("inst/img/", tools::file_path_sans_ext(basename(fl)), ".jpg")

  img <- magick::image_read(fl)
  img <- magick::image_convert(img, "png")
  magick::image_write(img, path, quality = 25)

  system(paste0("magick ", path, " -sampling-factor 4:2:0 -strip -quality 55 -interlace JPEG -colorspace RGB ", path))

  in_size <- file.size(fl)
  out_size <- file.size(path)
  print(out_size / in_size)
}
