exportPlot <- function(plot_obj, file_path, width = 12, height = 7, dpi = 160) {
  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("png", "jpg", "jpeg")) {
    stop("Export format must be png/jpg/jpeg.")
  }
  ggplot2::ggsave(
    filename = file_path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  invisible(file_path)
}
