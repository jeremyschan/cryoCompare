#' @import imager
#' @import ggplot2
#' @import dplyr
#' @import ijtiff

runDenoising <- function(image_path) {
  message(sprintf("Reading image %s...\n", image_path))
  img <- ijtiff::read_tif(image_path)
  ijtiff::display(img)

  message(sprintf("Running denoising algorithms on %s...\n", image_path))
  gaussian_denoise <- imager::isoblur(img, sigma = 1, gaussian = TRUE) %>%
    plot(main="Gaussian denoising, sigma = 1")

  median_denoise <- imager::medianblur(img, n = 5) %>%
    plot(main="Median denoising, median = 5")

  blur_anisotropic_denoise <- imager::blur_anisotropic(img, amplitude = 1e4) %>%
    plot(main="Anisotropic filter denoising, amplitude = 1e4")
}
