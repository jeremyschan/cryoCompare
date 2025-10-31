#' Apply Denoising Algorithms To Tomograms
#'
#' This function performs image denoising on tomograms (in .tif format) using
#' several filtering algorithms implemented in the imager package. The supported
#' denoising algorithms include Gaussian blur, median filter, and anisotropic
#' diffusion filtering. The function reads and displays the input image, applies
#' the denoising algorithms, and visualises the results for comparison. This is
#' meant to assist the user in evaluating the effectiveness of different
#' denoising algorithms before passing them into segmentation.
#'
#' @param image_path path to the tomogram file in .tif format
#'
#' @return Returns null, instead saves the denoised images.
#'
#' @examples
#' # Example 1:
#' # Running denoising on a sample tomogram
#' image_path <- system.file("inst", "extdata", "TS_001.133.tif", package = "cryoCompare")
#' denoised_results <- cryoCompare::runDenoising(image_path)
#'
#' @references
#' https://cran.r-project.org/web/packages/ijtiff/index.html
#' https://cran.r-project.org/web/packages/imager/index.html
#' https://pubmed.ncbi.nlm.nih.gov/6102765/
#' https://www.sid.ir/paper/544308/en
#' https://www.mia.uni-saarland.de/weickert/Papers/book.pdf
#'
#' @export
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

  results <- list(
    original = img,
    gaussian = gaussian_denoise,
    median = median_denoise,
    anisotropic = blur_anisotropic_denoise
  )

  output_dir <- file.path("inst", "extdata")

  # Define output file paths
  base_name <- tools::file_path_sans_ext(basename(image_path))
  gaussian_path <- file.path(output_dir, paste0(base_name, "_gaussian.tif"))
  median_path <- file.path(output_dir, paste0(base_name, "_median.tif"))
  anisotropic_path <- file.path(output_dir, paste0(base_name, "_anisotropic.tif"))

  # Save denoised images
  ijtiff::write_tif(gaussian_denoise, gaussian_path)
  ijtiff::write_tif(median_denoise, median_path)
  ijtiff::write_tif(blur_anisotropic_denoise, anisotropic_path)

  message(sprintf("Denoising completed.\n"))
}
