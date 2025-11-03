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
#' @param image tomogram file to be denoised
#' @param output_dir optional directory to save the denoised images, otherwise
#' images will be saved in the data folder of the package
#'
#' @return Returns null, instead saves the denoised images.
#'
#' @examples
#' # Example 1:
#' # Running denoising on a sample tomogram
#'
#' image <- TS_001.133
#' cryoCompare::runDenoising(image, tempdir())
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

runDenoising <- function(image, output_dir = NULL) {
  message(sprintf("Reading image...\n"))
  ijtiff::display(image)

  message(sprintf("Running denoising algorithms...\n"))
  gaussian_denoise <- imager::isoblur(image, sigma = 1, gaussian = TRUE)
  plot(gaussian_denoise, main="Gaussian denoising, sigma = 1")

  median_denoise <- imager::medianblur(image, n = 5)
  plot(median_denoise, main="Median denoising, median = 5")

  blur_anisotropic_denoise <- imager::blur_anisotropic(image, amplitude = 1e4)
  plot(blur_anisotropic_denoise, main="Anisotropic filter denoising,
       amplitude = 1e4")

  results <- list(
    original = image,
    gaussian = gaussian_denoise,
    median = median_denoise,
    anisotropic = blur_anisotropic_denoise
  )

  if(is.null(output_dir)) {
     output_dir <- file.path("data")
  }

  # Define output file paths
  gaussian_path <- file.path(output_dir, "gaussian.tif")
  median_path <- file.path(output_dir, "median.tif")
  anisotropic_path <- file.path(output_dir, "anisotropic.tif")

  # Save denoised images
  gaussian_image <- gaussian_denoise %>%
    as.array() %>%
    { as.integer(.) } %>%
    { dim(.) <- dim(as.array(image)); . }

  median_image <- median_denoise %>%
    as.array() %>%
    { as.integer(.) } %>%
    { dim(.) <- dim(as.array(image)); . }

  blur_anisotropic_image <- blur_anisotropic_denoise %>%
    as.array() %>%
    { as.integer(.) } %>%
    { dim(.) <- dim(as.array(image)); . }

  ijtiff::write_tif(gaussian_image, gaussian_path,
                    bits_per_sample = 8, overwrite = TRUE)
  ijtiff::write_tif(median_image, median_path,
                    bits_per_sample = 8, overwrite = TRUE)
  ijtiff::write_tif(blur_anisotropic_image, anisotropic_path,
                    bits_per_sample = 8, overwrite = TRUE)

  message(sprintf("Denoising completed.\n"))
  return()
}

