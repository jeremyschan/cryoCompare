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
#' images will be saved in the inst/extdata folder of the package
#' @param gaussian_sigma optional standard deviation for Gaussian blur, default
#' set to 1
#' @param median_size optional window size of the median filter, default set
#' to 5
#'
#' @return A list containing the original image and the denoised images.
#'
#' @examples
#' # Example 1:
#' # Running denoising on a sample tomogram
#'
#' data("TS_001.133", package = "cryoCompare")
#' image <- TS_001.133
#' cryoCompare::runDenoising(image, tempdir())
#'
#' @references
#' Pau, G., Fuchs, F., Sklyar, O., Boutros, M., & Huber W. (2010).
#' EBImage - an R package for image processing with applications to
#' cellular phenotypes. Bioinformatics, 26(7), 979-981.
#' https://doi.org/10.1093/bioinformatics/btq046
#' Huang, T., Yang, G., & Tang, G. (1979). A fast two-dimensional median
#' filtering algorithm. IEEE Transactions on Acoustics, Speech, and Signal
#' Processing, 27(1), 13-18. https://doi.org/10.1109/TASSP.1979.1163188
#' Marr, D., & Hildreth, E. (1980). Theory of edge detection. Proceedings of
#' the Royal Society B: Biological Sciences, 207(1167): 187-217.
#' https://doi.org/10.1098/rspb.1980.0020
#' Nolan, R., & Padilla-Parra, S. (2018). ijtiff: An R package providing TIFF
#' I/O for ImageJ users. Journal of Open Source Software, 3(23), 633.
#' https://doi.org/10.21105/joss.00633
#'
#' @export
#' @importFrom ijtiff display as_EBImage write_tif
#' @importFrom EBImage gblur medianFilter is.Image
#' @import ggplot2
#' @import dplyr

runDenoising <- function(image, output_dir = NULL,
                         gaussian_sigma = 1, median_size = 3) {
  # Input validation
  if (!EBImage::is.Image(image)) {
    image <- ijtiff::as_EBImage(image)
  }

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
  }

  if (!is.numeric(gaussian_sigma) || gaussian_sigma <= 0) {
    stop("gaussian_sigma must be a positive integer.")
  }

  if (!is.numeric(median_size) || median_size <= 0) {
    stop("median_size must be a positive integer.")
  }

  # Run denoising algorithms and plot them out
  message(sprintf("Reading image...\n"))
  ijtiff::display(image)
  title("Original tomogram")

  message(sprintf("Running denoising algorithms...\n"))
  gaussian_denoise <- EBImage::gblur(image, sigma = gaussian_sigma)
  ijtiff::display(gaussian_denoise)
  title(paste("Gaussian denoising, sigma = ", gaussian_sigma))

  median_denoise <- EBImage::medianFilter(image, size = median_size)
  ijtiff::display(median_denoise)
  title(paste("Median denoising, size = ", median_size))

  # Store denoised images in a list for return
  results <- list(
    original = image,
    gaussian = gaussian_denoise,
    median = median_denoise
  )

  # Set default output directory if not provided
  if(is.null(output_dir)) {
     output_dir <- file.path("inst/extdata")
  }

  # Define output file paths
  gaussian_path <- file.path(output_dir, "gaussian.tif")
  median_path <- file.path(output_dir, "median.tif")

  # Save denoised images
  gaussian_image <- gaussian_denoise %>%
    as.array() %>%
    { . * 100 } %>%
    { as.integer(.) } %>%
    { dim(.) <- dim(as.array(image)); . }
  gaussian_image <- aperm(gaussian_image, c(2, 1, 3))

  median_image <- median_denoise %>%
    as.array() %>%
    { . * 100 } %>%
    { as.integer(.) } %>%
    { dim(.) <- dim(as.array(image)); . }
  median_image <- aperm(median_image, c(2, 1, 3))

  ijtiff::write_tif(gaussian_image, gaussian_path,
                    bits_per_sample = 8, overwrite = TRUE)
  ijtiff::write_tif(median_image, median_path,
                    bits_per_sample = 8, overwrite = TRUE)

  message(sprintf("Denoising completed.\n"))
  return(results)
}

# [END]
