#' Run Complete Denoising and Segmentation Pipeline
#'
#' This function first applies several denoising filters to a tomogram in .tif
#' format, then prompts the user to select one of the denoised images (or the
#' original) for segmentation. It then runs the specified segmentation
#' algorithms on the chosen image. This function integrates the denoising and
#' segmentation steps into a single pipeline for ease of use.
#'
#' @param image tomogram file to be run through the pipeline
#' @param methods vector of strings of segmentation methods to apply
#' @param output_dir optional directory to save the denoised images, otherwise
#' images will be saved in the data folder of the package
#' @param ground_truth optional ground truth mask of tomogram file
#'
#' @examples
#' # No examples: this function requires user input
#'
#' @returns Returns null as this is the end of the pipeline
#' @export

runPipeline <- function(image,
                        methods,
                        output_dir = NULL,
                        ground_truth = NULL) {
  runDenoising(image, output_dir)

  message(sprintf("Choose an image for segmentation.\n"))
  choice <- readline("Enter 'original', 'gaussian', 'median', or 'anisotropic': ")

  if (is.null(output_dir)) {
    base_dir <- "data"
  } else {
    base_dir <- output_dir
  }

  if (choice == "gaussian") {
    image_path <- file.path(base_dir, "gaussian.tif")
  } else if (choice == "median") {
    image_path <- file.path(base_dir, "median.tif")
  } else if (choice == "anisotropic") {
    image_path <- file.path(base_dir, "anisotropic.tif")
  }

  if (choice != "original") {
    chosen_image <- ijtiff::read_tif(image_path)
  } else {
    chosen_image <- image
  }

  if (!is.null(ground_truth)) {
    runSegmentation(chosen_image, methods, ground_truth)
  } else {
    runSegmentation(chosen_image, methods)
  }

  return(invisible(NULL))
}

# [END]
