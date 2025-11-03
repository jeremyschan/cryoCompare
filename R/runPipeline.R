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
#' @param ground_truth optional ground truth mask of tomogram file
#'
#' @returns Returns null as this is the end of the pipeline
#'
#' @examples
#' # Example 1:
#' # Running the complete pipeline on a sample tomogram with Otsu and
#' # Triangle segmentation methods with ground truth
#' image <- TS_001.133
#' ground_truth <- TS_001.133_ground_truth
#' cryoCompare::runPipeline(image,
#'                         methods = c("Otsu", "Triangle"),
#'                         ground_truth)
#'
#' # Example 2:
#' # Running the complete pipeline on a sample tomogram with all segmentation
#' # methods without ground truth
#' image <- TS_001.133
#' cryoCompare::runPipeline(image,
#'                         methods = c("Huang", "Mean", "Otsu", "Triangle"))
#' @export

runPipeline <- function(image,
                        methods,
                        ground_truth = NULL) {
  runDenoising(image)

  message(sprintf("Choose an image for segmentation.\n"))
  choice <- readline("Enter 'original', 'gaussian', 'median', or 'anisotropic': ")

  if (choice == 'gaussian') {
    image_path <- "/data/gaussian.tif"
  } else if (choice == 'median') {
    image_path <- "/data/median.tif"
  } else if (choice == 'anisotropic') {
    image_path <- "/data/anisotropic.tif"
  }

  if (choice != 'original') {
    chosen_image <- ijtiff::read_tif(image_path)
  } else {
    chosen_image <- image
  }

  if (!is.null(ground_truth)) {
    runSegmentation(chosen_image, methods, ground_truth)
  } else {
    runSegmentation(chosen_image, methods)
  }

  return()
}
