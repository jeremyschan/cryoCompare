#' Run Complete Denoising and Segmentation Pipeline
#'
#' This function first applies several denoising filters to a tomogram in .tif
#' format, then prompts the user to select one of the denoised images (or the
#' original) for segmentation. It then runs the specified segmentation
#' algorithms on the chosen image. This function integrates the denoising and
#' segmentation steps into a single pipeline for ease of use.
#'
#' @param image_path path to the tomogram file in .tif format
#' @param methods vector of strings of segmentation methods to apply
#' @param ground_truth_image_path optional path to the ground truth tomogram
#' file in .tif format
#'
#' @returns Returns null as this is the end of the pipeline
#'
#' @examples
#' # Example 1:
#' # Running the complete pipeline on a sample tomogram with Otsu and
#' # Triangle segmentation methods with ground truth
#' image_path <- system.file("inst", "extdata", "TS_001.133.tif",
#'                           package = "cryoCompare")
#' ground_truth_image_path <- system.file("inst", "extdata",
#'                                       "TS_001.133_ground_truth.tif",
#'                                       package = "cryoCompare")
#' cryoCompare::runPipeline(image_path,
#'                         methods = c("Otsu", "Triangle"),
#'                         ground_truth_image_path)
#'
#' # Example 2:
#' # Running the complete pipeline on a sample tomogram with all segmentation
#' # methods without ground truth
#' image_path <- system.file("inst", "extdata", "TS_001.133.tif",
#'                          package = "cryoCompare")
#' cryoCompare::runPipeline(image_path,
#'                         methods = c("Huang", "Mean", "Otsu", "Triangle"))
#' @export

runPipeline <- function(image_path,
                        methods,
                        ground_truth_image_path = NULL) {
  runDenoising(image_path)

  message(sprintf("Choose an image for segmentation.\n"))
  choice <- readline("Enter 'original', 'gaussian', 'median', or 'anisotropic': ")

  if (choice == 'gaussian') {
    image_path <- sub(".tif", "_gaussian.tif", image_path)
  } else if (choice == 'median') {
    image_path <- sub(".tif", "_median.tif", image_path)
  } else if (choice == 'anisotropic') {
    image_path <- sub(".tif", "_anisotropic.tif", image_path)
  }

  if (!is.null(ground_truth_image_path)) {
    runSegmentation(image_path, methods, ground_truth_image_path)
  } else {
    runSegmentation(image_path, methods)
  }

  return()
}
