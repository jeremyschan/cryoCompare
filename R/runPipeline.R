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
  # Input validation
  if (!inherits(image, "ijtiff_img")) {
    stop("Input image must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif().")
  }

  for (method in methods) {
    if (!(method %in% available_methods)) {
      stop("Invalid segmentation method. Choose from: ",
           paste(available_methods, collapse = ", "))
    }
  }

  if (!file.exists(output_dir)) {
    dir.create(output_dir)
  }

  if (!is.null(ground_truth) && !inherits(ground_truth, "ijtiff_img")) {
    stop("Ground truth mask must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif().")
  }

  # Begin pipeline
  runDenoising(image, output_dir)

  message(sprintf("Choose an image for segmentation.\n"))
  valid <- c("original", "gaussian", "median")
  choice <- readline("Enter 'original', 'gaussian', or 'median': ")
  while (!(choice %in% valid)) {
    choice <- readline("Invalid choice. Please enter 'original', 'gaussian', or 'median': ")
  }

  if (is.null(output_dir)) {
    base_dir <- "data"
  } else {
    base_dir <- output_dir
  }

  if (choice == "gaussian") {
    image_path <- file.path(base_dir, "gaussian.tif")
  } else if (choice == "median") {
    image_path <- file.path(base_dir, "median.tif")
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
