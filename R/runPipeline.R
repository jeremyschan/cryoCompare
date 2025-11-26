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
#' @returns Returns a list containing: the segmentation results from each method,
#' and the ground truth comparison results (if provided).
#'
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

  available_methods <- c("Huang", "Mean", "Otsu", "Triangle")
  for (method in methods) {
    if (!(method %in% available_methods)) {
      stop("Invalid segmentation method. Choose from: ",
           paste(available_methods, collapse = ", "))
    }
  }

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
  }

  if (!is.null(ground_truth) && !inherits(ground_truth, "ijtiff_img")) {
    stop("Ground truth mask must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif().")
  }

  suppressWarnings({
    gaussian_choice <- read("Enter a value for Gaussian sigma: ")
    gaussian_choice <- as.numeric(gaussian_choice)
    while (is.na(gaussian_choice) || gaussian_choice <= 0) {
      gaussian_choice <- read("Invalid input. Please enter a positive number for Gaussian sigma: ")
      gaussian_choice <- as.numeric(gaussian_choice)
    }

    median_choice <- read("Enter a value for Median filter size: ")
    median_choice <- as.numeric(median_choice)
    while (is.na(median_choice) || median_choice <= 0) {
      median_choice <- read("Invalid input. Please enter a positive number for Median filter size: ")
      median_choice <- as.numeric(median_choice)
    }
  })

  # Begin pipeline
  runDenoising(image, output_dir, gaussian_choice, median_choice)

  # Allow users to choose which denoised image to segment
  message(sprintf("Choose an image for segmentation.\n"))
  valid <- c("original", "gaussian", "median")
  seg_choice <- read("Enter 'original', 'gaussian', or 'median': ")
  while (!(seg_choice %in% valid)) {
    seg_choice <- read("Invalid choice. Please enter 'original', 'gaussian', or 'median': ")
  }

  # Determine image path based on output directory, then read the image in
  if (is.null(output_dir)) {
    base_dir <- "inst/extdata"
  } else {
    base_dir <- output_dir
  }

  if (seg_choice == "gaussian") {
    image_path <- file.path(base_dir, "gaussian.tif")
  } else if (seg_choice == "median") {
    image_path <- file.path(base_dir, "median.tif")
  }

  if (seg_choice != "original") {
    chosen_image <- ijtiff::read_tif(image_path)
  } else {
    chosen_image <- image
  }

  # Run segmentation on chosen image, with or without ground truth
  if (!is.null(ground_truth)) {
    results <- runSegmentation(chosen_image, methods, ground_truth)
  } else {
    results <- runSegmentation(chosen_image, methods)
  }

  return(results)
}

# Wrap base::readline() for testing
read <- function(prompt = "") {
  return(base::readline(prompt))
}

# [END]
