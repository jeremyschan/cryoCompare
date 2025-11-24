#' Run Segmentation Pipeline On Tomograms
#'
#' This function will run segmentation algorithms on the specified tomograms
#' (must be in .tif format). It is preferred that the tomograms are already
#' denoised, as segmentation algorithms typically perform better on denoised
#' data. The function supports multiple segmentation algorithms, including
#' various traditional image thresholding methods including Huang
#' (Huang & Wang, 1995), Mean (Glasbey, 1993), Otsu (1979), and Triangle
#' (Zack et al., 1977). This function will read the tiff tomogram file,
#' then send the results to other functions to apply the specified segmentation
#' algorithms, visualise the results, and compare them against ground truth
#' if provided.
#'
#' @param image tomogram file to be run through the pipeline
#' @param methods vector of strings of segmentation methods to apply; choose
#' from "Huang", "Mean", "Otsu", and "Triangle"
#' @param ground_truth optional ground truth mask of tomogram file
#'
#' @return Returns a data frame containing the DICE similarity coefficient for
#' each segmentation method compared to the ground truth, if provided. Otherwise,
#' returns NULL.
#'
#' @examples
#' # Example 1:
#' # Segmentation of a tomogram with Otsu and Triangle algorithms without ground truth
#'
#' data("TS_001.133", package = "cryoCompare")
#' image <- TS_001.133
#' cryoCompare::runSegmentation(image, methods = c("Otsu", "Triangle"))
#'
#' # Example 2:
#' # Segmentation of a tomogram with all available methods with ground truth
#' data("TS_001.133", package = "cryoCompare")
#' data("TS_001.133_ground_truth", package = "cryoCompare")
#' image <- TS_001.133
#' ground_truth <- TS_001.133_ground_truth
#' cryoCompare::runSegmentation(image,
#' methods = c("Huang", "Mean", "Otsu", "Triangle"), ground_truth)
#'
#' @references
#' Glasbey, C. A. (1993). An analysis of histogram-based thresholding
#' algorithms. CVGIP: Graphical Models and Image Processing, 55(6), 532-537.
#' https://doi.org/10.1006/cgip.1993.1040
#' Huang, L., & Wang, M. J. (1995). Image thresholding by minimizing the
#' measures of fuzziness. Pattern Recognition, 28(1), 41-51.
#' https://doi.org/10.1016/0031-3203(94)E0043-K.
#' Nolan, R., & Padilla-Parra, S. (2018). ijtiff: An R package providing TIFF
#' I/O for ImageJ users. Journal of Open Source Software, 3(23), 633.
#' https://doi.org/10.21105/joss.00633
#' Otsu, N. (1979). A threshold selection method from gray-level histograms.
#' IEEE Transactions on Systems, Man, and Cybernetics, 9(1), 62-66.
#' https://doi.org/10.1109/TSMC.1979.4310076
#' Zack, G. W., Rogers, W. E., & Latt, S. A. (1977). Automatic measurement of
#' sister chromatid exchange frequency. Journal of Histochemistry &
#' Cytochemistry, 25(7), 741-753. https://doi.org/10.1177/25.7.70454
#'
#' @export
#' @import ijtiff

runSegmentation <- function(image,
                            methods,
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

  if (!is.null(ground_truth) && !inherits(ground_truth, "ijtiff_img")) {
    stop("Ground truth mask must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif().")
  }

  # Begin segmentation pipeline
  message(sprintf("Running segmentation algorithms...\n"))
  seg_result <- thresholdSegmentation(image, methods)
  message(sprintf("Segmentation completed.\n"))

  message(sprintf("Visualising segmentation results...\n"))
  viewSegmentation(image, methods)
  message(sprintf("Visualisation completed.\n"))

  result <- "Segmentation completed. No ground truth provided for comparison."

  # Only run ground truth comparison when provided
  if (!is.null(ground_truth)) {
    message(sprintf("Comparing against ground truth...\n"))
    gt_comparison <- .compareGroundTruth(seg_result, ground_truth)
    print(gt_comparison$p)
    message(sprintf("Comparison completed.\n"))
    result <- gt_comparison$dice_score
  }

  return(result)
}


#' Apply Thresholding Segmentation Methods
#'
#' This function applies specified traditional image thresholding algorithms to
#' a tomogram to generate binary masks for segmentation. The function takes
#' in an already read image, computes the individual masks for each method, and
#' returns them along with their respective threshold values for further
#' analysis. While this is a helper function for the main segmentation pipeline,
#' it can also be used independently for obtaining masks and thresholding values
#' for further analysis.
#'
#' @param image tomogram image read into R
#' @param methods vector of strings of segmentation methods to apply
#'
#' @return Returns a list containing the mask and threshold values for each
#' method
#'
#' @examples
#' # Example 1:
#' # Extracting the segmentation masks and thresholds using Otsu and Triangle methods
#' data("TS_001.133", package = "cryoCompare")
#' image <- TS_001.133
#' seg_result <- cryoCompare::thresholdSegmentation(image, methods = c("Otsu", "Triangle"))
#'
#' @references
#' Landini, G., Randell, D. A., Fouad, S., & Galton, A. (2017). Automatic
#' thresholding from the gradients of region boundaries. Journal of Microscopy,
#' 265(2), 185-195. https://doi.org/10.1111/jmi.12474
#' Nolan, R., & Padilla-Parra, S. (2018). ijtiff: An R package providing TIFF
#' I/O for ImageJ users. Journal of Open Source Software, 3(23), 633.
#' https://doi.org/10.21105/joss.00633
#'
#' @export
#' @import autothresholdr
#' @import ijtiff

thresholdSegmentation <- function(image, methods) {
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

  # Set names and lengths of thresholds and masks according to the methods
  thresholds <- stats::setNames(numeric(length(methods)), methods)
  masks <- vector("list", length(methods))
  names(masks) <- methods

  # Run auto-thresholding with each method and store results
  for (m in methods) {
    message(sprintf("Running segmentation method: %s...\n", m))
    thr <- autothresholdr::auto_thresh(int_arr = image, method = m)
    thresholds[m] <- thr
    masks[[m]] <- image >= thr
  }

  return(list(thresholds = thresholds, masks = masks))
}


#' Visualise Segmentation Results
#'
#' This function will display the mask from a specific thresholding algorithm
#' applied to a given tomogram, allowing users to visually inspect what each
#' individual segmentation method is picking up. It is primarily used within
#' the segmentation pipeline to visualise the results of different segmentation
#' methods but can also be used independently for visual analysis.
#'
#' @param image tomogram image read into R
#' @param methods vector of strings of segmentation methods to visualise
#'
#' @return Returns null as this function is used for visualisation only.
#'
#' @examples
#' #' # Example 1:
#' # Visualising segmentation results using Otsu and Triangle methods
#' data("TS_001.133", package = "cryoCompare")
#' image <- TS_001.133
#' cryoCompare::viewSegmentation(image, methods = c("Otsu", "Triangle"))
#'
#' @references
#' Landini, G., Randell, D. A., Fouad, S., & Galton, A. (2017). Automatic
#' thresholding from the gradients of region boundaries. Journal of Microscopy,
#' 265(2), 185-195. https://doi.org/10.1111/jmi.12474
#' Nolan, R., & Padilla-Parra, S. (2018). ijtiff: An R package providing TIFF
#' I/O for ImageJ users. Journal of Open Source Software, 3(23), 633.
#' https://doi.org/10.21105/joss.00633
#'
#' @export
#' @import ijtiff
#' @import autothresholdr

viewSegmentation <- function(image, methods) {
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

  # Display segmentation masks on the image for each method
  for (m in methods) {
    ijtiff::display(autothresholdr::apply_mask(image, m))
    title(paste("Segmentation using", m))
  }

  return(invisible(NULL))
}


#' Compare Segmentation Results Against Ground Truth
#'
#' This function compares the segmentation output from multiple thresholding
#' methods against a provided ground truth mask (also in .tif format),
#' using the DICE similarity coefficient as a measure of accuracy. As
#' the segmentation output is required for comparison, this function is
#' private and only used within the segmentation pipeline.
#'
#' @param seg_result list containing segmentation masks from different methods
#' @param ground_truth ground truth tomogram image read into R
#'
#' @return Returns a ggplot2 bar plot visualising the accuracy of each
#' segmentation method compared to the ground truth
#'
#' @references
#' Dice, L. R. (1945). Measures of the amount of ecological association between
#' species. Ecology, 26(3), 297-302. https://doi.org/10.2307/1932409
#' Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
#' Springer-Verlag New York. https://ggplot2.tidyverse.org
#'
#' @import ggplot2

.compareGroundTruth <- function(seg_result, ground_truth) {
  # Function to calculate DICE similarity coefficient
  dice <- function(a, b) {
    # Binarize the masks
    a <- a != 0
    b <- b != 0

    # Calculate the number of overlapping pixels, and total number of pixels
    intersection <- sum(a & b)
    denominator <- sum(a) + sum(b)

    # Calculate DICE coefficient
    return(2 * intersection / denominator)
  }

  # Calculate accuracy for each method
  methods <- names(seg_result$masks)
  acc <- vapply(
    methods,
    function(m) dice(seg_result$masks[[m]], ground_truth) * 100,
    numeric(1)
  )

  # Create a data frame for plotting with ggplot
  dice_score <- data.frame(
    method = methods,
    accuracy_percent = as.numeric(acc),
    stringsAsFactors = FALSE
  )

  # Generate bar plot using ggplot2
  p <- ggplot2::ggplot(dice_score, ggplot2::aes(x = stats::reorder(methods, accuracy_percent),
                                   y = accuracy_percent, fill = method)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(accuracy_percent, 1), "%")),
        hjust = -0.1,
        size = 3.5,
        color = "black"
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"), limits = c(0,100)) +
      ggplot2::labs(title = "Segmentation accuracy by method",
                    x = "Method",
                    y = "Accuracy (%)") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")

  return(list(dice_score = dice_score, p = p))
}

# [END]
