#' Run Segmentation Algorithms On Tomograms
#'
#' This function will run segmentation algorithms on the specified tomograms
#' (must be in .mrc format). It is preferred that the tomograms are already
#' denoised, as segmentation algorithms typically perform better on denoised
#' data. The function supports multiple segmentation algorithms, including
#' traditional thresholding and machine learning-based method, membrain-seg.
#'
#' To run this function, you will need to have the necessary dependencies
#' installed and the algorithm models should be downloaded from the repository
#' homepage.
#'
#' @param tomogram_path path to the tomogram file in .mrc format
#' @param model_path path to the pre-trained model for
#'  machine learning-based segmentation
#' @param output_path path to save the segmentation comparison results
#'
#' @return TO-DO
#' @export
#' @import ijtiff
#' @import autothresholdr

runSegmentation <- function(image_path, methods, ground_truth_image_path) {
  message(sprintf("Reading image %s...\n", image_path))
  img <- ijtiff::read_tif(image_path)

  available_methods <- c("Huang", "Mean", "Otsu", "Triangle")
  for (method in methods) {
    if (!(method %in% available_methods)) {
      stop("Invalid segmentation method. Choose from: ", paste(available_methods, collapse = ", "))
    }
  }

  message(sprintf("Running segmentation algorithms on %s...\n", image_path))
  seg_result <- thresholdSeg(img, methods)
  message(sprintf("Segmentation completed.\n"))

  message(sprintf("Visualising segmentation results...\n"))
  viewSegmentation(img, methods)
  message(sprintf("Visualisation completed.\n"))

  if (!is.null(ground_truth_image_path)) {
    message(sprintf("Comparing against ground truth...\n"))
    ground_truth_img <- ijtiff::read_tif(ground_truth_image_path)
    plot <- compareGroundTruth(seg_result, ground_truth_img)
    print(plot)
    message(sprintf("Comparison completed.\n"))
  }
}


thresholdSeg <- function(img, methods) {
  thresholds <- stats::setNames(numeric(length(methods)), methods)
  masks <- vector("list", length(methods))
  names(masks) <- methods

  for (m in methods) {
    message(sprintf("Running segmentation method: %s...\n", m))
    thr <- autothresholdr::auto_thresh(img, method = m)
    thresholds[m] <- thr
    masks[[m]] <- img >= thr
  }

  return(list(thresholds = thresholds, masks = masks))
}


viewSegmentation <- function(img, methods) {
  for (m in methods) {
    ijtiff::display(autothresholdr::apply_mask(img, m), main = paste("Segmentation using", m))
  }
}

compareGroundTruth <- function(seg_result, ground_truth) {
  dice <- function(a, b) {
    a <- a != 0
    b <- b != 0

    intersection <- sum(a & b)
    denominator <- sum(a) + sum(b)
    return(2 * intersection / denominator)
  }

  methods <- names(seg_result$masks)

  acc <- vapply(
    methods,
    function(m) dice(seg_result$masks[[m]], ground_truth) * 100,
    numeric(1)
  )

  df <- data.frame(
    method = methods,
    accuracy_percent = as.numeric(acc),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(methods, accuracy_percent),
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

  return(p)
}
