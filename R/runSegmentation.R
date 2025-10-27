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
#' @import torch
#' @import ijtiff
#' @import autothresholdr

runSegmentation <- function(image_path) {
  img <- ijtiff::read_tif(image_path)

  seg <- thresholdSeg(img)
  plot <- plotSegmentationThresholds(seg)
  plot
}


# memBrainSeg <- function(tomogram_path, model_path) {
#   # Load the pre-trained model
#   device <- torch::torch_device(if (torch::cuda_is_available()) "cuda" else "cpu")
#   model_checkpoint <- torch::torch_load(model_path)
#   model_checkpoint$to(device = torch::torch_device(device))
#
#   model_checkpoint$eval()
#
#   # Read the tomogram data
# }


thresholdSeg <- function(image_path) {
  methods <- c("Huang", "Mean", "Otsu", "Triangle")
  thresholds <- stats::setNames(numeric(length(methods)), methods)
  masks <- vector("list", length(methods))
  names(masks) <- methods

  for (m in methods) {
    thr <- autothresholdr::auto_thresh(img, method = m)
    thresholds[m] <- thr
    masks[[m]] <- img >= thr
  }

  return(list(thresholds = thresholds, masks = masks))
}

plotSegmentationSlices <- function(seg_result) {}

plotSegmentationThresholds <- function(seg_result) {
  thresholds <- seg_result$thresholds

  df <- tibble::tibble(methods = names(thresholds),
                       threshold = as.numeric(thresholds))

  plot <- ggplot2::ggplot(df, ggplot2::aes(x = methods, y = threshold)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Segmentation Thresholds by Method",
                  x = "Segmentation Method",
                  y = "Threshold Value")

  return(plot)
}

image_path <- "~/TS_001stacked"
img <- ijtiff::read_tif(image_path)
autothresholdr::auto_thresh(img, "tri")
ijtiff::display(autothresholdr::apply_mask(img, "tri"))

