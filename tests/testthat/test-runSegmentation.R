test_that("runSegmentation validates methods and runs thresholding", {

  # Create a mock image
  img_mat <- matrix(as.integer(1:100), nrow = 10, ncol = 10)
  img_arr <- array(img_mat, dim = c(10, 10, 1, 1))
  td <- tempdir(check = TRUE)
  src <- file.path(td, "test_image.tif")
  ijtiff::write_tif(img_arr, src, bits_per_sample = 8, overwrite = TRUE)
  img <- ijtiff::read_tif(src)

  methods <- c("Otsu", "Triangle")

  got_thresholds <- NULL

  testthat::with_mocked_bindings({
    result <- runSegmentation(image = img, methods = methods)
    testthat::expect_null(result)
  },
  viewSegmentation = function(image, methods) {invisible(NULL)},
  thresholdSeg = function(image, methods) {
    thresholds <- stats::setNames(numeric(length(methods)), methods)
    masks <- vector("list", length(methods))
    names(masks) <- methods

    for (m in methods) {
      message(sprintf("Running segmentation method: %s...\n", m))
      thr <- autothresholdr::auto_thresh(int_arr = image, method = m)
      thresholds[m] <- thr
      masks[[m]] <- image >= thr
    }

    got_thresholds <<- thresholds
    return(list(thresholds = thresholds, masks = masks))
    },
  .compareGroundTruth = function(seg_result, ground_truth) {invisible(NULL)}
  )

  testthat::expect_true(is.numeric(got_thresholds))
  testthat::expect_equal(names(got_thresholds), methods)
})

# [END]
