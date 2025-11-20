test_that("thresholdSeg returns thresholds and masks for methods", {
  # Create a mock image
  img_mat <- matrix(as.integer(1:100), nrow = 10, ncol = 10)
  img_arr <- array(img_mat, dim = c(10, 10, 1, 1))
  td <- tempdir(check = TRUE)
  src <- file.path(td, "test_image.tif")
  ijtiff::write_tif(img_arr, src, bits_per_sample = 8, overwrite = TRUE)
  img <- ijtiff::read_tif(src)

  methods <- c("Otsu", "Triangle")

  results <- thresholdSegmentation(image = img, methods = methods)

  testthat::expect_type(results, "list")
  testthat::expect_named(results, c("thresholds", "masks"))
  testthat::expect_true(all(names(results$thresholds) == methods))
  testthat::expect_true(all(names(results$masks) == methods))
  testthat::expect_type(results$thresholds, "double")
})

# [END]
