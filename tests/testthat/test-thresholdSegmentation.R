# Test user input validation
test_that("thresholdSegmentation errors for non ijtiff_img input", {
  fake_img <- matrix(1:4, nrow = 2)

  expect_error(
    thresholdSegmentation(fake_img, methods = "Otsu"),
    "Input image must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif()."
  )
})

test_that("thresholdSegmentation errors for invalid segmentation methods", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  expect_error(
    thresholdSegmentation(img, methods = "InvalidMethod"),
    "Invalid segmentation method. Choose from: Huang, Mean, Otsu, Triangle"
  )
})

# Test output structure
test_that("thresholdSegmentation returns correct structure", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  class(img) <- "ijtiff_img"

  methods <- c("Otsu", "Triangle")

  result <- suppressMessages(thresholdSegmentation(img, methods))

  # Correct outer structure
  expect_type(result, "list")
  expect_named(result, c("thresholds", "masks"))

  # Thresholds part
  expect_true(is.numeric(result$thresholds))
  expect_equal(names(result$thresholds), methods)
  expect_length(result$thresholds, length(methods))

  # Masks part
  expect_true(is.list(result$masks))
  expect_equal(names(result$masks), methods)
  expect_length(result$masks, length(methods))

  # Each mask is type logical and same size as input
  for (m in methods) {
    expect_type(result$masks[[m]], "logical")
    expect_equal(dim(result$masks[[m]]), dim(img))
  }
})

# [END]
