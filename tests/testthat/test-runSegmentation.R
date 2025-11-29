# Test user input validation for runSegmentation
test_that("runSegmentation errors for non ijtiff_img input", {
  fake_img <- matrix(1:4, nrow = 2)

  expect_error(
    runSegmentation(fake_img, methods = "Otsu"),
    "Input image must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif()."
  )
})

test_that("runSegmentation errors for invalid methods", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  expect_error(
    runSegmentation(img, methods = "InvalidMethod"),
    "Invalid segmentation method. Choose from: Huang, Mean, Otsu, Triangle"
  )
})

test_that("runSegmentation errors for invalid ground truth type", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  bad_gt <- matrix(1:9, nrow = 3)

  expect_error(
    runSegmentation(img, methods = "Otsu", ground_truth = bad_gt),
    "Ground truth mask must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif()."
  )
})

# Integration tests
test_that("runSegmentation runs end to end without ground truth", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  result <- suppressMessages(
    runSegmentation(
    image = img,
    methods = c("Otsu", "Triangle")
  ))

  # Should return a list regardless of ground truth
  expect_true(is.list(result))

  # Confirm something was plotted
  p <- recordPlot()
  expect_gt(length(p), 0)
})

test_that("runSegmentation runs end to end with ground truth", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  data("TS_001.133_ground_truth", package = "cryoCompare")
  gt <- TS_001.133_ground_truth

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  result <- suppressMessages(
    runSegmentation(
    image = img,
    methods = c("Otsu", "Mean"),
    ground_truth = gt
  ))

  # Result should be a data frame containing dice scores for each method
  expect_true(is.list(result))

  # Plot should exist after running viewSegmentation + printed gt plot
  p <- recordPlot()
  expect_gt(length(p), 0)
})

# [END]
