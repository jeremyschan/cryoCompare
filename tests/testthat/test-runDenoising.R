# Test user input validation
test_that("runDenoising errors for invalid gaussian_sigma", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  expect_error(
    runDenoising(img, gaussian_sigma = -1),
    "gaussian_sigma must be a positive integer."
  )
})

test_that("runDenoising errors for invalid median_size", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  expect_error(
    runDenoising(img, median_size = 0),
    "median_size must be a positive integer."
  )
})

# Test output
test_that("runDenoising produces visualisation plots", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  tmp <- tempdir()

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  suppressMessages(runDenoising(img, tmp))

  p <- recordPlot()
  expect_equal(length(p), 3) # Expecting 3 plots: original, Gaussian, Median
})

test_that("runDenoising saves gaussian and median images", {
  data("TS_001.133", package = "cryoCompare")
  img <- TS_001.133

  tmp <- tempdir()

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  suppressMessages(
    runDenoising(
    image = img,
    output_dir = tmp,
    gaussian_sigma = 1,
    median_size = 3
  ))

  gaussian_path <- file.path(tmp, "gaussian.tif")
  median_path <- file.path(tmp, "median.tif")

  expect_true(file.exists(gaussian_path))
  expect_true(file.exists(median_path))
})

# [END]
