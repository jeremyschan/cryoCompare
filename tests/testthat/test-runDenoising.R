library(ijtiff)

test_that("runDenoising writes 2 TIFFs to a writable directory", {
  # Set up test inputs
  data("TS_001.133", package = "cryoCompare")
  img <- ijtiff::as_EBImage(TS_001.133)
  out_dir <- tempdir(check = TRUE)

  # Run the function
  result <- suppressMessages(cryoCompare::runDenoising(image = img,
                                                    output_dir = out_dir))

  # Function returns null
  testthat::expect_null(result)

  # Two output files created
  gaussian_path <- file.path(out_dir, "gaussian.tif")
  median_path <- file.path(out_dir, "median.tif")

  testthat::expect_true(file.exists(gaussian_path))
  testthat::expect_true(file.exists(median_path))
})

# [END]
