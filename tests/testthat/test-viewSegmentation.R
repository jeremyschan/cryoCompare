# Test user input validation
test_that("viewSegmentation errors for non ijtiff_img input", {
  fake_img <- matrix(1:4, nrow = 2)

  expect_error(
    viewSegmentation(fake_img, methods = "Otsu"),
    "Input image must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif()."
  )
})

test_that("viewSegmentation errors for invalid method names", {
  fake_img <- structure(list(), class = "ijtiff_img")

  expect_error(
    viewSegmentation(fake_img, methods = "InvalidMethod"),
    "Invalid segmentation method. Choose from: Huang, Mean, Otsu, Triangle"
  )
})

# Test that plots are outputted
test_that("viewSegmentation draws plots", {
  img <- array(1:9, c(3, 3))
  class(img) <- "ijtiff_img"

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  methods <- c("Otsu", "Triangle")
  viewSegmentation(img, methods)

  p <- recordPlot()
  expect_equal(length(p), length(methods) + 1) # +1 for original image
})

# [END]
