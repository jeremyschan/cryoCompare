test_that("viewSegmentation calls display for each requested method", {
  # Create a mock image
  img_mat <- matrix(as.integer(1:100), nrow = 10, ncol = 10)
  img_arr <- array(img_mat, dim = c(10, 10, 1, 1))
  td <- tempdir(check = TRUE)
  src <- file.path(td, "test_image.tif")
  ijtiff::write_tif(img_arr, src, bits_per_sample = 8, overwrite = TRUE)
  img <- ijtiff::read_tif(src)

  methods <- c("Otsu", "Triangle")

  calls <- 0L

  testthat::with_mocked_bindings({
    result <- viewSegmentation(image = img, methods = methods)
    testthat::expect_null(result)
  },
  viewSegmentation = function(image, methods) {
    for (m in methods) {
      ijtiff::display(autothresholdr::apply_mask(image, m),
                      main = paste("Segmentation using", m))
      calls <<- calls + 1L
    }
    return()
  })

  testthat::expect_identical(calls, length(methods))
})

# [END]
