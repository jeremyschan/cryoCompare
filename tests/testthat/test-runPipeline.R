# Cannot test with interactive prompt

# test_that("runPipeline with choice = 'original' forwards original image to
# runSegmentation", {
#
#   # Create a mock ijtiff_img object
#   img_mat <- matrix(as.integer(1:100), nrow = 10, ncol = 10)
#   img_arr <- array(img_mat, dim = c(10, 10, 1, 1))
#   td <- tempdir(check = TRUE)
#   src <- file.path(td, "test_image.tif")
#   ijtiff::write_tif(img_arr, src, bits_per_sample = 8, overwrite = TRUE)
#   img <- ijtiff::read_tif(src)
#
#   # Setup variables to be changed if run correctly
#   called <- FALSE
#   class <- NULL
#
#   testthat::with_mocked_bindings({
#     result <- runPipeline(
#       image = img,
#       methods = c("Otsu", "Triangle"),
#       output_dir = tempdir(check = TRUE),
#       ground_truth = NULL
#     )
#     testthat::expect_null(result)
#   },
#   # Mock interactive prompt to choose 'original'
#   choice = function(prompt = "") {"original"},
#   runDenoising = function(image, output_dir = NULL) {invisible(NULL)},
#
#   runSegmentation = function(chosen_image, methods, ground_truth = NULL) {
#     called <- TRUE
#     class <- class(chosen_image)
#   }
#   )
#
#   testthat::expect_true(called)
#   testthat::expect_true("ijtiff_img" %in% class)
# })
#
# # [END]
