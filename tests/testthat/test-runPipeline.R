# Test user input validation
test_that("runPipeline errors when image is not an ijtiff_img", {
  fake_img <- matrix(1:4, 2, 2)

  expect_error(
    runPipeline(fake_img, methods = "Otsu"),
    "Input image must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif()."
  )
})

test_that("runPipeline errors for invalid segmentation method", {
  fake_img <- structure(list(), class = "ijtiff_img")

  expect_error(
    runPipeline(fake_img, methods = "InvalidMethod"),
    "Invalid segmentation method. Choose from: Huang, Mean, Otsu, Triangle"
  )
})

test_that("runPipeline errors when ground truth is wrong class", {
  fake_img <- structure(list(), class = "ijtiff_img")
  wrong_gt <- matrix(1:4, 2, 2)

  expect_error(
    runPipeline(fake_img, methods = "Otsu", ground_truth = wrong_gt),
    "Ground truth mask must be of class 'ijtiff_img'.
         Please read the .tif file using ijtiff::read_tif()."
  )
})

# Test input handling for numeric prompts (gaussian and median
# denoising algorithm parameters) and integration testing
test_that("runPipeline repeats input until valid numeric value is given", {
  data("TS_001.133", package = "cryoCompare")
  test_img <- TS_001.133

  tmp <- tempdir()

  # invalid gaussian filtering: "abc", "0"
  # valid gaussian filtering: "3"
  # valid median filter: "2"
  # invalid image: "none"
  # valid image: "original"

  user_inputs <- c("abc", "0", "3", "2", "none", "original")
  counter <- 0

  fake_read <- function(prompt = "") {
    counter <<- counter + 1
    return(user_inputs[counter])
  }

  local_mocked_bindings(
    read = fake_read
  )

  expect_no_error(
    suppressMessages(
      runPipeline(image = test_img,
                  methods = "Otsu",
                  output_dir = tmp))
  )
})

# [END]
