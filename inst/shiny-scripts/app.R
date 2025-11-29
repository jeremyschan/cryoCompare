#' This Shiny application is inspired by the following tutorials:
#' Tabsets. (n.d.) Shiny. https://shiny.posit.co/r/gallery/application-layout/tabsets/
#' conditionalPanel demo. (n.d.) Shiny. https://shiny.posit.co/r/gallery/dynamic-user-interface/conditionalpanel-demo/
#' File Upload. (n.d.) Shiny. https://shiny.posit.co/r/gallery/widgets/file-upload/

library(shiny)
library(ijtiff)
library(autothresholdr)

# Define UI
ui <- fluidPage(
  titlePanel("cryoCompare: cryo-ET Segmentation and Denoising Comparison"),

  sidebarLayout(

    sidebarPanel(
      tags$p("This is a Shiny application for comparing denoising and segmentation
             methods on cryo-electron tomography data, part of the cryoCompare package in R."),

      tags$b("Description: cryoCompare is an R package designed to analyse cryo-electron
             tomography (cryo-ET) data by comparing the performance of segmentation and
             denoising algorithms. This Shiny application is part of the cryoCompare package.
             It allows user to upload their tomogram and optionally, a ground truth mask
             directly in .tif format, choose hyperparameters for denoising algorithms
             (Gaussian and Median), then select segmentation methods (Otsu, Triangle, Huang, Mean)
             to compare. If the ground truth mask is provided, a plot showing the DICE score
             metric for each segmentation method will also be displayed. For more information,
             please refer to the cryoCompare package documentation."),

      # Extra vertical spacing
      br(),
      br(),

      tags$p("Instructions: Upload your tomogram (and optionally a ground truth mask),
              then enter hyperparameters for denoising algorithms, and click 'Run denoising'.
              After denoising is complete, select one of the denoised images (or the original)
              for segmentation, choose segmentation methods, and click 'Run segmentation'.
              Results for each step can be accessed by their respective tabs at the top.
              Test data is available in the /inst/extdata directory, with example tomogram
              TS_001.133.tif and ground truth mask TS_001.133_ground_truth.tif."),

      # Inputs
      fileInput("tomogram",
                "Upload the tomogram to be denoised and segmented (.tif format)",
                accept = c(".tif", ".tiff")),

      fileInput("ground_truth",
                "Upload the ground truth mask of the tomogram (optional, .tif format)",
                accept = c(".tif", ".tiff")),

      textInput("gaussian_sigma",
                "Gaussian denoising sigma: (recommended integer values between 1 and 3)",
                value = "1"),

      textInput("median_size",
                "Median denoising size: (recommended integer values between 1 and 5)",
                value = "3"),

      # Button for denoising step
      actionButton("run_denoising", "Run denoising"),
      textOutput("denoise_status"),

      br(),

      # Selection and segmentation step only possible after denoising
      conditionalPanel(
        condition = "output.denoise_status == 'Denoising complete.'",

        # Choose image for segmentation
        uiOutput("image_choice_ui"),

        # Segmentation methods
        checkboxGroupInput("seg_methods", "Select segmentation methods to run",
                           choices = list("Otsu" = "Otsu",
                                          "Triangle" = "Triangle",
                                          "Huang" = "Huang",
                                          "Mean" = "Mean")),

        # Button for segmentation step
        actionButton("run_segmentation", "Run segmentation"),
        textOutput("segment_status"),
      ),
    ),

    mainPanel(

      # Tabs for denoised images, segmentation results, and accuracy plot
      tabsetPanel(

        tabPanel("Denoised images", uiOutput("denoised_images")),

        tabPanel("Segmentation results", uiOutput("segmentation_results")),

        tabPanel("Accuracy comparison", plotOutput("accuracy_plot"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {

  # Store denoised images and segmentation results since it's two steps
  rv <- reactiveValues(
    denoised = NULL,
    segmented = NULL
  )

  # Create temporary directory for images
  image_dir <- file.path("www", "tmp")
  if (!dir.exists(image_dir)) {
    dir.create(image_dir, recursive = TRUE)
  }
  shiny::addResourcePath("temp_images", image_dir)

  # Step 1: Run denoising
  observeEvent(input$run_denoising, {
    req(input$tomogram)

    # Update status
    output$denoise_status <- renderText("Running...")

    # Read tomogram
    tomogram <- ijtiff::read_tif(input$tomogram$datapath)

    # Run denoising
    gaussian_sigma <- as.numeric(input$gaussian_sigma)
    median_size <- as.numeric(input$median_size)

    rv$denoised <- cryoCompare::runDenoising(image = tomogram,
                                             output_dir = image_dir,
                                             gaussian_sigma = gaussian_sigma,
                                             median_size = median_size)

    # Create buttons for selecting image for segmentation
    output$image_choice_ui <- renderUI({
      req(rv$denoised)
      radioButtons(
        "selected_image",
        "Choose image for segmentation:",
        choices = names(rv$denoised),
        selected = names(rv$denoised)[1]
      )
    })

    # Display denoised images in three columns
    output$denoised_images <- renderUI({
      image_names <- names(rv$denoised)
      rows <- split(image_names, ceiling(seq_along(image_names) / 3))

      tagList(
        lapply(rows, function(row_names) {
          fluidRow(
            # Create a column for each image in the row
            lapply(row_names, function(name) {
              label <- paste0(
                toupper(substr(name, 1, 1)),
                substr(name, 2, nchar(name)),
                " image"
              )

              # Save denoised image as PNG for display
              denoise_name <- paste0(name, ".png")
              denoise_path <- file.path(image_dir, denoise_name)

              png(denoise_path)
              ijtiff::display(rv$denoised[[name]])
              dev.off()

              # Display each image in its own column
              column(
                width = 4,
                tags$div(
                  style = "text-align: center; margin-bottom: 20px;",
                  tags$h4(label),
                  tags$img(src = file.path("temp_images", denoise_name),
                           width = "100%"),
                  style = "border: 1px solid #ccc; border-radius: 5px;"
                )
              )
            })
          )
        })
      )
    })
    # Update status
    output$denoise_status <- renderText("Denoising complete.")
  })

  # Step 2: Run segmentation
  observeEvent(input$run_segmentation, {
    # Require denoised images and the selection of one
    req(rv$denoised)
    req(input$selected_image)

    # Update status
    output$segment_status <- renderText("Running...")

    # Load selected image for segmentation
    tif_directories <- list(
      original = input$tomogram$datapath,
      gaussian = file.path("www", "tmp", "gaussian.tif"),
      median = file.path("www", "tmp", "median.tif"))

    selected_image <- ijtiff::read_tif(tif_directories[[input$selected_image]])

    # Load ground truth if provided
    if (is.null(input$ground_truth)) {
      ground_truth_mask <- NULL
    } else {
      ground_truth_mask <- ijtiff::read_tif(input$ground_truth$datapath)
    }

    # Run segmentation
    rv$segmented <- cryoCompare::runSegmentation(
      image = selected_image,
      methods = input$seg_methods,
      ground_truth = ground_truth_mask
    )

    # Extract segmentation masks
    segmentation_masks <- rv$segmented$segmentation$masks

    # Display segmentation results
    output$segmentation_results <- renderUI({
      lapply(names(segmentation_masks), function(method) {

        # Save segmentation mask as PNG for display
        seg_name <- paste0(method, ".png")
        seg_path <- file.path(image_dir, seg_name)

        png(seg_path)
        ijtiff::display(segmentation_masks[[method]])
        dev.off()

        # Display each segmentation result
        tags$div(
          tags$h4(method),
          tags$img(src = file.path("temp_images", seg_name),
                   width = "50%")
        )
      })
    })

    # Display accuracy metrics if ground truth is provided
    if (is.null(ground_truth_mask)) {
      output$accuracy_plot <- renderPlot({
        plot.new()
        text(0.5, 0.5, "No ground truth mask provided.")
      })
    } else {
      gt <- rv$segmented$ground_truth

      output$accuracy_plot <- renderPlot(gt$p)
    }
    # Update status
    output$segment_status <- renderText("Segmentation complete.")

    # Clean up temporary directory
    unlink(file.path(image_dir, "*"), recursive = TRUE)
  })
}

# Create Shiny app
shiny::shinyApp(ui, server)

# [END]
