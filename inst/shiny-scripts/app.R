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
             directly in .tif format, choose hyperparameters for  denoising algorithms,
             and select segmentation methods to compare. If the ground truh mask is provided,
             accuracy metrics will be calculated and compared for each segmentation method.
             Thus, the application will display the denoised images, segmentation results,
             and accuracy metrics. For more information, please refer to the cryoCompare package
             documentation."),

      # Extra vertical spacing
      br(),
      br(),

      tags$p("Instructions: Upload your tomogram and (optional) a ground truth mask,
              enter hyperparameters for denoising algorithms,
              select the segmentation methods you wish to compare, and click 'Run'
              to see the results."),

      fileInput("tomogram", "Upload tomogram (.tif format)", accept = c(".tif", ".tiff")),

      fileInput("ground_truth", "Upload ground truth mask (optional, .tif format)", accept = c(".tif", ".tiff")),

      textInput("gaussian_sigma", "Gaussian denoising sigma:", value = "1"),

      textInput("median_size", "Median denoising size:", value = "3"),

      checkboxGroupInput("seg_methods", "Select segmentation methods",
                         choices = list("Otsu" = "Otsu", "Triangle" = "Triangle", "Huang" = "Huang", "Mean" = "Mean")),

      actionButton("button2", "Run")
    ),

    mainPanel(

      tabsetPanel(

        tabPanel("Denoised images", uiOutput("denoised_images")),

        tabPanel("Segmentation results", uiOutput("segmentation_results")),

        tabPanel("Accuracy metrics", tableOutput("accuracy_metrics"), plotOutput("accuray_plot"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {

  observeEvent(input$button2, {
    req(input$tomogram)

    # Read tomogram
    tomogram <- ijtiff::read_tif(input$tomogram$datapath)

    # Run denoising
    denoised_results <- cryoCompare::runDenoising(image = tomogram,
                                                  gaussian_sigma = input$gaussian_sigma,
                                                  median_size = input$median_size)

    # Display denoised images
    output$denoised_images <- renderUI({
      lapply(names(denoised_results), function(name) {
        img_path <- tempfile(fileext = ".png")
        png(img_path)
        ijtiff::display(denoised_results[[name]])
        dev.off()
        tags$div(
          tags$h4(name),
          tags$img(src = img_path, width = "100%")
        )
      })
    })

    # Select image for segmentation (for simplicity, using Gaussian denoised image)
    selected_image <- denoised_results$gaussian

    # Load ground truth if provided
    if (is.null(input$ground_truth)) {
      ground_truth_mask <- NULL
    } else {
      ground_truth_mask <- ijtiff::read_tif(input$ground_truth$datapath)
    }

    # Run segmentation pipeline
    seg_results <- cryoCompare::runSegmentation(
      image = selected_image,
      methods = input$seg_methods,
      ground_truth = ground_truth_mask
    )

    # Extract masks for visualization
    segmentation_masks <- seg_results$segmentation$masks

    # Output segmentation visualisations
    output$segmentation_results <- renderUI({
      lapply(names(segmentation_masks), function(method) {

        seg_path <- tempfile(fileext = ".png")

        png(seg_path)
        ijtiff::display(segmentation_masks[[method]])
        title(method)
        dev.off()

        tags$div(
          tags$h4(method),
          tags$img(src = seg_path, width = "100%")
        )
      })
    })

    # Handle accuracy metrics
    if (!is.null(ground_truth_mask)) {
      gt <- seg_results $ground_truth

      output$accuracyMetrics <- renderTable({
        gt$dice_score
      })

      output$accuracy_plot <- renderPlot({
        gt$p
      })

    } else {
      output$accuracyMetrics <- renderTable({
        data.frame(
          Message = "No ground truth provided. Accuracy metrics not calculated."
        )
      })

      output$accuracy_plot <- renderPlot({
        plot.new()
        text(0.5, 0.5, labels = "No ground truth provided.")
      })

    }
  })
}

# Create Shiny app
shiny::shinyApp(ui, server)

# [END]
