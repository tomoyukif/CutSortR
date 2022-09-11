library(shiny)
library(shinyFeedback)
library(magick)

ui <- navbarPage("CutSortR",
                 header = useShinyFeedback(),
                 tabPanel("Image Slicer",
                          fluidPage(
                            titlePanel("Image Slicer"),

                            sidebarLayout(
                              sidebarPanel(
                                fileInput("img_fn",
                                          label = "Input image file(s):",
                                          multiple = TRUE,
                                          placeholder = "Select file(s)",
                                          accept = "image/*"),

                                numericInput("slice_width",
                                            label = "Sliced image width (px):",
                                            value = 100,
                                            min = 1),
                                textOutput("invalid_width"),

                                numericInput("slice_height",
                                             label = "Sliced image height (px):",
                                             value = 100,
                                             min = 1),
                                textOutput("invalid_height"),

                                numericInput("step",
                                             label = "Sliding step size (px):",
                                             value = 100,
                                             min = 1),
                                textOutput("invalid_step"),
                                uiOutput("fmt"),
                                downloadButton()
                                ),
                              mainPanel(
                                plotOutput("img")
                              )
                            )
                          )),
                 tabPanel("Image Sorter")
)

server <- function(input, output){
  img_info <- reactiveValues(width = 100,
                             height = 100,
                             minlen = 100,
                             fmt = "jpg")

  observeEvent(input$img_fn, {
    info <- image_info(image_read(input$img_fn$datapath[1]))
    img_info$width <- info$width
    img_info$height <- info$height
    img_info$minlen <- min(info$width, info$height)
    img_info$fmt <- switch(info$format,
                           JPEG = "jpg", "JPG" = "jpg", "jpeg" = "jpg", jpg = "jpg",
                           PNG = "png", png = "png", TIFF = "tif", TIF = "tif",
                           tiff = "tiff", tif = "tif")
  })

  output$fmt <- renderUI({
    selectInput("fmt",
                "Output image format:",
                c("jpg", "png", "tif"),
                selected = img_info$fmt)
  })

  invalid_width <- reactive({
    feedbackWarning("slice_width", input$slice_width > img_info$width, "Invalid width!")
  })
  output$invalid_width <- renderText(invalid_width())

  invalid_height <- reactive({
    feedbackWarning("slice_height", input$slice_height > img_info$height, "Invalid height!")
  })
  output$invalid_height <- renderText(invalid_height())

  invalid_step <- reactive({
    feedbackWarning("step", input$step > img_info$minlen, "Invalid step!")
  })
  output$invalid_step <- renderText(invalid_step())

}

shinyApp(ui = ui, server = server)
