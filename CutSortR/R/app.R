library(shiny)
library(shinyFeedback)



ui <- navbarPage("CutSortR",
                 header = useShinyFeedback(),
                 tabPanel("Image Slicer",
                          fluidPage(
                            titlePanel("Image Slicer"),

                            sidebarLayout(
                              sidebarPanel(
                                fileInput("fn",
                                          label = "Input image file(s):",
                                          multiple = TRUE,
                                          placeholder = "Select file(s)",
                                          accept = "image/*"),

                                numericInput("width",
                                            label = "Sliced image width (px):",
                                            value = 100,
                                            min = 1),
                                textOutput("invalid_width"),

                                numericInput("height",
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
                                ),
                              mainPanel(
                                textOutput("img_width")
                              )
                            )
                          )),
                 tabPanel("Component 2")
)

server <- function(input, output){
  img_width <- 100
  img_height <- 100
  img_minlen <- min(c(img_width, img_height))
  img_fmt <- "png"

  output$fmt <- renderUI({
    selectInput("fmt",
                "Output image format:",
                c("jpg", "png", "tif"),
                selected = img_fmt)
  })

  invalid_width <- reactive({
    feedbackWarning("width", input$width > img_width, "Invalid width!")
  })
  output$invalid_width <- renderText(invalid_width())

  invalid_height <- reactive({
    feedbackWarning("height", input$height > img_height, "Invalid height!")
  })
  output$invalid_height <- renderText(invalid_height())

  invalid_step <- reactive({
    feedbackWarning("step", input$step > img_minlen, "Invalid step!")
  })
  output$invalid_step <- renderText(invalid_step())

}

shinyApp(ui = ui, server = server)
