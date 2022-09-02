library(shiny)

img_fmt <- c("jpg", "png", "tif")

ui <- navbarPage("CutSortR",
                 tabPanel("Image Slicer", 
                          fluidPage(
                            titlePanel("Image Slicer"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("fn",
                                          label = "Input image file(s):",
                                          multiple = TRUE,
                                          accept = "image/*"),
                                
                                numericInput("width",
                                            label = "Sliced image width (px):",
                                            value = 100,
                                            min = 1),
                                
                                numericInput("height",
                                             label = "Sliced image height (px):",
                                             value = 100,
                                             min = 1),
                                
                                numericInput("step",
                                             label = "Sliding step size (px):",
                                             value = 100,
                                             min = 1),
                                
                                selectInput("fmt",
                                            "Output image format:",
                                            img_fmt),
                                ),
                              mainPanel(
                                # textOutput("img_width")
                              )
                            )
                          )),
                 tabPanel("Component 2")
)

server <- function(input, output){
  # if(input$fn == ""){
  #   output$img_width <- 100
  # }
} 

shinyApp(ui = ui, server = server)