library(shinyFeedback)
library(shinyFiles)
library(magick)
library(exifr)
library(CutSortR)

ui <- navbarPage("CutSortR",
                 header = useShinyFeedback(),
                 tabPanel("Image Slicer",
                          fluidPage(
                              titlePanel("Image Slicer"),
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      shinyFilesButton('img_fn1',
                                                       label='Input image file(s):',
                                                       title='Select file(s)',
                                                       multiple=TRUE),
                                      
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
                                      
                                      numericInput("slice_step",
                                                   label = "Sliding step size (px):",
                                                   value = 100,
                                                   min = 1),
                                      textOutput("invalid_step"),
                                      numericInput("divide",
                                                   label = "Cut into n x n tiles:",
                                                   value = 0,
                                                   min = 0),
                                      uiOutput("fmt"),
                                      br(),
                                      br(),
                                      actionButton("slice", "Do slice!")
                                  ),
                                  mainPanel(
                                      tags$style("#dirchoose {font-size:20px; display:block; }"),
                                      tags$style("#img_fn {font-size:20px; display:block; }"),
                                      tags$style("#img_width {font-size:20px; display:block; }"),
                                      tags$style("#img_height {font-size:20px; display:block; }"),
                                      p("The sliced images will be named",
                                        "<input file name>_<sliced image width>x<sliced image height>_<sliding step>.<output image format>",
                                        "in a new directory named <input file name> in the same",
                                        "dicretory with the images to be sliced."),
                                      textOutput("dirchoose"),
                                      textOutput("img_fn"),
                                      textOutput("img_width"),
                                      textOutput("img_height"),
                                      plotOutput("img")
                                  )
                              )
                          )),
                 tabPanel("Image Sorter",
                          fluidPage(
                              titlePanel("Image Sorter"),
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      shinyDirButton('in_dir',
                                                     label='Input direcotry:',
                                                     title='Select directory',
                                                     multiple=TRUE),
                                      p("The annotation file(s) would be searched recuresively",
                                        "in the input directory and sort images in a direcotry",
                                        "based on the annotation file in the same direcotry."),
                                      br(),
                                      br(),
                                      actionButton("sort", "Do sort!")
                                  ),
                                  mainPanel(
                                      textOutput("dir_choose"),
                                      tableOutput("dir_df"),
                                  )
                              )
                          ))
)

server <- function(input, output){
    img_info <- reactiveValues(fn = "",
                               path = "",
                               width = Inf,
                               height = Inf,
                               minlen = Inf,
                               fmt = "jpg")
    
    observeEvent(input$img_fn1, {
        if(!inherits(input$img_fn1, "shinyActionButtonValue")){
            fn1 <- parseFilePaths(volumes, input$img_fn1)
            ex <- read_exif(fn1$datapath[1])
            img_size <- as.numeric(unlist(strsplit(ex$ImageSize, " ")))
            
            img_info$fn <- fn1$name[1]
            img_info$path <- fn1$datapath[1]
            img_info$width <- img_size[1]
            img_info$height <- img_size[2]
            img_info$minlen <- min(img_size)
            output$img_fn <- renderText(paste0("The first input file: ",
                                               img_info$fn))
            output$img_width <- renderText(paste0("Image width: ",
                                                  img_info$width, "px"))
            output$img_height <- renderText(paste0("Image height: ", 
                                                   img_info$height, "px"))
            img_info$fmt <- switch(ex$FileType,
                                   JPEG = "jpg", JPG = "jpg",
                                   jpeg = "jpg", jpg = "jpg",
                                   PNG = "png", png = "png", 
                                   TIFF = "tif", TIF = "tif",
                                   tiff = "tiff", tif = "tif")
            
            render_limit <- 2000
            if(all(img_size <= render_limit)){
                print("render")
                render_size <- 600
                out_width <- img_info$width
                out_height <- img_info$height
                
                if(out_width > out_height){
                    out_height <- render_size * out_height / out_width
                    out_width <- render_size
                } else {
                    out_width <- render_size * out_width / out_height
                    out_height <- render_size
                }
                output$img <- renderImage(list(src = img_info$path,
                                               width = out_width,
                                               height = out_height),
                                          deleteFile = FALSE)
            } else {
                output$img <- renderImage(list(src = ""),
                                          deleteFile = FALSE)
            }
        }
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
        feedbackWarning("slice_step", input$slice_step > img_info$minlen, "Invalid step!")
    })
    output$invalid_step <- renderText(invalid_step())
    
    volumes = getVolumes()()
    shinyFileChoose(input, 'img_fn1', roots=volumes,
                    filetype=c("jpg", "jpeg", "png", "tif", "tiff"))
    
    do_slice <- reactiveVal(FALSE)
    observeEvent(input$confirmSlice, {
        do_slice(TRUE)
    })
    
    observeEvent(input$slice, {
        if(!isTruthy(input$img_fn1)){
            showNotification("Please select input image file(s)!", duration = 10)
        } else {
            withProgress(message = "Making sliced images...", value = 0, {
                fn1 <- parseFilePaths(volumes, input$img_fn1)
                n <- length(fn1$datapath)
                
                for(i in seq_along(fn1$datapath)){
                    if(input$divide == 0){
                        out_dir <- paste0(tools::file_path_sans_ext(fn1$datapath[i]),
                                          "_", input$slice_width, "x", input$slice_height,
                                          "_", input$slice_step)
                        
                    } else {
                        out_dir <- paste0(tools::file_path_sans_ext(fn1$datapath[i]),
                                          "_", input$slice_width, "x", input$slice_height,
                                          "_", input$divide, "x", input$divide)
                    }
                    if(dir.exists(out_dir)){
                        showNotification(paste0("Slicing ", fn1$name[i], " was skipped. \n",
                                                "You might have already sliced ",
                                                "the given image with the same settings"),
                                         duration = NULL, closeButton = TRUE)
                        
                    } else {
                        out <- tools::file_path_sans_ext(fn1$name[i])
                        
                        sliceImages(img_fn = fn1$datapath[i],
                                    width = input$slice_width,
                                    height = input$slice_height,
                                    step = input$slice_step,
                                    divide = input$divide, 
                                    fmt = input$fmt,
                                    out_dir = out_dir,
                                    out = out)
                        incProgress(1/n, detail = paste0("Processing ", fn1$name[i]))
                    }
                }
            })
            showNotification("Sliced!")
        }
    })
    
    
    # Function for Image Sorter
    shinyDirChoose(input, "in_dir", roots = volumes)
    
    observeEvent(input$in_dir, {
        in_dir <- parseDirPath(volumes, input$in_dir)
        ann_files <- list.files(in_dir, "annotation", recursive = TRUE, full.names = TRUE)
        df <- data.frame(Directory = dirname(ann_files))
        output$dir_choose <- renderText("Annotation files found in the following directory(s)")
        output$dir_df <- renderTable(df)
    })
    
    observeEvent(input$sort, {
        if(!isTruthy(input$in_dir)){
            showNotification("Please select input directory(s)!", duration = 10)
            
        } else {
            withProgress(message = "Sorting images...", value = 0, {
                in_dir <- parseDirPath(volumes, input$in_dir)
                ann_files <- list.files(in_dir, "annotation", recursive = TRUE, full.names = TRUE)
                n <- length(ann_files)
                for(i_ann in ann_files){
                    i_dir <- dirname(i_ann)
                    coord_fn <- list.files(i_dir, "\\.coordinate\\.csv", full.names = TRUE)
                    if(length(coord_fn) == 0 ){
                        coord_fn <- NULL
                    }
                    sortImages(i_ann, coord_fn, out_dir = file.path(i_dir, "sort_out"))
                }
                incProgress(1/n, detail = paste0("Processing the annotation file in ", i_ann))
            })
            showNotification("Sorted!")
        }
    })
}

shinyApp(ui = ui, server = server)
