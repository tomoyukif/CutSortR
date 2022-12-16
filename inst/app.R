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
                                      tags$style("#dirchoose {font-size:12px; display:block; }"),
                                      p("The sliced images will be named",
                                        "<input file name>_<sliced image width>x<sliced image height>_<sliding step>.<output image format>",
                                        "in a new directory named <input file name> in the same",
                                        "dicretory with the images to be sliced."),
                                      verbatimTextOutput("dirchoose"),
                                      dataTableOutput("fn_tbl"),
                                      textOutput("tif_img"),
                                      plotOutput("img")
                                  )
                              )
                          )),
                 tabPanel("Image Sorter",
                          fluidPage(
                              titlePanel("Image Sorter"),
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      shinyDirButton('in_dir_sort',
                                                     label='Input direcotry:',
                                                     title='Select directory',
                                                     multiple=TRUE),
                                      p("The annotation file(s) would be searched recuresively",
                                        "in the input directory and sorted images will be written",
                                        "in a direcotry",
                                        "based on the annotation file in the same direcotry."),
                                      br(),
                                      br(),
                                      actionButton("sort", "Do sort!")
                                  ),
                                  mainPanel(
                                      dataTableOutput("ann_fn_sort")
                                  )
                              )
                          )),
                 tabPanel("Image Cropper",
                          fluidPage(
                              titlePanel("Image Cropper"),
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      shinyDirButton('in_dir_crop',
                                                     label='Input direcotry:',
                                                     title='Select directory',
                                                     multiple=TRUE),
                                      p("The annotation files (.xml) would be searched recuresively",
                                        "in the input directory and cropped images will be written ",
                                        "in a direcotry",
                                        "based on the annotation files."),
                                      br(),
                                      br(),
                                      actionButton("crop", "Do crop!")
                                  ),
                                  mainPanel(
                                      dataTableOutput("xml_fn"),
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
            ex <- subset(read_exif(fn1$datapath),
                         select = c(FileName, SourceFile, FileType,
                                    ImageWidth, ImageHeight, Directory))
            output$dirchoose <- renderText(paste0("Output directory: \n",
                                                  ex$Directory[1]))
            tbl <- subset(ex, select = c(FileName, ImageWidth, ImageHeight))
            output$fn_tbl <- renderDataTable(tbl,
                                             options = list(pageLength = 10,
                                                            scrollY = "200px",
                                                            scrollCollapse = TRUE,
                                                            searching = FALSE))
            
            img_info$fn <- ex$FileName[1]
            img_info$path <- ex$SourceFile[1]
            img_info$width <- ex$ImageWidth[1]
            img_info$height <- ex$ImageHeight[1]
            img_info$minlen <- min(c(ex$ImageWidth[1], ex$ImageHeight[1]))
            img_info$fmt <- switch(ex$FileType[1],
                                   JPEG = "jpg", JPG = "jpg",
                                   jpeg = "jpg", jpg = "jpg",
                                   PNG = "png", png = "png", 
                                   TIFF = "tif", TIF = "tif",
                                   tiff = "tif", tif = "tif")
            
            render_limit <- 2000
            if(img_info$fmt == "tif"){
                output$tif_img <- renderText("Not support rendering TIFF images on this web app.")
                
            } else if(all(c(img_info$width, img_info$height) <= render_limit)){
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
    shinyDirChoose(input, "in_dir_sort", roots = volumes)
    
    observeEvent(input$in_dir_sort, {
        in_dir_sort <- parseDirPath(volumes, input$in_dir_sort)
        ann_files <- list.files(in_dir_sort, "annotation", recursive = TRUE, full.names = TRUE)
        df <- data.frame(AnnotationFile = ann_files)
        output$ann_fn_sort <- renderDataTable(df,
                                              options = list(pageLength = 10,
                                                             scrollCollapse = TRUE,
                                                             searching = FALSE))
    })
    
    observeEvent(input$sort, {
        if(!isTruthy(input$in_dir_sort)){
            showNotification("Please select input directory(s)!", duration = 10)
            
        } else {
            withProgress(message = "Sorting images...", value = 0, {
                in_dir_sort <- parseDirPath(volumes, input$in_dir_sort)
                ann_files <- list.files(in_dir_sort, "annotation", recursive = TRUE, full.names = TRUE)
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
    
    
    
    # Function for Image Cropper
    shinyDirChoose(input, "in_dir_crop", roots = volumes)
    
    observeEvent(input$in_dir_crop, {
        in_dir_crop <- parseDirPath(volumes, input$in_dir_crop)
        ann_files <- list.files(in_dir_crop, ".xml$", recursive = TRUE, full.names = TRUE)
        df <- data.frame(AnnotationFile = ann_files)
        output$xml_fn <- renderDataTable(df,
                                         options = list(pageLength = 10,
                                                        scrollCollapse = TRUE,
                                                        searching = FALSE))
    })
    
    observeEvent(input$crop, {
        if(!isTruthy(input$in_dir_crop)){
            showNotification("Please select input directory(s)!", duration = 10)
            
        } else {
            withProgress(message = "Sorting images...", value = 0, {
                in_dir_crop <- parseDirPath(volumes, input$in_dir_crop)
                ann_files <- list.files(in_dir_crop, "annotation", recursive = TRUE, full.names = TRUE)
                n <- length(ann_files)
                for(i_ann in ann_files){
                    i_dir <- dirname(i_ann)
                    coord_fn <- list.files(i_dir, "\\.coordinate\\.csv", full.names = TRUE)
                    if(length(coord_fn) == 0 ){
                        coord_fn <- NULL
                    }
                    cropImages(i_ann, coord_fn, out_dir = file.path(i_dir, "sort_out"))
                }
                incProgress(1/n, detail = paste0("Processing the annotation file in ", i_ann))
            })
            showNotification("Sorted!")
        }
    })
}

shinyApp(ui = ui, server = server)
