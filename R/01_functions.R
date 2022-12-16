#' Launch the shiny application for MCPtaggR
#'
#' @import shiny
#' @import shinyFiles
#' @import shinyFeedback
#'
#' @export

cutsortr <- function(){
  runApp(system.file("app.R", package = "CutSortR"))
}

#' Slice an image
#'
#' @param img_fn A path to an image file
#' @param width An integer to specify the width of output sliced images
#' @param height An integer to specify the height of output sliced images
#' @param step An integer to specify the sliding window step size
#' @param divide An integer to specify how many tiles the image is cut into.
#' @param fmt A string to specify the output file format
#' @param out_dir A path to the output directory
#' @param out The prefix of output files
#'
#' @importFrom magick image_read image_info geometry_area image_crop image_write
#'
#' @export
#'
sliceImages <- function(img_fn, width = 100, height = 100, step = 80, divide = 0,
                        fmt = "auto", out_dir = "", out = "slice"){
  if(length(img_fn) == 1){
    img <- image_read(img_fn)
    info <- image_info(img)

    if(fmt == "auto"){
      fmt = sub(".*\\.", "", img_fn)
      fmt <- match.arg(fmt, c("jpg", "png", "tiff", "gif"))
    } else {
      fmt <- match.arg(fmt, c("jpg", "png", "tiff", "gif"))
    }
    if(out_dir == ""){out_dir <- "./"}
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    coord_fn <- paste(out_dir, paste0(out, ".coordinate.csv"), sep = "/")
    write.table(t(c("fileName", "x", "y", "width", "height")), coord_fn,
                row.names = FALSE, col.names = FALSE, sep = ",")
    
    x_step <- step
    y_step <- step
    if(divide != 0){
        x_step <- info$width / divide
        y_step <- info$height / divide
        if(x_step != round(x_step)){
            x_step <- round(x_step + 1)
        } else {
            y_step <- round(y_step + 1)
        }
        width <- x_step
        height <- y_step
    }
    
    x <- 0
    y <- 0
    while(TRUE){
      out_fn <- paste(paste(out, x, y, sep = "_"), fmt, sep = ".")
      image_write(image_crop(img, geometry_area(width, height, x, y)),
                  paste(out_dir, out_fn, sep = "/"),
                  fmt, 100, 16)
      write.table(t(c(out_fn, x, y, width, height)), coord_fn,
                  row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
      if(x + width < info$width){
        x <- x + x_step

      } else {
        x <- 0
        if(y + height < info$height){
          y <- y + y_step
        } else {
          break
        }
      }
    }

  } else if(length(img_fn) > 1){
    for(fn in img_fn){
      sliceImages(img_fn = fn, width = width, height = height, step = step, divide = divide,
                  fmt = fmt,
                  out_dir = paste(out_dir, gsub(".*\\/|\\..*", "", fn), sep = "/"),
                  out = out)
    }
  }
}


#' Sort annotated images
#'
#' @param ann_fn A path to an annotation file
#' @param coord_fn A path to a coordinate file
#' @param out_dir A path to an output directory
#'
#' @importFrom magick image_read image_info geometry_area image_crop image_write
#' @importFrom jsonlite read_json
#' 
#' @export
#'
sortImages <- function(ann_fn, coord_fn = NULL, out_dir = ""){
  ann <- read_json(ann_fn)
  ann_dir <- paste(head(unlist(strsplit(ann_fn, "/")), -1), collapse = "/")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_df <- NULL
  for(i in seq_along(ann)){
    ann_i <- ann[[i]]
    img_fn <- ann_i$image
    labels <- sapply(ann_i$annotations, function(x) {x$label})
    coordinates <- lapply(ann_i$annotations, function(x) {as.data.frame(x$coordinates)})
    coordinates <- do.call("rbind", coordinates)
    df <- data.frame(img_fn, label = labels, coordinates)

    img <- image_read(paste(ann_dir, img_fn, sep = "/"))
    fmt <- switch(image_info(img)$format,
                  JPEG = "jpg", "JPG" = "jpg", "jpeg" = "jpg", jpg = "jpg",
                  PNG = "png", png = "png", TIFF = "tif", TIF = "tif",
                  tiff = "tiff", tif = "tif")
    for(j in seq_len(nrow(df))){
      image_write(image_crop(img, geometry_area(df$width[j],
                                                df$height[j],
                                                df$x[j] - df$width[j]/2,
                                                df$y[j] - df$height[j]/2)),
                  paste(paste(paste(out_dir, sub("\\..*", "", img_fn), sep = "/"),
                              df$x[j],
                              df$y[j],
                              df$width[j],
                              df$height[j],
                              df$label[j],
                              sep = "_"), fmt, sep = "."),
                  fmt, 100)
    }
    out_df <- rbind(out_df, df)
  }
  write.csv(out_df, paste(out_dir, "sortedImages.csv", sep = "/"), row.names = FALSE)
  cls <- table(out_df$label)
  return(cls)
}
