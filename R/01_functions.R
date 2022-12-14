#' Launch the shiny application for MCPtaggR
#'
#' @import shiny
#' @import shinyFiles
#' @import shinyFeedback
#' @import exifr
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
            } 
            if(y_step != round(y_step)){
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
            if(x + x_step < info$width){
                x <- x + x_step
                
            } else {
                x <- 0
                if(y + y_step < info$height){
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
    invisible(cls)
}


#' Crop images as annotated
#'
#' @param ann_fn A path to an annotation file
#' @param coord_fn A path to a coordinate file
#' @param out_dir A path to an output directory
#'
#' @importFrom magick image_read image_info geometry_area image_crop image_write
#' @importFrom XML xmlParse xmlToDataFrame
#' 
#' @export
#'
cropImages <- function(xml_fn, out_dir = "", poly_cut){
    xml <- xmlParse(xml_fn)
    xml <- xmlToList(xml)
    img_fn <- xml[[2]]
    xml <- xml[-(1:3)]
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    out_df <- NULL
    for(i in seq_along(xml)){
        xml_i <- xml[[i]]
        img <- image_read(file.path(dirname(out_dir), img_fn))
        fmt <- switch(image_info(img)$format,
                      JPEG = "jpg", "JPG" = "jpg", "jpeg" = "jpg", jpg = "jpg",
                      PNG = "png", png = "png", TIFF = "tif", TIF = "tif",
                      tiff = "tiff", tif = "tif")
        label <- xml_i$name
        box_coord <- as.numeric(xml_i$bndbox)
        names(box_coord) <- c("Xmin", "Ymin", "Xmax", "Ymax")
        if(is.null(xml_i$cubic_bezier)){
            poly_coord <- data.frame(x_pos = NA,
                                     y_pos = NA)
            
        } else {
            x_pos <- grepl("x", names(xml_i$cubic_bezier))
            x_pos <- as.numeric(unlist(xml_i$cubic_bezier[x_pos]))
            y_pos <- grepl("y", names(xml_i$cubic_bezier))
            y_pos <- as.numeric(unlist(xml_i$cubic_bezier[y_pos]))
            poly_coord <- data.frame(X_pos = paste(x_pos, collapse = ","),
                                     Y_pos = paste(y_pos, collapse = ","))
        }
        df <- data.frame(FileName = img_fn, 
                         Label = label,
                         Bndbox = t(box_coord),
                         Polygon = poly_coord)
        if(poly_cut & !is.null(xml_i$cubic_bezier)){
            ii <- image_info(img)
            img_d <- image_draw(image_blank(ii$width, ii$height))
            polygon(x = x_pos, y = y_pos, col = "black")
            dev.off()
            outimg <- image_composite(img, img_d, operator = "copyopacity")
            outimg <- image_crop(outimg,
                                 geometry_area(df$Bndbox.Xmax - df$Bndbox.Xmin,
                                               df$Bndbox.Ymax - df$Bndbox.Ymin,
                                               df$Bndbox.Xmin,
                                               df$Bndbox.Ymin))
            image_write(image_background(outimg, "white"),
                        paste(paste(file.path(out_dir, sub("\\..*", "", img_fn)),
                                    df$Bndbox.Xmin,
                                    df$Bndbox.Ymin,
                                    df$Bndbox.Xmax - df$Bndbox.Xmin,
                                    df$Bndbox.Ymax - df$Bndbox.Ymin,
                                    df$Label,
                                    sep = "_"), fmt, sep = "."),
                            fmt, 100)
        } else {
            image_write(image_crop(img,
                                   geometry_area(df$Bndbox.Xmax - df$Bndbox.Xmin,
                                                 df$Bndbox.Ymax - df$Bndbox.Ymin,
                                                 df$Bndbox.Xmin,
                                                 df$Bndbox.Ymin)),
                        paste(paste(file.path(out_dir, sub("\\..*", "", img_fn)),
                                    df$Bndbox.Xmin,
                                    df$Bndbox.Ymin,
                                    df$Bndbox.Xmax - df$Bndbox.Xmin,
                                    df$Bndbox.Ymax - df$Bndbox.Ymin,
                                    df$Label,
                                    sep = "_"), fmt, sep = "."),
                        fmt, 100)
        }
        out_df <- rbind(out_df, df)
    }
    write.csv(out_df, file.path(out_dir,
                                paste0(sub("\\..*", "", img_fn), 
                                       "croppedObjects.csv")),
              row.names = FALSE)
    cls <- table(out_df$Label)
    invisible(cls)
}
