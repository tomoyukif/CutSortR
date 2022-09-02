makeTiles <- function(fn, width, height, step, fmt, out_dir, out){
  img <- image_read(fn)
  info <- image_info(img)
  
  if(out_dir == ""){out_dir <- "./"}
  dir.create(out_dir, showWarnings = FALSE)
  x <- 0
  y <- 0
  while(TRUE){
    image_write(image_crop(img, geometry_area(width, height, x, y)),
                paste(out_dir, 
                      paste(paste(out, x, y, sep = "_"), 
                            fmt, sep = "."),
                      sep = "/"),
                fmt, 100, 16)
    if(x + width < info$width){
      x <- x + step
      
    } else {
      x <- 0
      if(y + height < info$height){
        y <- y + step
      } else {
        break
      }
    }
  }
}

library(magick)
fn <- "sampleImage/RED_line_pilot_3C/Red2-10-(1)_248.jpg"
width <- 100
height <- 100
step <- 80
fmt <- sub(".*\\.", "", fn)
out <- "output"
out_dir <- "tiling"
makeTiles(fn, width, height, step, fmt, out_dir, out)
