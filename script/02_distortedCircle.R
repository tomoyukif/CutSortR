library(magick)
make.pollen <- function(n = 1, 
                        delta = 0.01,
                        diameter = 100, 
                        res = 96, 
                        check = TRUE,
                        low.limit = 0.9,
                        high.limit = 1,
                        lwd = 2,
                        fill = "black"){
  out <- NULL
  for(i in seq_len(n)){
    yc <- xc <- 10
    n <- 200
    t = seq(0, 2 * pi, length = n)[1:(n-1)]
    while(TRUE){
      R = NULL
      prev <- 1
      for(i in 1:(n-1)){
        if(prev <= low.limit){
          prev <- prev + runif(1, 0, delta) * 1
        } else if(prev >= high.limit){
          prev <- prev + runif(1, 0, delta) * -1
        } else {
          prev <- prev + runif(1, 0, delta) * sample(c(1, -1), 1)
        }
        R <- c(R, prev)
      }
      if(check){
        if(abs(head(R, 1) - tail(R, 1)) < delta * 5){
          break
        }
      } else {
        break
      }
    }
    x = xc + R * cos(t)
    y = yc + R * sin(t)
    
    fig <- image_graph(width = 400, height = 400, res = res, bg = "transparent")
    plot.new()
    plot.window(xlim = range(x), ylim = range(y), asp = 1)
    polygon(x, y, col = fill, lwd = lwd)
    dev.off()
    fig <- image_resize(image_trim(fig), paste0(diameter, "x", diameter))
    if(is.null(out)){
      out <- fig
    } else {
      out <- c(out, fig)
    }
  }
  return(out)
}

fertile <- make.pollen(100)
sterile <- make.pollen(100, lwd = 10, fill = NULL)
fertile <- image_animate(fertile, fps = 2)
sterile <- image_animate(sterile, fps = 2)

image_write(fertile, "fertile_pollen.gif")
image_write(sterile, "sterile_pollen.gif")
