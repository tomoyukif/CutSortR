library(magick)
make.pollen <- function(n = 1,
                        delta = 0.01,
                        diameter = 100,
                        check = TRUE,
                        min.radius = 0.9,
                        lwd = 2,
                        fill = "black",
                        res = 96){
  out <- NULL
  for(i in seq_len(n)){
    yc <- xc <- 10
    n <- 200
    t = seq(0, 2 * pi, length = n)[1:(n-1)]
    while(TRUE){
      R = NULL
      prev <- 1
      for(i in 1:(n-1)){
        if(prev <= min.radius){
          prev <- prev + runif(1, 0, delta) * 1
        } else if(prev >= 1){
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

    fig <- image_graph(width = res*5, height = res*5, res = res, bg = "transparent")
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

spread.pollen <- function(n_fertie = 10,
                          n_sterile = 10,
                          diameter = 50,
                          delta = 0.01,
                          check = TRUE,
                          lwd = 10,
                          min.radius = 0.9,
                          width = 1000,
                          height = 1000,
                          res = 72,
                          fill = NULL,
                          bg = "white"){
  fertile <- make.pollen(n = n_fertie,
                         delta = delta,
                         diameter = diameter,
                         check = check,
                         min.radius = min.radius,
                         lwd = lwd,
                         fill = "black",
                         res = res)
  sterile <- make.pollen(n = n_sterile,
                         delta = delta,
                         diameter = diameter,
                         check = check,
                         min.radius = min.radius,
                         lwd = lwd,
                         fill = NULL,
                         res = res)
  pollen <- c(fertile, sterile)

  pollen_indices <- seq_along(pollen)
  pollen_indices <- sample(pollen_indices, length(pollen_indices))
  x_pos <- round(runif(length(pollen), 0, width), 0)
  y_pos <- round(runif(length(pollen), 0, height), 0)
  pollen_pos <- paste0("+", x_pos, "+", y_pos)

  out <- image_graph(width = width + diameter*2,
                     height = height + diameter*2,
                     res = res,
                     bg = bg)
  plot.new()
  dev.off()

  for(i in pollen_indices){
    out <- image_composite(out,
                           pollen[i],
                           offset = pollen_pos[i])
  }
  out <- image_crop(out, paste0(width, "x", height, "+", diameter))
  return(out)
}

high_dense <- spread.pollen(n_fertie = 90,
                            n_sterile = 10,
                            width = 1500,
                            height = 1500)
low_dense <- spread.pollen(n_fertie = 45,
                           n_sterile = 5,
                           width = 1500,
                           height = 1500)

image_write(high_dense, "./highDense_psuedoPollen.tiff", "tiff")
image_write(low_dense, "./lowDense_psuedoPollen.tiff", "tiff")
