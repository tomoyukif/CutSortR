# CutSortR

## Installation
You need the following packages installed.
```
if( !require(shinyFiles, quietly = TRUE) ){
  install.packages("shiny")
}
if( !require(shinyFiles, quietly = TRUE) ){
  install.packages("shinyFiles")
}
if( !require(shinyFiles, quietly = TRUE) ){
  install.packages("shinyFeedback")
}
if( !require(shinyFiles, quietly = TRUE) ){
  install.packages("magick")
}
if( !require(shinyFiles, quietly = TRUE) ){
  install.packages("jsonlite")
}
```

You can install `CutSortR` from the GitHub repository.
```
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("tomoyukif/CutSortR", dependencies = TRUE)
```

To execute a shinyApp of CutSortR, run the following.
```
library(CutSortR)
cutsortr()
```
