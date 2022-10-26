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

When you meet a dependency error in package installation, 
please try installing the package shown in the ERROR message.

The message would be like the following.
```
ERROR: dependencies ‘shiny’ is not available for package ‘CutSortR’
```
Then try
```
install.packages("shiny")
```