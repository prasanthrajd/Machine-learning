install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")
image_dir = "C:/Users/dprra/Desktop/Advanced Data Mining/blood-cells/TRAIN"
source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)
example_image <- readImage(file.path(image_dir, "_0_207.jpeg"))
display(example_cat_image)

install.packages("tensorflow")
library(tensorflow)
install_tensorflow()
install.packages('keras')
library(keras)

