library(dslabs)
library(matrixStats)
library(ggplot2)
#data("mnist_27")
#mnist <- read_mnist()


 
x <- mnist$train$images
y <- mnist$train$labels
tick <- 0

for (i in 1:60000){
  x_784_grey <- x[i,] >=51 & x[i,] <=204
  tick<- tick+ sum(x_784_grey)
}
print(tick)
2908814/(60000*784)










grid <- matrix(x[784,], 28, 28)
image(1:28, 1:28, grid[,28:1])



#sds <- colSds(x)

#new_x <- x[, colSds(x) > 60]

#qplot(as.vector(x), bins = 30, color=I("black"))
#new_x <- x
#new_x[new_x < 50] <- 0 # changes values that are less that 50 to zero use 

#bin_x <- x
#bin_x[bin_x < 255/2] <- 0
#bin_x[bin_x > 255/2] <- 1

#bin_X <- (x>255/2)*1
