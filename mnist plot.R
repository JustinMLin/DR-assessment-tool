library(dslabs)

mnist = read_mnist()

plot_image = function(i) image(1:28, 1:28, matrix(mnist$train$images[i,], nrow=28)[ , 28:1], col = gray(seq(0, 1, 0.05)))