library(MASS)
library(Rtsne)

source("../Algorithms/assessment tool.r")

Z = rbind(mvrnorm(n=30, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=20, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=50, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=40, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=25, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=30, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))),
          mvrnorm(n=15, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=10)))))

X = Rtsne(Z)$Y
X1 = prcomp(Z, center=TRUE, scale.=TRUE, rank.=2)$x
tree = get_mst(dist(Z))
labels = rep(1:7, c(30, 20, 50, 40, 25, 30, 15))
id = 1:210