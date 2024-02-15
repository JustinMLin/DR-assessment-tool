library(MASS)
library(Rtsne)

source("../Algorithms/assessment tool.r")

Z = rbind(mvrnorm(n=40, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=15)))),
          mvrnorm(n=50, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=15)))),
          mvrnorm(n=50, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=15)))),
          mvrnorm(n=40, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=15)))),
          mvrnorm(n=35, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=15)))),
          mvrnorm(n=30, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=15)))),
          mvrnorm(n=65, mu=runif(n=5, min=-10, max=10), Sigma=diag(abs(rnorm(n=5, mean=0, sd=15)))))

X = Rtsne(Z, perplexit=10)$Y # perplexity too low
X1 = prcomp(Z, center=TRUE, scale.=TRUE, rank.=2)$x
tree = get_mst(dist(Z))
labels = rep(1:7, c(40, 50, 50, 40, 35, 30, 65))
id = 1:310