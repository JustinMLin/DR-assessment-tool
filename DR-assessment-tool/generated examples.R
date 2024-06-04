library(MASS)
library(Rtsne)

set.seed(4323)
source("../Algorithms/assessment tool.r")

Z = rbind(mvrnorm(n=40, mu=c(10,0,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=200, mu=c(0,-10,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=100, mu=c(0,0,5,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=80, mu=c(0,0,0,8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=30, mu=c(0,0,0,-8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=60, mu=c(0,0,0,0,4), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=130, mu=c(0,0,0,0,-6), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))))
Z_dist = dist(Z)

X = Rtsne(Z, perplexit=10)$Y # perplexity too low
X1 = prcomp(Z, center=TRUE, scale.=TRUE, rank.=2)$x
tree = get_mst(dist(Z))
labels = rep(1:7, c(40, 200, 100, 80, 30, 60, 130))
labels_calc = kmeans(X, centers=7, iter.max=50, nstart=100)$cluster
id = 1:nrow(Z)

##########################

Z = rbind(mvrnorm(n=40, mu=c(4,0), Sigma=diag(c(2,2))),
          mvrnorm(n=40, mu=c(-4,0), Sigma=diag(c(2,2))))
Z_dist = dist(Z)
X = Z
tree = get_mst(dist(Z))
labels = rep(1:2, c(40, 40))
id = 1:80

###########################

p = 10
Z = rbind(mvrnorm(n=40, mu=c(4, rep(0, p-1)), Sigma=diag(rep(2, p))),
          mvrnorm(n=40, mu=c(-4, rep(0, p-1)), Sigma=diag(rep(2, p))))
Z_dist = dist(Z)
X = Rtsne(Z, perplexit=20)$Y
tree = get_mst(dist(Z))
labels = rep(1:2, c(40, 40))
id = 1:80