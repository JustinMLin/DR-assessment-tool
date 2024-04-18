library(MASS)
library(Rtsne)

source("../Algorithms/assessment tool.r")

Z = rbind(mvrnorm(n=80, mu=c(10,0,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=100, mu=c(0,-10,0,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=100, mu=c(0,0,5,0,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=80, mu=c(0,0,0,8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=70, mu=c(0,0,0,-8,0), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=60, mu=c(0,0,0,0,4), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))),
          mvrnorm(n=130, mu=c(0,0,0,0,-6), Sigma=diag(abs(rnorm(n=5, mean=0, sd=5)))))
Z_dist = dist(Z)

X = Rtsne(Z, perplexit=10)$Y # perplexity too low
X1 = prcomp(Z, center=TRUE, scale.=TRUE, rank.=2)$x
tree = get_mst(dist(Z))
labels = rep(1:7, c(80, 100, 100, 80, 70, 60, 130))
id = 1:620

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