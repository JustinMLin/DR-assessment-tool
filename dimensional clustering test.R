library(ggplot2)
library(dplyr)
library(MASS)

source("Algorithms/dimensional clustering.R")

Z = rbind(mvrnorm(n=5, mu=c(0,0,0,0,0,0,0,0,0,0), Sigma=diag(rep(1,10))),
          mvrnorm(n=10, mu=c(0,-5,0,0,0,0,0,0,0,0), Sigma=diag(rep(1,10))),
          mvrnorm(n=8, mu=c(0,0,3,0,0,0,0,0,0,0), Sigma=diag(rep(1,10))),
          mvrnorm(n=7, mu=c(0,0,0,6,0,0,0,0,0,0), Sigma=diag(rep(1,10))),
          mvrnorm(n=6, mu=c(0,0,0,0,-8,0,0,0,0,0), Sigma=diag(rep(1,10))))