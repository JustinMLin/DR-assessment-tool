library(DRtool)

n <- 50
p <- c(5, 10, 20, 50, 100)
limit <- seq(from=0.1, to=1, by=0.1)
b <- 100

g1 <- 1:n
g2 <- (n+1):(2*n)

power <- matrix(nrow=length(p), ncol=length(limit))
for (i in 1:length(p)) {
  for (j in 1:length(limit)) {
    power[i,j] <- mean(replicate(b, {
      X1 <- cbind(matrix(runif(n, min=-2, max=-limit[j]), nrow=n, ncol=1),
                  matrix(runif(n*(p[i]-1), min=-1, max=1), nrow=n, ncol=p[i]-1))
      X2 <- cbind(matrix(runif(n, min=limit[j], max=2), nrow=n, ncol=1),
                  matrix(runif(n*(p[i]-1), min=-1, max=1), nrow=n, ncol=p[i]-1))
      X <- rbind(X1, X2)

      mst <- get_mst(dist(X))
      crossings <- count_crossings_brush(mst, g1, g2)
      sim <- sim_crossings_brush(X, g1, g2, 100)

      p_val <- mean(sim < crossings)
      if (p_val < 0.05) TRUE else FALSE
    }))
  }
}

###############################################################################

typeIerror <- vector(length=length(p))
for (i in 1:length(p)) {
  error[i] <- mean(replicate(b, {
    X <- cbind(matrix(runif(2*n, min=-2, max=2), nrow=2*n, ncol=1),
               matrix(runif(2*n*(p-1), min=-1, max=1), nrow=2*n, ncol=p-1))

    g1 <- which(X[,1] < 0)
    g2 <- which(X[,1] >= 0)
    mst <- get_mst(dist(X))
    crossings <- count_crossings_brush(mst, g1, g2)
    sim <- sim_crossings_brush(X, g1, g2, 100)

    p_val <- mean(sim < crossings)
    if (p_val < 0.05) TRUE else FALSE
  }))
}

###############################################################################

save(power, error, file="~/assessment tool/power_size_analysis.rda")
