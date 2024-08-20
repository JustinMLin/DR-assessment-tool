runif_ball = function(n, center, radius) {
  m = length(center)
  
  sample = matrix(nrow=n, ncol=m)
  for (i in 1:n) {
    Z = rnorm(m)
    sample[i,] = center + radius * runif(1)^(1/m) * Z/norm(matrix(Z), type='2')
  }
  
  sample
}

runif_ellipse = function(n, center, Sigma) {
  m = length(center)
  
  sample_ball = runif_ball(n, rep(0,m), 1)
  sample = matrix(nrow=n, ncol=m)
  for (i in 1:n) {
    sample[i,] = sample_ball[i,] %*% Sigma + center
  }
  
  sample
}
