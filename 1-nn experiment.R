library(dbscan)

# consider [-t/2,t/2]^2
t = 10
A_t = t^2

b = 50000
probs = vector(length=b)
counts = vector(length=b)
for (j in 1:b) {
  num_pts = rpois(n=1, lambda=A_t)
  
  pts_x = runif(n=num_pts, min=-t/2, max=t/2)
  pts_y = runif(n=num_pts, min=-t/2, max=t/2)
  pts = matrix(c(pts_x, pts_y), ncol=2)
  
  neighbors = kNN(pts, k=1)
  
  count = 0
  for (i in 1:nrow(pts)) {
    if (sign(pts[i,1]) != sign(pts[neighbors$id[i,1],1])) {
      count = count + 1
    }
  }
  
  counts[j] = count
  probs[j] = count/nrow(pts)
}