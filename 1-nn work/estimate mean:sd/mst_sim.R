## source("../../Final/DR tool functions final.R")
source("DR tool functions final.R")

n = c(10, 20, 50, 100, 200, 300, 400, 500)
p = c(2, 5, 10, 50, 100)
b = 100

means = vars = matrix(nrow=length(n), ncol=length(p))
for (i in 1:length(n)) {
  for (j in 1:length(p)) {
    if (i >= 6 & j >= 5) {
      means[i,j] = NA
      vars[i,j] = NA
    }
    
    else {
      t = n[i]^(1/p[j])
      
      counts = vector(length=b)
      for (k in 1:b) {
        X = matrix(runif(n[i]*p[j], min=-t/2, max=t/2), nrow=n[i], ncol=p[j])
        
        mst = get_mst(dist(X))
        
        count = 0
        for (l in 1:ecount(mst)) {
          head = X[head_of(mst, l),]
          tail = X[tail_of(mst, l),]
          
          if (sign(head[1]) != sign(tail[1])) {
            count = count + 1
          }
        }
        
        counts[k] = count
      }
      
      means[i,j] = mean(counts)
      vars[i,j] = var(counts)
    }
    print(paste((i-1)*length(p) + j, "/", length(n)*length(p), " loops completed!"))
  }
}

## save(n, p, means, vars, file="mst_sim_data.Rda")
save(n, p, means, vars, file="~/1nn/mst_sim_data.Rda")