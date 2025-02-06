## source("../../Final/DR tool functions final.R")
source("DR tool functions final.R")

n = c(2:50,
      seq(from=55, to=100, by=5),
      seq(from=120, to=200, by=20),
      300, 400, 500)
p = c(2:20,
      seq(from=30, to=100, by=10),
      150, 200)
  
b = 200

vars = matrix(nrow=length(n), ncol=length(p))
for (i in 1:length(n)) {
  for (j in 1:length(p)) {
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
    
    vars[i,j] = var(counts)
    
    print(paste((i-1)*length(p) + j, "/", length(n)*length(p), " loops completed!"))
  }
}

## save(n, p, vars, file="mst_sd_sim_data.Rda")
save(n, p, vars, file="~/1nn/mst_sd_sim_data.Rda")