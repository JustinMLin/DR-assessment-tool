source("../../Final/DR tool functions final.R")
## source("DR tool functions final.R")

p_vals = read.csv("E_p_t0 data.csv")
coef_df = read.csv("fitted parameters.csv")
m = coef_df$Values[1]
k = coef_df$Values[2]
C = coef_df$Values[3]

## E_mst(p, t)
E_mst = function(p, t, b=100) {
  mean(replicate(b, {
    num_pts = rpois(n=1, lambda=t^p)
    
    if (num_pts <= 1) {
      1
    } else {
      X = matrix(runif(num_pts*p, min=-t/2, max=t/2), nrow=num_pts, ncol=p)
      
      mst = get_mst(dist(X))
      
      count = 0
      for (l in 1:ecount(mst)) {
        head = X[head_of(mst, l),]
        tail = X[tail_of(mst, l),]
        
        if (sign(head[1]) != sign(tail[1])) {
          count = count + 1
        }
      }
      
      count
    }
  }))
}

# Simulate E(p, t0)
approx_E_mst = function(p, t, t0=NULL, b=100) {
  if (is.null(t0)) t0 = (200)^(1/p)
  
  (t/t0)^(p-1) * E_mst(p, t0)
}

# Use precalculated approximation of E(p, t0)
approx_E_mst_precalc = function(p, t, t0) {
  E_p_t0 = ifelse(p <= 50, p_vals[p-1,3], -m * p^(-k) + C)
  
  (t/t0)^(p-1) * E_p_t0
}