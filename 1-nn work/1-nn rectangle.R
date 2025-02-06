source("1-nn functions generalized.R")

E_rectangle = function(var_ratio, n, keep=1) {
  var_ratio = sort(var_ratio, decreasing=TRUE)
  
  trunc_var_ratio = var_ratio[cumsum(var_ratio)/sum(var_ratio) <= keep]
  
  p = length(trunc_var_ratio)
  k = (n/prod(trunc_var_ratio))^(1/p)
  
  t = k*trunc_var_ratio[1]
  
  n*prob_p(p, t)
}