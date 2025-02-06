source("approx E_mst.R")

p = c(2:50,
      seq(from=55, to=200, by=5),
      seq(from=220, to=500, by=20),
      seq(from=550, to=800, by=50))

E_t0 = sapply(p, function(p) E_mst(p, (400)^(1/p), b=200))

df_t0 = data.frame(p=p, E_t0)

write.csv(df_t0, "E_p_t0 data.csv")