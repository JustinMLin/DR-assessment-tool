source("Algorithms/DR assessment tool algs.R")
source("Algorithms/graph algs.R")

load("../Data/avg-linkage data.Rda")

g = Z_sl

same_cluster = c(48137, 27493, 58917, 21849, 18591, 3199, 15134, 57811, 21954, 12691, 9875, 23981, 25804, 6608, 24630, 32532)

false_ = c()

id = sapply(same_cluster,
            function(x) {
              which(df_long$id == x)
            })

plot_tree_weights(get_subtree(g, id))
