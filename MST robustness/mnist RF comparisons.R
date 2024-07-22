library(ggplot2)

dists = read.delim("Python experiments/mnist_distances.txt", header=FALSE)$V1
dists_rand = read.delim("Python experiments/mnist_random.txt", header=FALSE)$V1

df = data.frame(dist = c(dists, dists_rand),
                type = c(rep("non-random", length(dists)),
                         rep("random", length(dists_rand))))

ggplot(df, aes(x = type, y = dist, col = type)) +
  geom_boxplot()

cdf = ecdf(dists_rand)
percentiles = cdf(dists)