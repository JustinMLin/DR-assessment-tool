library(ggplot2)

dists = read.delim("Python experiments/mnist_distances.txt", header=FALSE)$V1
dists_rand = read.delim("Python experiments/mnist_random.txt", header=FALSE)$V1

df = data.frame(dists)
df_rand = data.frame(dists_rand)

ggplot() +
  geom_histogram(data=df, aes(x=dists, y = ..count../sum(..count..), color="a"), fill="white", alpha=0.6, bins=20) +
  geom_histogram(data=df_rand, aes(x=dists_rand, y = ..count../sum(..count..), color="b"), fill="white", alpha=0.6, bins=20) +
  scale_color_manual(name="Experiment", 
                     values=c("a"="#F8766D", "b"="#00BFC4"), 
                     labels=c("Random Noise", "Null Distribution")) +
  labs(x="Robinson-Foulds Distance", y="Proportion", title="MST Stability on MNIST Data Set")

cdf = ecdf(dists_rand)
percentiles = cdf(dists)