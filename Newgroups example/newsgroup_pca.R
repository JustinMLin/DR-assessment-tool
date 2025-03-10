data <- Matrix::readMM("tfidf_word.mtx")

pca <- irlba::prcomp_irlba(data, n=500)

save(pca, file="~/assessment\ tool/newsgroup_pca.rda")