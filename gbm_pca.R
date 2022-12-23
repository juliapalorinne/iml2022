## choose variables whose names end with ".mean" together with "class4"
vars <- colnames(npf)[sapply(
  colnames(npf),
  function(s) nchar(s) > 5 && substr(s, nchar(s) - 4, nchar(s)) == ".mean"
)]
means2 <- npfOrig[, endsWith(colnames(npfOrig), ".mean")]
means2$class2 <- npf$class2
means4 <- npfOrig[, c(vars, "class4")]
## strip the trailing ".mean" to make the variable names prettier
colnames(means2)[1:length(vars)] <- sapply(
  colnames(means2)[1:length(vars)],
  function(s) substr(s, 1, nchar(s) - 5)
)
colnames(means4)[1:length(vars)] <- sapply(
  colnames(means4)[1:length(vars)],
  function(s) substr(s, 1, nchar(s) - 5)
)
vars <- colnames(means4)[1:length(vars)]
means2
vars
npf.pca <- prcomp(scale(means2[, vars]), center = T, scale = T)
x1 <- as.matrix(means2[, vars]) %*% npf.pca$rotation
means2.pca <- as.data.frame(cbind(x1, means2$class2))
colnames(means2.pca) <- c(vars, "class2")

gbm.mod.pca10 <- gbm(class2 ~ ., data = means2.pca, distribution = "bernoulli", n.trees = 500)


## choose variables whose names end with ".mean" together with "class4"
vars <- colnames(npf)[sapply(
  colnames(npf),
  function(s) nchar(s) > 5 && substr(s, nchar(s) - 4, nchar(s)) == ".mean"
)]
test2 <- npf_test[, endsWith(colnames(npfOrig), ".mean")]
test2$class2 <- npf_test2$class2
test4 <- npf_test[, c(vars, "class4")]
## strip the trailing ".mean" to make the variable names prettier
colnames(test2)[1:length(vars)] <- sapply(
  colnames(test2)[1:length(vars)],
  function(s) substr(s, 1, nchar(s) - 5)
)
colnames(test4)[1:length(vars)] <- sapply(
  colnames(test4)[1:length(vars)],
  function(s) substr(s, 1, nchar(s) - 5)
)
vars <- colnames(test4)[1:length(vars)]
test2
vars
npf.test.pca <- scale(test2[, vars])

n = nrow(npf.test.pca)
npf.test.eig <- data.frame()
npf.test.sum <- data.frame()
for (p in 1:10) {
  for (i in 1:dim(npf.test.pca)[1]) {
    for (j in 1:100) {
      npf.test.eig[i, j] <- npf.test.pca[i, j] * t(gbm.pca10)[p, j]
    }
    npf.test.sum[i, p] <- sum(npf.test.eig[i, (1:j)])
  }
}
npf.test.data <- npf.test.sum
colnames(npf.test.data) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")

gbm.pred.pca10 <- predict(gbm.mod.pca10, newdata = npf.test.data, n.trees = 100, type = "response")
gbm.pred.pca10

accuraciesPca10 <- data.frame(pca10 = accuracy(gbm.pred.pca10, npf_test2$class2))
perplexitiesPca10 <- data.frame(pca10 = perplexity(gbm.pred.pca10, npf_test2$class2))
tablePca10 <- cbind(rbind(accuraciesPca10), rbind(perplexitiesPca10))
tablePca10
