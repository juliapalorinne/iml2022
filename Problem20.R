

### Task A

npf.pca <- prcomp(means4[, vars], center = T, scale. = T)
cumsum(npf.pca$sdev^2)/sum(npf.pca$sdev^2)
x1 <- as.matrix(means4[, vars]) %*% npf.pca$rotation

means4.pca <- as.data.frame(cbind(x1, means4$class4))
colnames(means4.pca) <- c(vars)
plot(means4.pca)
