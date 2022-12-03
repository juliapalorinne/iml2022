set.seed(68)
library(tree)
library(ISLR2)

classF <- factor(ifelse(npf$class2 < 1, "No" ,"Yes"))
npfF <- data.frame(npf, classF)

#' Select training and test data
idx <- sample(nrow(npfF), nrow(npfF)/2)
npf_train <- npfF[idx, ]
npf_test <- npfF[-idx, ]
classF.train <- npfF[idx, ]
classF.test <- npfF[-idx, ]

# tree.train <- tree(class2 ~ ., npf_train)
tree.train <- tree(classF ~ . - class2, npf_train)
summary(tree.train)

plot(tree.train)
text(tree.train, pretty = 0)

tree.pred <- predict(tree.train, npf_test, type = "class")
table(tree.pred, npf_test$classF)

set.seed(7)
cv.train <- cv.tree(tree.train, FUN = prune.misclass)
names(cv.train)
cv.train
par(mfrow = c(1, 2))

plot(cv.train$size, cv.train$dev, type = "b")
plot(cv.train$k, cv.train$dev, type = "b")

prune.train <- prune.misclass(tree.train, best = 9)
par(mfrow = c(1, 1))
plot(prune.train)
text(prune.train, pretty = 0)

tree.pred <- predict(prune.train, npf_test, type = "class")
table(tree.pred, npf_test$classF)

prune.train <- prune.misclass(tree.train, best = 14)
plot(prune.train)
# text(prune.train, pretty = 0)
tree.pred <- predict(prune.train, npf_test, type = "class")
table(tree.pred, npf_test$classF)



