set.seed(68)
library(tree)
library(ISLR2)

classF <- factor(ifelse(npf$class2 < 1, "No" ,"Yes"))
npfF <- data.frame(npf.168, classF)

#' Select training and test data
idx <- sample(nrow(npfF), nrow(npfF)/2)
npf_trainF <- npfF[idx, ]
npf_testF <- npfF[-idx, ]
classF.train <- npfF[idx, ]
classF.test <- npfF[-idx, ]

# tree.train <- tree(class2 ~ ., npf_train)
tree.trainF <- tree(classF ~ . - class2, npf_trainF)
summary(tree.trainF)

plot(tree.trainF)
text(tree.trainF, pretty = 0)

tree.pred <- predict(tree.trainF, npf_testF, type = "class")
table(tree.pred, npf_testF$classF)

set.seed(7)
cv.trainF <- cv.tree(tree.trainF, FUN = prune.misclass)
names(cv.trainF)
cv.trainF
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




# Select training and test data
idx <- sample(nrow(npf.168.fact), nrow(npf.168.fact)/2)
npf_trainF168 <- npf.168.fact[idx, ]
npf_testF168 <- npf.168.fact[-idx, ]
classF.train <- classF[idx, ]
classF.test <- classF[-idx, ]

# tree.train <- tree(class2 ~ ., npf_train)
tree.trainF168 <- tree(class2~., npf_trainF168)
summary(tree.trainF168)

par(mfrow = c(1, 2))
plot(tree.trainF)
text(tree.trainF, pretty = 0)
plot(tree.trainF168)
text(tree.trainF168, pretty = 0)

prune.trainF168 <- prune.misclass(tree.trainF168, best = 8)
plot(prune.trainF168)
# text(prune.train, pretty = 0)
tree.predF168 <- predict(prune.trainF168, npf_testF168, type = "class")
table(tree.predF168, npf_testF168$class2)

mean(tree.predF168 == npf_testF168$class2)

LOOCV.tree = loocv(prune.train, data=npf_test)
LOOCV.tree

