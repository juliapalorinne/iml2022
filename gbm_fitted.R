library(gbm) # GBM
set.seed(42)
## Accuracy
accuracy <- function(pred, y) mean(ifelse(pred >= 0.5, 1, 0) == y)
## Multiaccuracy
multiaccuracy <- function(pred, y) mean(colnames(pred)[apply(pred,1,which.max)] == y)
## Perplexity
## Force probabilities to [0.995, 0.005]
perplexity <- function(pred, y) {
  pred <- ifelse(pred != 0 & pred != 1, pred, ifelse(pred == 0, 0.005, 0.995 ))
  exp(-mean(log(ifelse(y == 1, pred, 1 - pred)))) 
}

npfOrig <- read.csv("npf_train.csv")
npfOrig$class2 <- ifelse(npfOrig$class4 == "nonevent", 0, 1)
rownames(npfOrig) <- npfOrig[, "date"]

# Select global and 16.8 m high values
npf.168 <- data.frame(npfOrig[, (5:6)], npfOrig[, (13:16)], npfOrig[, (27:30)], 
                      npfOrig[, (41:42)], npfOrig[, (53:54)], npfOrig[, (63:72)],
                      npfOrig[, (83:88)], npfOrig[, (99:105)])

# Remove columns id, date, class4, partlybad from npf
npf <- npfOrig[, -(1:4)]

# Create datasets where class2 is a factor
npf.fact <- npf
npf.fact$class2 <- factor(npf.fact$class2)
npf.168.fact <- npf.168
npf.168.fact$class2 <- factor(npf.168.fact$class2)


# Divide numerical training data into two for training & testing
idx <- sample(nrow(npf), nrow(npf)/2)
npfO_train <- npf[idx, ]
npfO_test <- npf[-idx, ]


idx <- sample(nrow(npf.168.fact), nrow(npf.168.fact)/2)
npf.168f_train <- npf.168.fact[idx, ]
npf.168f_test <- npf.168.fact[-idx, ]

idx <- sample(nrow(npfsp.fact), nrow(npfsp.fact)/2)
npfsp_train <- npfsp[idx, ]
npfsp_test <- npfsp[-idx, ]


# MODEL WITH ORIGINAL DATA

gbm.modO <- gbm(class2 ~ ., data = npfO_train, distribution = "bernoulli", n.trees = 500)
gbm.predO <- predict(gbm.modO, newdata = npfO_test, n.trees = 100, type = "response")

# loocv
n = nrow(npf)
npfO.gbm.LOOCV <- rep(0, n)
for (i in 1:dim(npf)[1]) {
  mod <- gbm(class2 ~ ., data = npf[-i, ], distribution = "bernoulli", n.trees = 500)
  npfO.gbm.LOOCV[i] <- predict(mod, npf[i, ], n.trees = 100, type = "response")
  print(i)
}

accuraciesO <- data.frame(original = accuracy(gbm.predO, npfO_test$class2),
                         LOOCV = accuracy(npfO.gbm.LOOCV, npf$class2))
perplexitiesO <- data.frame(original = perplexity(gbm.predO, npfO_test$class2),
                           LOOCV = perplexity(npfO.gbm.LOOCV, npf$class2))
tableO <- cbind(rbind(accuraciesO), rbind(perplexitiesO))
tableO


# MODEL WITH 168 DATA

idx <- sample(nrow(npf.168), nrow(npf.168)/2)
npf.168_train <- npf.168[idx, ]
npf.168_test <- npf.168[-idx, ]

gbm.mod.168 <- gbm(class2 ~ ., data = npf.168_train, distribution = "bernoulli", n.trees = 500)
gbm.pred.168 <- predict(gbm.mod.168, newdata = npf.168_test, n.trees = 100, type = "response")

# loocv
npf.168.gbm.LOOCV <- rep(0, n)
for (i in 1:dim(npf.168)[1]) {
  mod <- gbm(class2 ~ ., data = npf.168[-i, ], distribution = "bernoulli", n.trees = 500)
  npf.168.gbm.LOOCV[i] <- predict(mod, npf.168[i, ], n.trees = 100, type = "response")
  print(i)
}

accuracies168 <- data.frame(original = accuracy(gbm.pred.168, npf.168_test$class2),
                         LOOCV = accuracy(npf.168.gbm.LOOCV, npf.168$class2))
perplexities168 <- data.frame(original = perplexity(gbm.pred.168, npf.168_test$class2),
                           LOOCV = perplexity(npf.168.gbm.LOOCV, npf.168$class2))
table168 <- cbind(rbind(accuracies168), rbind(perplexities168))
table168




# WITH ORIGINAL TEST DATA
npf_test <- read.csv("npf_test.csv")
npf_test2 <- npf_test
npf_test2$class2 <- ifelse(npf_test$class4 == "nonevent", 0, 1)
rownames(npf_test2) <- npf_test[, "date"]

gbm.test.pred <- predict(gbm.mod, newdata = npf_test2, n.trees = 200, type = "response")
gbm.test.pred

accuraciesTest <- data.frame(test = accuracy(gbm.test.pred, npf_test2$class2))
perplexitiesTest <- data.frame(test = perplexity(gbm.test.pred, npf_test2$class2))
tableTest <- cbind(rbind(accuraciesTest), rbind(perplexitiesTest))
tableTest



# WITH TEST DATA AND 168
npf.test.168 <- data.frame(npf_test2[, (5:6)], npf_test2[, (13:16)], npf_test2[, (27:30)], 
                           npf_test2[, (41:42)], npf_test2[, (53:54)], npf_test2[, (63:72)],
                           npf_test2[, (83:88)], npf_test2[, (99:105)])

gbm.test.168.pred <- predict(gbm.mod.168, newdata = npf.test.168, n.trees = 200, type = "response")

accuraciesTest168 <- data.frame(test.168 = accuracy(gbm.test.168.pred, npf.test.168$class2))
perplexitiesTest168 <- data.frame(test.168 = perplexity(gbm.test.168.pred, npf.test.168$class2))
tableTest168 <- cbind(rbind(accuraciesTest168), rbind(perplexitiesTest168))
tableTest168



# MODEL WITH 4 CLASSES
npf4 <- npfOrig[,-c(1,2,4)]
npf_test4 <- npf_test[,-c(1,2,4)]
npf4.168 <- data.frame(npf4[, (1:3)], npf4[, (10:13)], npf4[, (24:27)], 
                       npf4[, (38:39)], npf4[, (50:51)], npf4[, (60:69)],
                       npf4[, (80:85)], npf4[, (96:101)])
rownames(npf4) <- npfOrig[, "date"]
rownames(npf_test4) <- npf_test[, "date"]

idx <- sample(nrow(npf4), nrow(npf4)/2)
npf4_train <- npf4[idx, ]
npf4_test <- npf4[-idx, ]

gbm.mod4 <- gbm(class4 ~ ., data = npf4_train, distribution = "multinomial", n.trees = 500)
gbm.pred4 <- predict(gbm.mod4, newdata = npf4_test, n.trees = 100, type = "response")
multiaccuracy(gbm.pred4, npf4_test$class4)

gbm.test.mod4 <- gbm(class4 ~ ., data = npf4, distribution = "multinomial", n.trees = 500)
gbm.test.pred4 <- predict(gbm.test.mod4, newdata = npf_test4, n.trees = 100, type = "response")
multiaccuracy(gbm.test.pred4, npf_test4$class4)


# loocv
npf4.gbm.LOOCV <- rep(0, n)
for (i in 1:dim(npf)[1]) {
  mod <- gbm(class2 ~ ., data = npf4[-i, ], distribution = "multinomial", n.trees = 500)
  npf4.gbm.LOOCV[i] <- predict(mod, npf4[i, ], n.trees = 100, type = "response")
  print(i)
}

accuracies4 <- data.frame(original = multiaccuracy(gbm.pred4, npf4_test$class4),
                                LOOCV = multiaccuracy(npf4.gbm.LOOCV, npf4$class4))
perplexities4 <- data.frame(original = perplexity(gbm.pred4, npf4_test$class4),
                                  LOOCV = perplexity(npf4.gbm.LOOCV, npf4$class4))
table4 <- cbind(rbind(accuracies4), rbind(perplexities4))
table4

colnames(gbm.pred4)[apply(gbm.pred4,1,which.max)]


# MODEL WITH 4 CLASSES AND 168 DATA

idx <- sample(nrow(npf4.168), nrow(npf4.168)/2)
npf4.168_train <- npf4.168[idx, ]
npf4.168_test <- npf4.168[-idx, ]

gbm.168.mod4 <- gbm(class2 ~ ., data = npf4.168_train, distribution = "multinomial", n.trees = 500)
gbm.168.pred4 <- predict(gbm.168.mod4, newdata = npf4.168_test, n.trees = 100, type = "response")

gbm.168.test.mod4 <- gbm(class4 ~ ., data = npf4.168, distribution = "multinomial", n.trees = 500)
gbm.168.test.pred4 <- predict(gbm.test.mod4, newdata = npf_test4, n.trees = 100, type = "response")
multiaccuracy(gbm.168.test.pred4, npf_test4$class4)

# loocv
npf4.168.gbm.LOOCV <- rep(0, n)
for (i in 1:dim(npf)[1]) {
  mod <- gbm(class2 ~ ., data = npf4.168[i, ], distribution = "multinomial", n.trees = 500)
  npf4.168.gbm.LOOCV[i] <- predict(mod, npf4.168[i, ], n.trees = 100, type = "response")
  print(i)
}

accuracies4.168 <- data.frame(original = accuracy(gbm.pred4, npf4_test$class2),
                          LOOCV = accuracy(npf4.test.gbm.LOOCV, npf4$class2))
perplexities4.168 <- data.frame(original = perplexity(gbm.pred4, npf4_test$class2),
                            LOOCV = perplexity(npf4.gbm.LOOCV, npf4$class2))
table4 <- cbind(rbind(accuracies4), rbind(perplexities4))
table4