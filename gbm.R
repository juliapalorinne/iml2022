library(gbm) # GBM
set.seed(1)
npfOrig <- read.csv("npf_train.csv")
npfOrig$class2 <- ifelse(npfOrig$class4 == "nonevent", 0, 1)
rownames(npfOrig) <- npfOrig[, "date"]
npf.168 <- data.frame(npfOrig[, (5:6)], npfOrig[, (13:16)], npfOrig[, (27:30)], 
                      npfOrig[, (41:42)], npfOrig[, (53:54)], npfOrig[, (63:72)],
                      npfOrig[, (83:88)], npfOrig[, (99:105)])

npfsp <- npf.168[, c(1:2, 5:6, 9:10, 13:16, 19:20, 23:24, 27:30, 37)]
colnames(npfsp)
npfsp.fact <- npfsp
npfsp.fact[, 19] <- as.factor(npfsp.fact[, 19])

npf4<-npfOrig[,-c(1,2,4)]
npf4.168 <- data.frame(npf4[, (1:3)], npf4[, (10:13)], npf4[, (24:27)], 
                       npf4[, (38:39)], npf4[, (50:51)], npf4[, (60:69)],
                       npf4[, (80:85)], npf4[, (96:101)])
npf4sp<-npf4.168[,c(1,2:3,6:7,10:11,14:17,20:21,24:25,28:31)]

idx <- sample(nrow(npfsp.fact), nrow(npfsp.fact)/2)
npf_train <- npfsp[idx, ]
npf_test <- npfsp[-idx, ]

gbm.mod <- gbm(class2 ~ ., data=npf_train, distribution = "bernoulli", n.trees = 5000)
gbm.pred <- predict(gbm.mod, newdata=npf_test, n.trees = 100, type = "response")

gbm.pred

n = nrow(npfsp)
npf.gbm.LOOCV <- rep(0, n)

# loocv
for (i in 1:dim(npfsp)[1]) {
  mod <- gbm(class2 ~ ., data=npf_train, distribution = "bernoulli", n.trees = 5000)
  npf.gbm.LOOCV[i] <- predict(mod, npfsp[i, ], n.trees = 5000, type = "response")
  print(i)
}

accuracies <- data.frame(original = accuracy(gbm.pred, npf_test$class2),
                         LOOCV = accuracy(npf.gbm.LOOCV, npfsp$class2))
perplexities <- data.frame(original = perplexity(gbm.pred, npf_test$class2),
                           LOOCV = perplexity(npf.gbm.LOOCV, npfsp$class2))
table <- cbind(rbind(accuracies), rbind(perplexities))

knitr::kable(table, digits = 3) %>%
  add_header_above(c("Accuracy" = 2, "Perplexity" = 2)) %>%
  kable_styling(latex_options = "HOLD_position")





accuracy <- function(pred, y) mean(ifelse(pred >= 0.5, 1, 0) == y)
accuracy(gbm.pred, npf_test$class2)

# FACTORED

# Select training and test data
idx <- sample(nrow(npf.168.fact), nrow(npf.168.fact)/2)
npf_trainF <- npf.168.fact[idx, ]
npf_testF <- npf.168.fact[-idx, ]

boost.npfF <- gbm(class2~., data=npf_trainF, 
                 distribution="bernoulli", n.trees=100, type = "class")
summary(boost.npfF, xlim=c(0,1))
# plot(boost.npf, i=c("RHIRGA168.mean"))
# plot(boost.npf, i=c("RHIRGA168.mean", "H2O168.mean"))

gbm.predF <- predict(boost.npfF, newdata=npf_testF, n.trees=100)
mean(gbm.predF == npf_testF$class2)


LOOCV.gbm = accuracy(loocv(boost.npf, data=npf_test168), npf_test168$class2)
LOOCV.gbm

