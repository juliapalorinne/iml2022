library(gbm) # GBM
set.seed(1)

npfOrig <- read.csv("npf_train.csv")

# Create class2, 0 = nonevent, 1 = event
npfOrig$class2 <- ifelse(npfOrig$class4 == "nonevent", 0, 1)

# Add date as row name
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
npf.168.fact$class2 <- as.factor(npf.168.fact$class2)


# Select training and test data
idx <- sample(nrow(npf.168), nrow(npf.168)/2)
npf_train168 <- npf.168[idx, ]
npf_test168 <- npf.168[-idx, ]

boost.npf <- gbm(class2~., data=npf_train168, 
                        distribution="bernoulli", n.trees=5000)
summary(boost.npf)
# plot(boost.npf, i=c("RHIRGA168.mean"))
# plot(boost.npf, i=c("RHIRGA168.mean", "H2O168.mean"))

gbm.pred <- predict(boost.npf, newdata=npf_test168, n.trees=5000, type="response")

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

