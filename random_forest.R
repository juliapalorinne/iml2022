library(randomForest)
set.seed(1)
npf.RF <- randomForest (class2 ~ ., data = npf_train, 
                        mtry = 12, importance = TRUE)
npf.RF

yhat.RF <- predict(npf.RF, newdata = npf_test)
plot(yhat.RF, npf_test$class2)

# Select training and test data
idx <- sample(nrow(npf.168), nrow(npf.168)/2)
npf_train168 <- npf.168[idx, ]
npf_test168 <- npf.168[-idx, ]

# Select training and test data with factors
idx <- sample(nrow(npf.168.fact), nrow(npf.168.fact)/2)
npf_trainF168 <- npf.168.fact[idx, ]
npf_testF168 <- npf.168.fact[-idx, ]

npf.RF.mod <- randomForest(class2 ~., data=npf_train168, mtry=12, importance=TRUE)
npf.RF.pred <- predict(npf.RF.mod, newdata = npf_test168)

accuracy <- function(pred, y) mean(ifelse(pred >= 0.5, 1, 0) == y)
accuracy(npf.RF.pred, npf_test168$class2)

LOOCV.RF = accuracy(loocv(npf.RF.mod, data=npf_test168), npf_test168$class2)
LOOCV.RF
