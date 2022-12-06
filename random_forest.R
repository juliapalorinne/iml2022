library(randomForest)
set.seed(1)
npf.RF <- randomForest (class2 ~ ., data = npf_train, 
                        mtry = 12, importance = TRUE)
npf.RF

yhat.RF <- predict(npf.RF, newdata = npf_test)
plot(yhat.RF, npf_test$class2)
length(yhat.RF)
length(t(npf_test))

mean((yhat.RF - npf_test)^2)
