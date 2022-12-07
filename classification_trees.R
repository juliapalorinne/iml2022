# Classification trees
set.seed(68)
library(tree)
library(MASS)
library(randomForest) # random forest
library(gbm) # GBM
library (ISLR2)

npfOrig <- read.csv("npf_train.csv")

#' Create class2
npfOrig$class2[npfOrig$class4 == "nonevent"] <- 0
npfOrig$class2[npfOrig$class4 != "nonevent"] <- 1

#' Add date as row name
rownames(npfOrig) <- npfOrig[, "date"]

#' Remove columns 1-4 (id, date, class4, partlybad)
npf <- npfOrig[, -(1:4)]
print(npf)

#' Select training and test data
idx <- sample(nrow(npf), nrow(npf)/2)
npf_train <- npf[idx, ]
npf_test <- npf[-idx, ]

#' Root mean squared error
rmse <- function(yhat, y) sqrt(mean((y - yhat)**2))

#' Classification accuracy
cacc <- function(pred, true) sum(as.integer(as.logical(pred == true)))/length(pred)

#' Splitting function (from week 1 problem 2)
kpart <- function(n, k) {
  rep_len(1:k, length.out = n)
}

#' Cross-validation predictor (from week 1 problem 2)¨
cv <- function(
    formula, # variables to use
    data, # data
    model = lm, # model to train
    n = nrow(data), # rows in the matrix
    k = min(n, 10), # cross-validation folds
    split = kpart(n, k), # the split of n data items into k folds
    train = function(data) model(formula, data = data),
    pred = function(model, data) predict(model, newdata = data)) {
  yhat <- NULL
  for (i in 1:k) {
    mod <- train(data[split != i, ])
    if (is.null(yhat)) {
      yhat <- pred(mod, data)
    } else {
      yhat[split == i] <- pred(mod, data[split == i, ])
    }
  }
  return(yhat)
}

loocv <- function(
    formula, # Formula specifying which variables to use
    data, # Dataset
    model = lm, # Type of model to train (as a function)
    ## function to train a model on data
    train = function(data) model(formula, data = data),
    ## function to make predictions on the trained model
    pred = function(model, data) predict(model, newdata = data)) {
  yhat <- NULL
  for (i in 1:nrow(data)) {
    ## go through all stations, train on other stations, and make a prediction
    mod <- train(data[-i, ])
    if (is.null(yhat)) {
      ## initialise yhat to something of correct data type,
      yhat <- pred(mod, data)
    } else {
      yhat[i] <- pred(mod, data[i, ])
    }
  }
  yhat # finally, output cross-validation predictions
}


#' Dummy model (from week 1 problem 2)
dummy <- function(formula, data) {
  target <- all.vars(formula[[2]])
  lm(as.formula(sprintf("%s ~ 1", target)), data)
}

#' Models
models <- list(
  Dummy = dummy,
  RandomForest = randomForest,
  GBM = gbm
)

# gmb_pred <- predict(gbm(class2 ~ ., data = npf, cv.folds = 10, train.fraction = 0.5), data=npf)
# print(cacc(gmb_pred, npf$class2))


#' Errors
errors <- sapply(models, function(model) {
  mod <- model(class2 ~ ., data = npf_train)
  c(
    c(
      train = rmse(predict(mod, data = npf_train, ), npf_train$class2),
      test = rmse(predict(mod, data = npf_test), npf_test$class2),
      CV = rmse(cv(mod, data = npf_train), npf_train$class2),
      LOOCV = rmse(loocv(mod, data = npf_train), npf_train$class2) 
    )
  )
})

#' Classification accuracies
results <- sapply(models, function(model) {
  mod <- model(class2 ~ ., data = npf_train)
  c(
    c(
      train = accuracy(predict(mod, data = npf_train), npf_train$class2),
      test = accuracy(predict(mod, data = npf_test), npf_test$class2),
      CV = accuracy(cv(mod, data = npf_train), npf_train$class2),
      LOOCV = accuracy(loocv(mod, data = npf_train), npf_train$class2)
    )
  )
})

#' Result tables
knitr::kable(t(errors), "simple", digits = 3, caption = "Error")
knitr::kable(t(results), "simple", caption = "Model accuracy")

