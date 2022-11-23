# Classification trees

set.seed(42)

library(MASS)
library(randomForest) # random forest
library(gbm) # GBM

npf <- read.csv("npf_train.csv")

#' Create class2
npf$class2[npf$class4 == "nonevent"] <- 0
npf$class2[npf$class4 != "nonevent"] <- 1

#' Add date as row name
rownames(npf) <- npf[, "date"]

#' Remove columns 1-4 (id, date, class4, partlybad)
npf <- npf[, -(1:4)]
print(npf)

#' Select training and test data
npf_train <- npf[1:100, ]
npf_test <- npf[101:464, ]

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

#' Errors
errors <- sapply(models, function(model) {
  mod <- model(class2 ~ ., data = npf_train)
  c(
    c(
      train = rmse(predict(mod, newdata = npf_train), npf_train$class2),
      test = rmse(predict(mod, newdata = npf_test), npf_test$class2),
      CV = rmse(cv(class2 ~ ., npf_train, model), npf_train$class2)
    )
  )
})

#' Classification accuracies
results <- sapply(models, function(model) {
  mod <- model(class2 ~ ., data = npf_train)
  c(
    c(
      train = cacc(predict(mod, newdata = npf_train), npf_train$class2),
      test = cacc(predict(mod, newdata = npf_test), npf_test$class2)
    )
  )
})

#' Result tables
knitr::kable(t(errors), "simple", digits = 3)
knitr::kable(t(results), "simple", digits = 3)

