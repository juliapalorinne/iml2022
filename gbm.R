library(gbm) # GBM
set.seed(1)

npfOrig <- read.csv("npf_train.csv")

#' Create class2
npfOrig$class2[npfOrig$class4 == "nonevent"] <- 0
npfOrig$class2[npfOrig$class4 != "nonevent"] <- 1

#' Add date as row name
rownames(npfOrig) <- npfOrig[, "date"]

#' Remove columns 1-4 (id, date, class4, partlybad)
npf <- npfOrig[, -(1:4)]

#' Select training and test data
idx <- sample(nrow(npf), nrow(npf)/2)
npf_train <- npf[idx, ]
npf_test <- npf[-idx, ]

boost.npf <- gbm(class2 ~ ., data = npf_train, 
                        distribution = "bernoulli", n.trees = 5000,
                        interaction.depth = 4)
summary(boost.npf)
plot(boost.npf, i = "RHIRGA84.mean")

