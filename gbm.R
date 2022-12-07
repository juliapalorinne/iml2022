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
idx <- sample(nrow(npf.168), nrow(npf.168)/2)
npf_train <- npf.168[idx, ]
npf_test <- npf.168[-idx, ]

boost.npf <- gbm(class2 ~ ., data = npf_train, 
                        distribution = "bernoulli", n.trees = 5000,
                        interaction.depth = 4)
summary(boost.npf)
par(mfrow = c(2, 2))
plot(boost.npf, i.var=c("RHIRGA168.mean", "SWS.mean", "H2O168.mean", "CS.mean"))
