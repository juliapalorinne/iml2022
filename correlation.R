library(corrplot)

npfOrig <- read.csv("npf_train.csv")

#' Create class2
npfOrig$class2[npfOrig$class4 == "nonevent"] <- 0
npfOrig$class2[npfOrig$class4 != "nonevent"] <- 1

#' Add date as row name
rownames(npfOrig) <- npfOrig[, "date"]
View(npfOrig)

# Select global and 16.8 m high values
npf <- data.frame(npfOrig[, (5:6)], npfOrig[, (13:16)], npfOrig[, (27:30)], 
                  npfOrig[, (41:42)], npfOrig[, (53:54)], npfOrig[, (63:72)],
                  npfOrig[, (83:88)], npfOrig[, (99:104)])

# Calculate the correlation matrix
cm <- cor(npf)

# Order variables by 1st principal component (PCA later in the course!)
corrplot(cm, order = "FPC", tl.cex = 0.5, tl.col = "black")
corrplot(cm, order = "FPC", tl.cex = 0.5, tl.col = "black")
