library(car)

npfOrig <- read.csv("npf_train.csv")
```

```{r preprocess data}

# Dataset names:
# npfOrig          - Original data with has class2 as numeric values 0, 1
# npf          - "Original" data: has class2 with values 0,1 as numeric values, 
#                   columns removed: class4, partlybad, date (moved to rowname)
# npf.fact     - Otherwise the same as npf, but class2 is a factor with levels 0 and 1
# npf.168      - Processed data: Parameters that have different measurement heights have been reduced to height 16.8m
#                   columns removed: Parameters with height that's not 16.8m
# npf.168.fact -  Otherwise the same as npf.168, but class2 is a factor
# npfsp        - Reduced set of explanatory variables, received through running VIF analysis and repeating variable with highest VIF until no variable has a VIF
#                 higher than 10
# npfsp.fact   - Otherwise the same as npfsp but class2 is a factor


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
npf.168.fact$class2 <- factor(npf.168.fact$class2)


# Divide numerical training data into two for training & testing
set.seed(42)
idx <- sample(nrow(npf), nrow(npf)/2)
npf_train <- npf[idx, ]
npf_test <- npf[-idx, ]

testmod<-glm(class2~., npf.168,family=binomial(link=logit))
inflfact<-vif(testmod)
max(inflfact)
which(inflfact==max(inflfact))

testmod<-glm(class2~.-PAR.mean-PAR.std, npf.168,family=binomial(link=logit))
inflfact<-vif(testmod)
max(inflfact)
which(inflfact==max(inflfact))

testmod<-glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean, npf.168,family=binomial(link=logit))
inflfact<-vif(testmod)
max(inflfact)
which(inflfact==max(inflfact))

testmod<-glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean-Glob.std-Glob.mean, npf.168,family=binomial(link=logit))
inflfact<-vif(testmod)
max(inflfact)
which(inflfact==max(inflfact))

testmod<-glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean-Glob.std-Glob.mean-UV_B.std-UV_B.mean, npf.168,family=binomial(link=logit))
inflfact<-vif(testmod)
max(inflfact)
which(inflfact==max(inflfact))

testmod<-glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean-Glob.std-Glob.mean-UV_B.std-UV_B.mean-RPAR.mean-RPAR.std, npf.168,family=binomial(link=logit))
inflfact<-vif(testmod)
max(inflfact)
which(inflfact==max(inflfact))

testmod <- glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean-Glob.std-Glob.mean-UV_B.std-UV_B.mean-RPAR.mean-RPAR.std-NET.mean-NET.std, npf.168,family=binomial(link=logit))
inflfact <- vif(testmod)
max(inflfact)
which(inflfact == max(inflfact))

testmod<-glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean-Glob.std-Glob.mean-UV_B.std-UV_B.mean-RPAR.mean-RPAR.std-NET.mean-NET.std-RGlob.std-RGlob.mean, npf.168,family=binomial(link=logit))
inflfact <- vif(testmod)
max(inflfact)
which(inflfact == max(inflfact))

testmod <- glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean-Glob.std-Glob.mean-UV_B.std-UV_B.mean-RPAR.mean-RPAR.std-NET.mean-NET.std-RGlob.std-RGlob.mean-CS.mean-CS.std, npf.168,family=binomial(link=logit))
inflfact <- vif(testmod)
max(inflfact)
which(inflfact == max(inflfact))

testmod <- glm(class2~.-PAR.mean-PAR.std-UV_A.std-UV_A.mean-Glob.std-Glob.mean-UV_B.std-UV_B.mean-RPAR.mean-RPAR.std-NET.mean-NET.std-RGlob.std-RGlob.mean-CS.mean-CS.std-NOx168.mean-NOx168.std, npf.168,family=binomial(link=logit))
inflfact <- vif(testmod)
max(inflfact)
which(inflfact==max(inflfact))

#Thus, we come up to the following collection of variables
coefficients(testmod)
colnames(npf)
npfsp<-npf.168[,c(1:2,5:6,9:10,13:16,19:20,23:24,27:30,37)]
colnames(npfsp)
npfsp.fact<-npfsp
npfsp.fact[,19]<-as.factor(npfsp.fact[,19])
```

### PCA
#PCA for all three versions of data 
```{r PCA}
pcaOrig <- prcomp(npf[,1:100], center = T, scale.=T)
cumsum(pcaOrig$sdev^2)/sum(pcaOrig$sdev^2)
x1 <- as.matrix(npf[,1:100])%*%pcaOrig$rotation[,1:12]
pca.168 <- prcomp(npf.168[,1:36], center = T, scale. = T)
cumsum(pca.168$sdev^2)/sum(pca.168$sdev^2)
x2 <- as.matrix(npf.168[,1:36])%*%pca.168$rotation[,1:12]
pca.sp <- prcomp(npfsp[,1:18], center = T, scale. = T)
cumsum(pca.sp$sdev^2)/sum(pca.sp$sdev^2)
x3 <- as.matrix(npfsp[,1:18])%*%pca.sp$rotation[,1:11]
```

library(glmnet)

### Lasso (first version of final model)
```{r lasso2}
lasso1 <- cv.glmnet(x = as.matrix(npf[,1:100]), y = npf$class2, alpha=1, family="binomial", type.measure="class", nfolds=10)
lassomod1 <- glmnet(x=as.matrix(npf[,1:100]),y = npf$class2, alpha = 1, lambda = lasso1$lambda.min, family="binomial", type.measure="class")
preds<-c()
perps<-c()
for(i in 1:464){
  newexp <-npf[-i,]
  newmod <-glmnet(x = as.matrix(newexp[,1:100]), y=newexp[,101], alpha = 1,family="binomial", lambda = lasso1$lambda.min)
  newprob <-predict(newmod, newx=as.matrix(npf[i,1:100]),type="response")
  if(npf$class2[i] == 1) {
    newperp = log(newprob)
  } else {
    newperp = log(1-newprob)
  }
  perps <- c(perps,newperp)
  if (newprob > 0.5) {
    newpred <- 1
  } else{
    newpred <- 0
  }
  preds <- c(preds, newpred == npfsp$class2[i])
}
mean(preds == npf$class2)
exp(-mean(perps))

### GLM (first version of actual model without usage of PCAs):
logreg <- glm(class2~., npfsp, family=binomial())
logreg <- glm(class2~.-O3168.mean-O3168.std, npfsp, family=binomial())
logreg <- glm(class2~.-O3168.mean-O3168.std-PTG.mean-PTG.std, npfsp, family=binomial())
logreg <- glm(class2~.-O3168.mean-O3168.std-PTG.mean-PTG.std-Pamb0.mean-H2O168.std-SO2168.mean-SO2168.std-1, npfsp, family=binomial())
preds<-c()
perps<-c()
for (i in 1:464) {
  newexp <- npfsp[-i,]
  newmod <- glm(class2~.-O3168.mean-O3168.std-PTG.mean-PTG.std-Pamb0.mean-H2O168.std-SO2168.mean-SO2168.std-1, data=npfsp, family=binomial())
  newprob <- predict(newmod, newdata=npfsp[i,], type="response")
  if (npf$class2[i] == 1) {
    newperp = log(newprob)
  } else {
    newperp = log(1-newprob)
  }
  perps <- c(perps,newperp)
  if (newprob > 0.5) {
    newpred <- 1
  } else {
    newpred <- 0
  }
  preds <- c(preds, newpred == npfsp$class2[i])
}
mean(preds)
exp(-mean(perps))

### SVM (first version of actual model)
tc <- tune.control(cross=464)
tuner <- tune(svm.class2~., data=npf.168.fact, kernel="linear", type="C", ranges=list(cost=seq(0.01,0.05,0.01)), tunecontrol=tc)
bpam <- tuner$best.parameters
preds <- c()
perps<-c()
for(i in 1:464){
  newexp <- npf.168.fact[-i,]
  newmod <- svm(class2~., newexp,kernel="linear", cost=bpam[1,1], scale=F, probability=T, type="C")
  newprob <- predict(newmod, newdata=npf.168.fact[i,1:36], probability=T)
  if (class2[i] == 1) {
    newperp <- attr(newprob, "probabilities")[1,1]
  } else {
    newperp <- attr(newprob, "probabilities")[1,2]
  }
  perps <- c(perps,log(newperp))
  preds <- c(preds, newprob[1])
}
mean((preds-1) == class2)
exp(-mean(perps))