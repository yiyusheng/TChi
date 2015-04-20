# use svm to establish models
rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()
library(e1071)
library(gplots)
library(ROCR)
# change wd to read data
setwd(data_dir)
load('TChi_featureA.Rda')

# train
ftr.train <- rbind(cbind(ftr.train_pos,rep(1,dim(ftr.train_pos)[1])),
                   cbind(ftr.train_neg,rep(0,dim(ftr.train_neg)[1])))
ftr.train[is.nan(ftr.train)] <- 0
x <- ftr.train[,3:(ncol(ftr.train)-1)]
y <- ftr.train[,ncol(ftr.train)]
model <- svm(x,y,type="C-classification")

#test
ftr.test <- rbind(cbind(ftr.test_pos,rep(1,dim(ftr.test_pos)[1])),
                   cbind(ftr.test_neg,rep(0,dim(ftr.test_neg)[1])))
ftr.test[is.nan(ftr.test)] <- 0
x <- ftr.test[,3:(ncol(ftr.test)-1)]
y <- ftr.test[,ncol(ftr.test)]
result <- as.numeric(predict(model,x))
pred <- prediction(result,y)
perf <- performance(pred,"prec","rec")
t <- c(result,y)
num.pos <- sum(y==1)
num.neg <- sum(y==0)
pred.pos <- sum(result==2)
pred.neg <- sum(result==1)
TP <- length(subset(t,result == 2 & y == 1))
TN <- length(subset(t,result == 1 & y == 0))
FP <- length(subset(t,result == 2 & y == 0))
FN <- length(subset(t,result == 1 & y == 1))
precision <- TP/(TP+FP)
recall <- TP/(TP+FN)
# #plot
# plot(perf, avg= "threshold", colorize=T, lwd= 3,
#      main= "... Precision/Recall graphs ...")
# plot(perf, lty=3, col="grey78", add=T)
