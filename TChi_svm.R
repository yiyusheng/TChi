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


rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()

# change wd to read data
setwd(data_dir)
inter_train = c(3,7,15,25)
inter_test = c(1,3,5,7)
test_label_start <- as.Date('2014-12-18')
frame.result <- as.data.frame(matrix(0,nrow = 3,)

# loop start
for (itr in inter_train) {
  for (ite in inter_test) {
    for (suf in c('spec','all')){
      if (itr == 25 & itr != 1)
        continue
      column_name = paste(itr,'_',ite,'_',suf,sep='')
      file_suf = paste(column_name,'.Rda',sep='')
      in_file = paste('TChi_featureA_',file_suf,sep='')
      load(in_file)
      print(paste('SVM: inter_train:',itr,'inter_test:',ite,'type',suf))
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
      precision <- 1
      recall <- 1
      f <- 2*precision*recall/(precision+recall)
      frame.result$column_name <- c(precision,recall,f)
    }
  }
}
save(frame.result,file = 'svm_result.Rda')










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
