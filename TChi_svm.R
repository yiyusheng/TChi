# use svm to establish models
rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()
library(e1071)
library(gplots)
library(ROCR)
# change wd to read data
setwd(data_dir)

rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()

# change wd to read data
setwd(data_dir)
inter_train = c(25,15,7,3)
inter_test = c(1,3,5,7)
test_label_start <- as.Date('2014-12-18')
frame.result <- as.data.frame(matrix(0,nrow = 3,ncol = 0))

# loop start
for (itr in inter_train) {
  for (ite in inter_test) {
    for (suf in c('spec','all')){
      if (itr == 25 & (ite != 1 & ite != 3))next
      column_name = paste(suf,'_',itr,'_',ite,sep='')
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
      df <- data.frame(x = x, y = y)
      model <- svm(y ~ x, data = df, type="C-classification", cost = 1, kernel = 'radial', prob = TRUE)
      
      #test
      ftr.test <- rbind(cbind(ftr.test_pos,rep(1,dim(ftr.test_pos)[1])),
                        cbind(ftr.test_neg,rep(0,dim(ftr.test_neg)[1])))
      ftr.test[is.nan(ftr.test)] <- 0
      x <- ftr.test[,3:(ncol(ftr.test)-1)]
      y <- ftr.test[,ncol(ftr.test)]
      result <- predict(model, newdata = data.frame(x = x), prob = TRUE)
      num.result <- as.numeric(result) - 1
      perf <- data.frame(predict = num.result, real = y)
      TP <- nrow(subset(perf,predict == 1 & real == 1))
      FP <- nrow(subset(perf,predict == 1 & real == 0))
      FN <- nrow(subset(perf,predict == 0 & real == 1))
      TN <- nrow(subset(perf,predict == 0 & real == 0))
      precision <- TP/(TP+FP)
      recall <- TP/(TP+FN)
      f <- 2*precision*recall/(precision+recall)
      frame.result <- cbind(frame.result,c(precision,recall,f))
      names(frame.result)[ncol(frame.result)] <- column_name
#       pred <- prediction(perf.result,y)
#       perf <- performance(pred,"prec","rec")
#       precision <- as.numeric(perf@x.values)
#       recall <- as.numeric(perf@y.values)
#       f <- performance(pred,"f")
    }
  }
}
save(frame.result,file = 'svm_result.Rda')

