## run all function

# preprocessing
  rm(list = ls())
  Git_dir <- "D:/Git/TChi/"
  Data_dir <- "D:/Data/TChi/"
  setwd(Git_dir)
  source('TChi_function.R')
  
# parameters
  inter_train <- c(25,25)
  inter_test <- c(1,3)
  inter <- cbind(inter_train,inter_test)
  test_label_start <- as.Date('2014-12-19')
  itr <- 7
  ite <- 1
  vari_trainlabel <- 0
  rate.pos_neg = 10
  
# file name
  file_name <- paste('ds',itr,ite,vari_trainlabel,sep='_')
  out_file <- paste(Data_dir,file_name,'.Rda',sep = '')
  in_file_feature <- out_file
  out_file_feature <- paste(Data_dir,file_name,'ftr.Rda',sep='')
  file_sep_predix <- 'ds'
  file_feature_predix <- 'dsf'
  file_svm_predix <- 'svm'
  in_dir <- Data_dir
  out_dir <- Data_dir
  
# # load data
  load(paste(Data_dir,'TChi_data.Rda',sep=''))
#   
# # EXEC
#   data_seperate(data.alluser,data.item,
#                 test_label_start,
#                 itr, ite,
#                 vari_trainlabel,
#                 Data_dir,file_sep_predix)
   
#   feature_all(test_label_start,
#               itr,ite,
#               vari_trainlabel,
#               rate.pos_neg,
#               in_dir,file_sep_predix,
#               out_dir,file_feature_predix)
#   
#   svmf(test_label_start,
#        itr,ite,
#        vari_trainlabel,
#        rate.pos_neg,
#        in_dir,file_feature_predix,
#        out_dir,file_svm_predix)
