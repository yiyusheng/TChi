## run all function

# preprocessing
  rm(list = ls())
  if (.Platform$OS.type == 'windows') {
    Git_dir <- "D:/Git/TChi/"
    Data_dir <- "D:/Data/TChi/"
  }else {
    Git_dir <- "/home/yiyusheng/Git/TChi"
    Data_dir <- "/home/yiyusheng/Data"
  }

  setwd(Git_dir)
  source('TChi_function.R')
  
# parameters
  test_label_start <- as.Date('2014-12-18')
  itrain <- c(27,25,25,20,20,20,20) 
  itest <- c(1,1,3,1,3,5,7)
  vari_trainlabel <- 0
  rate <- c(10,30,50,70,100)
  
# file name
  file_sep_predix <- 'ds'
  file_feature_predix <- 'dsf'
  file_svm_predix <- 'svm'
  in_dir <- Data_dir
  out_dir <- Data_dir
  
# load data
 load(paste(Data_dir,'TChi_data.Rda',sep=''))

#   data_seperate(data.alluser,data.item,
#                 test_label_start,
#                 itrain[1], itest[1],
#                 vari_trainlabel,
#                 in_dir,file_sep_predix)
# EXEC function
exec <- function(data.alluser,data.item,
                 test_label_start,
                 itr, ite, rate.pos_neg,
                 vari_trainlabel,
                 file_sep_predix,
                 file_feature_predix,
                 file_svm_predix,
                 in_dir,out_dir) {
  print(paste(itr,ite,rate.pos_neg))
  data_seperate(data.alluser,data.item,
                test_label_start,
                itr, ite,
                vari_trainlabel,
                in_dir,file_sep_predix)
  feature_all(test_label_start,
              itr,ite,
              vari_trainlabel,
              rate.pos_neg,
              in_dir,file_sep_predix,
              out_dir,file_feature_predix)
  svmf(test_label_start,
       itr,ite,
       vari_trainlabel,
       rate.pos_neg,
       in_dir,file_feature_predix,
       out_dir,file_svm_predix)  
}

mapply(exec,
       data.alluser,data.item,
       test_label_start,
       itrain, itest, rate[100],
       vari_trainlabel,
       file_sep_predix,
       file_feature_predix,
       file_svm_predix,
       in_dir,out_dir)
