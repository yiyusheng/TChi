## run all function

# preprocessing
# rm(list = ls())
if (.Platform$OS.type == 'windows') {
  Git_dir <- "D:/Git/TChi/"
  Data_dir <- "D:/Data/TChi/"
}else {
  Git_dir <- "/home/yiyusheng/Git/TChi/"
  Data_dir <- "/home/yiyusheng/Data/"
}

setwd(Git_dir)
source('TChi_function.R')

# function
exec <- function(test_label_start,
                 itr, ite,
                 vari_trainlabel,
                 rate.pos_neg,
                 file_sep_predix,
                 file_feature_predix,
                 file_svm_predix,
                 in_dir,out_dir) {
  if (itr + ite <= 28) {
    print(paste(itr,ite,rate.pos_neg,sep='_'))
    
#     data_seperate(data.alluser,data.item,
#                   test_label_start,
#                   itr, ite,
#                   vari_trainlabel,
#                   in_dir,file_sep_predix)
    
#     feature_all(test_label_start,
#                 itr,ite,
#                 vari_trainlabel,
#                 rate.pos_neg,
#                 in_dir,file_sep_predix,
#                 out_dir,file_feature_predix)
    
    svmf(test_label_start,
         itr,ite,
         vari_trainlabel,
         rate.pos_neg,
         in_dir,file_feature_predix,
         out_dir,file_svm_predix) 
  }
}

# parameters
eval_only <- 1
test_label_start <- as.POSIXct('2014-12-18',format='%Y-%m-%d')
# itrain <- c(27,25,20) 
# itest <- c(1,3,5,7)
# vari_trainlabel <- c(0,1)
# rate <- c(10,50,100,500,1000)
itrain <- c(25)
itest <- c(1)
vari_trainlabel <- 0
rate <- c(30)
para <- expand.grid(itr = itrain, ite = itest,
                    vari = vari_trainlabel, rate = rate)

# file name
file_sep_predix <- 'ds'
file_feature_predix <- 'dsf'
file_svm_predix <- 'svm'
file_eval_predix <- 'eval'
in_dir <- Data_dir
out_dir <- Data_dir

# load data
# load(paste(Data_dir,'TChi_data.Rda',sep=''))
#   data.alluser <- data.specuser
#   save(data.alluser,data.specuser,data.item,file = paste(Data_dir,'TChi_specdata.Rda',sep=''))
#   load(paste(Data_dir,'TChi_specdata.Rda',sep=''))
assign("data.alluser",data.alluser,envir = .GlobalEnv)
assign("data.item",data.item,envir = .GlobalEnv)

realbuy <- subset(data.alluser, behavior_type == 4 
                  & time >= test_label_start
                  & time < (test_label_start + 24*60*60)
                  & item_id %in% data.item$item_id)
assign("realbuy",realbuy,envir = .GlobalEnv)

# data seperate->feature extraction->generate result
if (!eval_only){
  mapply(exec,
         test_label_start,
         para$itr, para$ite, 
         para$vari,para$rate,
         file_sep_predix,
         file_feature_predix,
         file_svm_predix,
         in_dir,out_dir)
}

# result evaluation


eva <- mapply(evaluate,
       test_label_start,
       para$itr, para$ite, 
       para$vari,para$rate,
       in_dir,file_svm_predix)
save(eva,file = 'eval.Rda')

