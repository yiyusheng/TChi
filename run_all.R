## run all function

# preprocessing
rm(list = ls())
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
                 rate,
                 svm_cost,
                 file_sep_predix,
                 file_feature_predix,
                 file_svm_predix,
                 in_dir,out_dir) {
  if (itr + ite <= 28) {  
    mapply(data_seperate,
            test_label_start,
            itr, ite,
            vari_trainlabel,
            in_dir,file_sep_predix)
    
    para_feature <- expand.grid(itr = itr,ite = ite,rate = rate)
    mapply(feature_all,
           test_label_start,
           para_feature$itr,para_feature$ite,
           vari_trainlabel,
           para_feature$rate,
           in_dir,file_sep_predix,
           out_dir,file_feature_predix)
    
    para_svm <- expand.grid(itr = itr,ite = ite,rate = rate,cost = svm_cost)
    mapply(svmf,
           test_label_start,
           para_svm$itr,para_svm$ite,
           vari_trainlabel,
           para_svm$rate,
           para_svm$cost,
           in_dir,file_feature_predix,
           out_dir,file_svm_predix)
  }
}

# parameters
eval_only <- 0
test_label_start <- as.POSIXct('2014-12-18',format='%Y-%m-%d')
itr <- c(25)
ite <- c(1)
vari_trainlabel <- 0
rate <- c(1,5)
svm_cost <- c(0.1,1)

# file name
file_sep_predix <- 'ds'
file_feature_predix <- 'dsf'
file_svm_predix <- 'svm'
file_eval_predix <- 'eval'
in_dir <- Data_dir
out_dir <- Data_dir

# load data
# load(paste(Data_dir,'TChi_data.Rda',sep=''))
# data.alluser <- data.specuser
# save(data.alluser,data.specuser,data.item,
#      file = paste(Data_dir,'TChi_specdata.Rda',sep=''))
if (.Platform$OS.type == 'windows') {
  load(paste(Data_dir,'TChi_specdata.Rda',sep=''))
}else {
  load(paste(Data_dir,'TChi_data.Rda',sep=''))
}

assign("data.alluser",data.alluser,envir = .GlobalEnv)
assign("data.item",data.item,envir = .GlobalEnv)

realbuy <- subset(data.alluser, behavior_type == 4 
                  & time >= test_label_start
                  & time < (test_label_start + 24*60*60)
                  & item_id %in% data.item$item_id)
assign("realbuy",realbuy,envir = .GlobalEnv)

# data seperate->feature extraction->generate result
if (!eval_only){
  exec(
  test_label_start,
  itr, ite, 
  vari_trainlabel,rate,
  svm_cost,
  file_sep_predix,
  file_feature_predix,
  file_svm_predix,
  in_dir,out_dir)
}

# result evaluation
para_eva <- expand.grid(itr = itr,ite = ite,rate = rate,cost = svm_cost)
eva <- mapply(evaluate,
              test_label_start,
              para_eva$itr, para_eva$ite, 
              vari_trainlabel,para_eva$rate,
              para_eva$cost,
              in_dir,file_svm_predix)
save(eva,file = paste(Data_dir,'eval.Rda',sep=''))
