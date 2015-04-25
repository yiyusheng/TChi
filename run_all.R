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
  
  para_sep <- expand.grid(itr = itr, ite = ite)
  if (enable[1])mapply(data_seperate,
                        test_label_start,
                        para_sep$itr, para_sep$ite,
                        vari_trainlabel,
                        in_dir,file_sep_predix)
  
  para_feature <- expand.grid(itr = itr,ite = ite,rate = rate)
  if (enable[2])mapply(feature_all,
                       test_label_start,
                       para_feature$itr,para_feature$ite,
                       vari_trainlabel,
                       para_feature$rate,
                       in_dir,file_sep_predix,
                       out_dir,file_feature_predix)
  
  r <- 0
  para_svm <- expand.grid(itr = itr,ite = ite,rate = rate,cost = svm_cost)
  if (enable[3])r <- mapply(svmf,
                       test_label_start,
                       para_svm$itr,para_svm$ite,
                       vari_trainlabel,
                       para_svm$rate,
                       para_svm$cost,
                       in_dir,file_feature_predix,
                       out_dir,file_svm_predix)
  print(r)
}

# parameters
eval_only <- 0
test_label_start <- as.POSIXct('2014-12-19',format='%Y-%m-%d')
assign("test_label_start",test_label_start,envir = .GlobalEnv)
# time
itr <- c(25)
ite <- c(1)
# vari
vari_trainlabel <- 0
# rate for feature only
rate <- c(100)
# cost for svm only
svm_cost <- c(1)
#enable spe,ftr,svm,eval
enable <- matrix(c(1,1,1,1),1,4)
assign('enable',enable,envir = .GlobalEnv)

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

testbuy <- 0
testbuy <- subset(data.alluser, behavior_type == 4 
                  & time >= test_label_start
                  & time < (test_label_start + 24*60*60)
                  & item_id %in% data.item$item_id)
assign("testbuy",testbuy,envir = .GlobalEnv)
# data seperate->feature extraction->generate result
exec(test_label_start,
            itr, ite, 
            vari_trainlabel,rate,
            svm_cost,
            file_sep_predix,
            file_feature_predix,
            file_svm_predix,
            in_dir,out_dir)


# result evaluation
# para_eva <- expand.grid(itr = itr,ite = ite,rate = rate,cost = svm_cost)
# if (enable[4]){
#   eva <- mapply(evaluate,
#                 test_label_start,
#                 para_eva$itr, para_eva$ite, 
#                 vari_trainlabel,para_eva$rate,
#                 para_eva$cost,
#                 in_dir,file_svm_predix)
#   save(eva,file = paste(Data_dir,'eval.Rda',sep=''))
# }

