## run all function

# preprocessing
  rm(list = ls())
  Git_dir <- "D:/Git/TChi/"
  Data_dir <- "D:/Data/TChi/"
  setwd(Git_dir)
  debugSource('TChi_function.R')
  
# parameters
  inter_train <- c(25,25)
  inter_test <- c(1,3)
  inter <- cbind(inter_train,inter_test)
  test_label_start <- as.Date('2014-12-19')
  itr <- 25
  ite <- 3
  vari_trainlabel <- 0
  file_name <- paste('ds',itr,ite,vari_trainlabel,sep='_')
  out_file <- paste(Data_dir,file_name,'')
  load(paste(Data_dir,'TChi_data.Rda',sep=''))
  
# EXEC
  data_seperate(data.alluser,data.item,test_label_start,itr, ite,vari_trainlabel,out_file)

# test
  system.time(a <- paste(data.specuser$user_id,data.specuser$item_id,sep='_'))
  system.time(b <- data.specuser$item_id[1:1000]*10000L+data.specuser$user_id )
  