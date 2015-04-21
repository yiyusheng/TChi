# featureA: count number of three behavior type and time before predict
# feature extraction 
rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()

featureA <- function(ds,last_date) {
  max_len = 5000
  ds$time_before <- last_date - ds$time
  ds <- ds[,c('user_id','item_id','behavior_type','time_before')]
  ds <- ds[with(ds, order(user_id,item_id)),]   #order
  
  uipair <- ds[!duplicated(ds[c('user_id','item_id')]),]
  len_uipair <- dim(uipair)[1]
  #uipair <- uipair[1:min(len_uipair,max_len),]
  randidx.uipair <- ceiling(runif(min(len_uipair,max_len),1,len_uipair))
  uipair <- uipair[randidx.uipair,]
  len_uipair <- dim(uipair)[1]
  ftr <- matrix(0,nrow = len_uipair,ncol = 8)
  #ds reduce
  reduce.ds <- merge(ds,uipair)
  # bt* means count number of behavior *
  # bt*_t means mean time before predict
  colnames(ftr) <- c('user_id','item_id',
                     'btA','btB','btC','btA_t','btB_t','btC_t')
  for (i in 1:len_uipair) {
    curr_ui <- uipair[i,]
    curr_data <- subset(reduce.ds, user_id == curr_ui$user_id &
                          item_id == curr_ui$item_id)
    ftr[i,c('user_id','item_id')] <- c(curr_ui$user_id,curr_ui$item_id)
    curr_data_btA = subset(curr_data, behavior_type == 1)
    ftr[i,c('btA','btA_t')] <- c(dim(curr_data_btA)[1],mean(curr_data_btA$time_before))
    curr_data_btB = subset(curr_data, behavior_type == 2)
    ftr[i,c('btB','btB_t')] <- c(dim(curr_data_btB)[1],mean(curr_data_btB$time_before))
    curr_data_btC = subset(curr_data, behavior_type == 3)
    ftr[i,c('btC','btC_t')] <- c(dim(curr_data_btC)[1],mean(curr_data_btC$time_before))
  }
  return(ftr)
}

# change wd to read data
setwd(data_dir)
inter_train = c(3,7,15,25)
inter_test = c(1,3,5,7)
test_label_start <- as.Date('2014-12-18')
for (itr in inter_train) {
  for (ite in inter_test) {
    for (suffix in c('spec','all')){
      if (itr == 25 & itr != 1)
        continue
      #load
      out_file = paste('TChi_featureA_',suffix,'_',itr,'_',ite,'.Rda',sep='')
      file_name = paste('TChi_trainset_testset_',suffix,'user_',itr,'_',ite,'.Rda', sep='') 
      load(file_name)
      print(paste('FEATURE: inter_train:',itr,'   inter_test:',ite,'   type:',suffix,sep=''))
      #predict parameter
      test_label_end <- test_label_start + 1
      test_end <- test_label_start
      test_start <- test_end - ite
      train_label_start <- test_start
      train_label_end <- test_end
      train_end <- train_label_start
      train_start <- train_end - itr
      print('train_pos')
      ftr.train_pos <- featureA(data.train_pos_rmna,train_end)
      print('train_neg')
      ftr.train_neg <- featureA(data.train_neg,train_end)
      print('test_pos')
      ftr.test_pos <- featureA(data.test_pos_rmna,train_end)
      print('test_neg')
      ftr.test_neg <- featureA(data.test_neg,train_end)
      save(ftr.test_neg,ftr.test_pos,ftr.train_neg,ftr.train_pos,file = out_file)
    }
  }
}





# 
# out_file = paste('TChi_trainset_testset_specuser_',itr,'_',ite,'.Rda', sep='')   
# ut_file = paste('TChi_trainset_testset_alluser_',itr,'_',ite,'.Rda', sep='')  
# load('TChi_trainset_testset_specuser.Rda')
# #predict parameter
# train_start <- as.Date('2014-11-18')
# train_end <- as.Date('2014-12-17')
# train_label_start <- as.Date('2014-12-17')
# train_label_end <- as.Date('2014-12-18')
# test_start <- as.Date('2014-12-17')
# test_end <- as.Date('2014-12-18')
# test_label_start <- as.Date('2014-12-18')
# test_label_end <- as.Date('2014-12-19')
# print('train_pos')
# ftr.train_pos <- featureA(data.train_pos_rmna,train_end)
# print('train_neg')
# ftr.train_neg <- featureA(data.train_neg,train_end)
# print('test_pos')
# ftr.test_pos <- featureA(data.test_pos_rmna,train_end)
# print('test_neg')
# ftr.test_neg <- featureA(data.test_neg,train_end)
# save(ftr.test_neg,ftr.test_pos,ftr.train_neg,ftr.train_pos,file = 'TChi_featureA.Rda')
