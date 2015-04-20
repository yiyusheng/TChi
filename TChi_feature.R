# feature extraction 
rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()

# change wd to read data
setwd(data_dir)
# load('TChi_data.Rda')
# load('sta_selled_item.Rda')
load('TChi_trainset_testset_specuser.Rda')

# featureA: count number of three behavior type and time before predict
featureA <- function(ds,last_date) {
  max_len = 1000
  ds$time_before <- last_date - ds$time
  ds <- ds[,c('user_id','item_id','behavior_type','time_before')]
  ds <- ds[with(ds, order(user_id,item_id)),]
  
  uipair <- ds[!duplicated(ds[c('user_id','item_id')]),]
  len_uipair <- dim(uipair)[1]
  uipair <- uipair[1:min(len_uipair,max_len),]
  len_uipair <- dim(uipair)[1]
  ftr <- matrix(0,nrow = len_uipair,ncol = 8)
  # bt* means count number of behavior *
  # bt*_t means mean time before predict
  colnames(ftr) <- c('user_id','item_id',
                     'btA','btB','btC','btA_t','btB_t','btC_t')
  for (i in 1:len_uipair) {
    curr_ui <- uipair[i,]
    curr_data <- subset(ds, user_id == curr_ui$user_id &
                          item_id == curr_ui$item_id)
    ftr[i,c('user_id','item_id')] <- c(curr_ui$user_id,curr_ui$item_id)
    curr_data_btA = subset(curr_data, behavior_type == 1)
    ftr[i,c('btA','btA_t')] <- c(dim(curr_data_btA)[1],mean(curr_data_btA$time_before))
    curr_data_btB = subset(curr_data, behavior_type == 2)
    ftr[i,c('btB','btB_t')] <- c(dim(curr_data_btB)[1],mean(curr_data_btB$time_before))
    curr_data_btC = subset(curr_data, behavior_type == 3)
    ftr[i,c('btC','btC_t')] <- c(dim(curr_data_btC)[1],mean(curr_data_btC$time_before))
    print(i)
  }
  return(ftr)
}

#predict parameter
train_start <- as.Date('2014-11-18')
train_end <- as.Date('2014-12-17')
train_label_start <- as.Date('2014-12-17')
train_label_end <- as.Date('2014-12-18')
test_start <- as.Date('2014-12-17')
test_end <- as.Date('2014-12-18')
test_label_start <- as.Date('2014-12-18')
test_label_end <- as.Date('2014-12-19')
print('train_pos')
ftr.train_pos <- featureA(data.train_pos_rmna,train_end)
print('train_neg')
ftr.train_neg <- featureA(data.train_neg,train_end)
print('test_pos')
ftr.test_pos <- featureA(data.test_pos_rmna,train_end)
print('test_neg')
ftr.test_neg <- featureA(data.test_neg,train_end)
save(ftr.test_neg,ftr.test_pos,ftr.train_neg,ftr.train_pos,file = 'TChi_featureA.Rda')
