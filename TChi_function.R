# function of TChi: 
#   data_seperate,feature,model


data_seperate <- function(data.alluser,
                          data.item,
                          test_label_start, 
                          itr, ite,
                          vari_trainlabel,
                          out_file) {
# function: data_seperate
# describtion: seperate
# args:
#     test_label_start: start time of test label to calculate the rest time with itr,ite and vari_trainlabel
#     itr: interval of train data
#     ite: interval of test data
#     vari_trainlabel: 
#       if 0, time range of train label equals to 1
#       if 1, time range of train label equals to time range of test data
#     out_file:
#       absolute path of out_file
# return:
#     no return, data is saved in Rda into out_file
  print(paste('DATA_SEPERATE: inter_train:',itr,'inter_test:',ite,
              'vari:', vari_trainlabel, 'time:', date()))
  # predict parameter  
    test_label_end <- test_label_start + 1
    test_end <- test_label_start
    test_start <- test_end - ite
    train_label_start <- test_start
    if (vari_trainlabel == 1)train_label_end <- test_end
    if (vari_trainlabel == 0)train_label_end <- test_start + 1
    train_end <- train_label_start
    train_start <- train_end - itr
  
  # Data
    user = data.alluser
    #out_file = paste('TChi_trainset_testset_specuser_',itr,'_',ite,'.Rda', sep='')   
    
  # train
    data.train <- subset(user, time >= train_start & time < train_end)
    label.train <- subset(user[c('user_id','item_id','time','behavior_type')], time >= train_label_start & 
                            time < train_label_end & behavior_type == 4)
    label.train <- label.train[!duplicated(label.train[c('user_id','item_id')]),c('user_id','item_id')]
    
    
  # posivite data of train (UIpair in label.train)
    #data.train_pos <- merge(x = label.train, y = data.train, all.x = TRUE)
    data.train_pos <- 0
    data.train_pos_rmna <- merge(x = label.train, y = data.train)
    
  # negative data of train (UIpair not in label.train)
    data.train$includeA <- TRUE
    label.train$includeB <- TRUE
    data.train_neg <- merge(x = label.train, y = data.train, all.y = TRUE)
    data.train_neg <- data.train_neg[is.na(data.train_neg$includeB),]
    drops <- c("includeA","includeB")
    data.train_neg <- data.train_neg[,!(names(data.train_neg) %in% drops)]
    label.train <- label.train[,!(names(label.train) %in% drops)]
    data.train <- data.train[,!(names(data.train) %in% drops)]
    
  # test
    data.test <- subset(user, time >= test_start & time < test_end)
    data.test <- merge(data.test,data.item$item_id)
  # save
    save(data.train_neg,
         data.train_pos,data.train_pos_rmna,
         data.test,
         file = out_file)
}

