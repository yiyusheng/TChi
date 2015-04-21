rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()

# change wd to read data
setwd(data_dir)
load('TChi_data.Rda')
#para
#vari_trainlabel: 
#1 means range of trainlabel equals to range of testdata
#0 means range of trainlabel equals to the first day of testdata
vari_trainlabel <- 0
inter_train = c(25,15,7,3)
inter_test = c(1,3,5,7)
# inter_train = c(25,15,7)
# inter_test = c(1,3)
test_label_start <- as.Date('2014-12-18')
for (itr in inter_train) {
  for (ite in inter_test) {
    if (ite == 3 & (itr != 25))next
        print(paste('inter_train:',itr,'inter_test:',ite))
        #predict parameter
        #     train_start <- as.Date('2014-11-18')
        #     train_end <- train_start + itr
        #     train_label_start <- train_end
        #     train_label_end <- train_label_start + ite
        #     test_start <- train_label_start
        #     test_end <- train_label_end
        #     test_label_start <- test_end
        #     test_label_end <- test_label_start + 1
        
        test_label_end <- test_label_start + 1
        test_end <- test_label_start
        test_start <- test_end - ite
        train_label_start <- test_start
        if (vari_trainlabel == 1)train_label_end <- test_end
        if (vari_trainlabel == 0)train_label_end <- test_start + 1
        train_end <- train_label_start
        train_start <- train_end - itr
        # specitem
        for (i in 1:2) {
          if (i == 1) {
            user = data.specuser
            out_file = paste('TChi_trainset_testset_specuser_',itr,'_',ite,'.Rda', sep='')   
          }else if (i == 2) {
            user = data.alluser 
            out_file = paste('TChi_trainset_testset_alluser_',itr,'_',ite,'.Rda', sep='')   
          }    
          # Seperate trainset and testset
          # train
          label.train <- subset(user[c('user_id','item_id','time','behavior_type')], time >= train_label_start & 
                                  time < train_label_end & behavior_type == 4)
          label.train <- label.train[!duplicated(label.train[c('user_id','item_id')]),c('user_id','item_id')]
          data.train <- subset(user, time >= train_start & time < train_end)
          
          # for posivite data (UIpair in label.train)
          data.train_pos <- merge(x = label.train, y = data.train, all.x = TRUE)
          data.train_pos_rmna <- merge(x = label.train, y = data.train)
          
          # for negative data (UIpair not in label.train)
          data.train$includeA <- TRUE
          label.train$includeB <- TRUE
          data.train_neg <- merge(x = label.train, y = data.train, all.y = TRUE)
          data.train_neg <- data.train_neg[is.na(data.train_neg$includeB),]
          drops <- c("includeA","includeB")
          data.train_neg <- data.train_neg[,!(names(data.train_neg) %in% drops)]
          label.train <- label.train[,!(names(label.train) %in% drops)]
          data.train <- data.train[,!(names(data.train) %in% drops)]
          
          # test
          label.test <- subset(user[c('user_id','item_id','time','behavior_type')], time >= test_label_start & 
                                 time < test_label_end & behavior_type == 4)
          label.test <- label.test[!duplicated(label.test[c('user_id','item_id')]),c('user_id','item_id')]
          data.test <- subset(user, time >= test_start & time < test_end)
          
          # for posivite data (UIpair in label.train)
          data.test_pos <- merge(x = label.test, y = data.test, all.x = TRUE)
          data.test_pos_rmna <- merge(x = label.test, y = data.test)
          
          # for negative data (UIpair not in label.test)
          data.test$includeA <- TRUE
          label.test$includeB <- TRUE
          data.test_neg <- merge(x = label.test, y = data.test, all.y = TRUE)
          data.test_neg <- data.test_neg[is.na(data.test_neg$includeB),]
          drops <- c("includeA","includeB")
          data.test_neg <- data.test_neg[,!(names(data.test_neg) %in% drops)]
          label.test <- label.test[,!(names(label.test) %in% drops)]
          data.test <- data.test[,!(names(data.test) %in% drops)]
          
          # save
          save(data.train_neg,data.train_pos,data.train_pos_rmna,
               data.test_neg,data.test_pos,data.test_pos_rmna,
               file = out_file)
        }
  }
}







