# function of TChi: 
#   data_seperate,feature,model

####################################################################################################################
data_seperate <- function(test_label_start,
                          itr, ite,
                          vari_trainlabel,
                          Data_dir,file_out_predix) {
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
  
  out_name <- paste(file_out_predix,itr,ite,vari_trainlabel,sep='_')
  out_file <- paste(Data_dir,out_name,'.Rda',sep = '')
  if (itr+ite <= 28 & !file.exists(out_file)) {
    print(paste('DATA_SEPERATE:',
                paste(itr,ite,vari_trainlabel,sep='_'),
                date()))
    # predict parameter  
    test_label_end <- test_label_start + 1*60*60*24
    test_end <- test_label_start
    test_start <- test_end - ite*60*60*24
    train_label_start <- test_start
    if (vari_trainlabel == 1)train_label_end <- test_end
    if (vari_trainlabel == 0)train_label_end <- test_start + 1*60*60*24
    train_end <- train_label_start
    train_start <- train_end - itr*60*60*24  
    
    # train
    data.train <- subset(data.alluser, time >= train_start & time < train_end)
    label.train <- subset(data.alluser, time >= train_label_start & 
                            time < train_label_end & behavior_type == 4)
    label.train <- label.train[!duplicated(label.train['ui']),]
    
    
    # seperate posivite data and negtive data in train (UIpair in label.train)
    data.train_pos <- 0 #may lead some problem
    data.train_pos_rmna <- subset(data.train, data.train$ui %in% label.train$ui)
    data.train_neg <- subset(data.train, !(data.train$ui %in% label.train$ui))
    
    # test: reduce by match item in data.item
    data.test <- subset(data.alluser, time >= test_start & time < test_end)
    data.test <- subset(data.test, item_id %in% data.item$item_id)
    data.test_label <- subset(data.alluser, time >= test_label_start 
                              & time < test_label_start + 24*60*60
                              & behavior_type == 4)
    data.test_label <- data.test_label[!duplicated(data.test_label['ui']),]
    
    # save
    save(data.train_neg,
         data.train_pos,data.train_pos_rmna,
         data.test,data.test_label,
         file = out_file)
  }
  

}

####################################################################################################################
feature_each <- function(ds,
                         last_date,
                         max_len) {
  # time calculate and sort by ui.(hours)
  time_before <- last_date - ds$time
  units(time_before) <- 'hours'
  ds$time_before <- as.numeric(time_before)
  ds <- ds[with(ds, order(user_id,item_id)),]   #order
  uipair <- ds[!duplicated(ds['ui']),c('user_id','item_id','ui',
                                       'user_geohash','item_category','item_geohash')]
  len_uipair <- nrow(uipair)
  
  # uipair sample
  if (max_len == 0){}
  else {
    randidx.uipair <- ceiling(runif(min(len_uipair,max_len),1,len_uipair))
    uipair <- uipair[randidx.uipair,]
  }
  len_uipair <- nrow(uipair)
  reduce.ds <- subset(ds,ds$ui %in% uipair$ui)
  
  # feature generate
  # bt* means count number of behavior *
  # bt*_t means mean time before predict
  colname <- c('user_id','item_id','ui',
               'user_geohash','item_category','item_geohash',
               'A_count', 'A_meanH', 'A_count6','A_count12','A_count24','A_count36','A_count48','A_countout',
               'B_count', 'B_meanH', 'B_count6','B_count12','B_count24','B_count36','B_count48','B_countout',
               'C_count', 'C_meanH', 'C_count6','C_count12','C_count24','C_count36','C_count48','C_countout')
  fix_ncol <- 6
  ftr_ncol <- 8
  ftr <- data.frame(matrix(0,nrow = len_uipair,ncol = length(colname)))
  colnames(ftr) <- colname
  ftr[,c('user_id','item_id','ui','user_geohash','item_category','item_geohash')] <- uipair
  for (i in 1:3) {
    sset <- subset(reduce.ds,behavior_type == i)
    idx <- as.numeric(as.character(sset$ui))
    
    ftr1 <- tapply(idx,idx,length)
    ftr2 <- tapply(as.numeric(sset$time_before),idx,mean)
    ftr3 <- tapply(as.numeric(sset$time_before),idx,function(x) sum(x<=6))
    ftr4 <- tapply(as.numeric(sset$time_before),idx,function(x) sum(x<=12))
    ftr5 <- tapply(as.numeric(sset$time_before),idx,function(x) sum(x<=24))
    ftr6 <- tapply(as.numeric(sset$time_before),idx,function(x) sum(x<=36))
    ftr7 <- tapply(as.numeric(sset$time_before),idx,function(x) sum(x<=48))
    ftr8 <- tapply(as.numeric(sset$time_before),idx,function(x) sum(x>48))
    ftr4 <- ftr4 - ftr3
    ftr5 <- ftr5 - ftr4
    ftr6 <- ftr6 - ftr5
    ftr7 <- ftr7 - ftr6
    ui <- unique(idx)
    if (length(ui) > 0){
      ftr[match(ui,as.numeric(as.character(ftr$ui))),
              (1+fix_ncol+(i-1)*ftr_ncol):(fix_ncol+(i)*ftr_ncol)] <- 
          cbind(as.numeric(ftr1),as.numeric(ftr2),as.numeric(ftr3),
            as.numeric(ftr4),as.numeric(ftr5),as.numeric(ftr6),
            as.numeric(ftr7),as.numeric(ftr8))
    }
 
  }
  # return
  return(list('feature' = ftr,'uipair_len' = len_uipair))
}

####################################################################################################################
feature_all <- function(test_label_start,
                        itr,ite,
                        vari_trainlabel,
                        rate.pos_neg,
                        in_dir,file_in_predix,
                        out_dir,file_out_predix) {
  # load
  out_name <- paste(file_out_predix,itr,ite,vari_trainlabel,rate.pos_neg,sep='_')
  out_file <- paste(out_dir,out_name,'.Rda',sep = '')
  if (itr+ite <= 28 & !file.exists(out_file)) {
    in_name <- paste(file_in_predix,itr,ite,vari_trainlabel,sep='_')
    in_file <- paste(in_dir,in_name,'.Rda',sep='')
    load(in_file)
    print(paste('FEATURE:',
                paste(itr,ite,vari_trainlabel,rate.pos_neg,sep='_'),
                date()))
   
    # predict parameter
    test_label_end <- test_label_start + 1*60*60*24
    test_end <- test_label_start
    test_start <- test_end - ite*60*60*24
    train_label_start <- test_start
    train_label_end <- test_end
    train_end <- train_label_start
    train_start <- train_end - itr*60*60*24
    
    #train buy
    trainbuy <- subset(data.alluser, behavior_type == 4 
                      & time >= train_label_start
                      & time < (train_label_start + 24*60*60))
    assign("trainbuy",trainbuy,envir = .GlobalEnv)
#                       & item_id %in% data.item$item_id)

    # feature_each for train_pos
    print(paste('train_pos: ', nrow(data.train_pos_rmna), 'lines',sep=''))
    r <- feature_each(data.train_pos_rmna,train_end,0)
    ftr.train_pos <- r$feature
    # train_end
    print(paste('train_neg: ', nrow(data.train_neg), 'lines',sep=''))
    r <- feature_each(data.train_neg,train_end,r$uipair_len*rate.pos_neg)
    ftr.train_neg <- r$feature
    # test
    print(paste('test: ', nrow(data.test), 'lines',sep=''))
    r <- feature_each(data.test,test_end,0)
    ftr.test <- r$feature
    # save
    save(ftr.test,ftr.train_neg,ftr.train_pos,data.test_label,file = out_file)
  }
  
}

####################################################################################################################
svmf <- function(test_label_start,
                 itr,ite,
                 vari_trainlabel,
                 rate.pos_neg,
                 cost,
                 in_dir,file_in_predix,
                 out_dir,file_out_predix){
  # load library
  library(e1071)
  library(gplots)
  library(ROCR)
  
  # change wd to read data
  in_name <- paste(file_in_predix,itr,ite,vari_trainlabel,rate.pos_neg,sep='_')
  in_file <- paste(in_dir,in_name,'.Rda',sep='')
  out_name <- paste(file_out_predix,itr,ite,
                    vari_trainlabel,rate.pos_neg,
                    cost,sep='_')
  out_file <- paste(out_dir,out_name,'.Rda',sep='')
  csv_file <- paste(out_dir,out_name,'.csv',sep='')
  if (itr+ite <= 28) {
#   if (itr+ite <= 28 & !file.exists(out_file) & !file.exists(csv_file)) {
    load(in_file)
    print(paste('SVM:',
                paste(itr,ite,vari_trainlabel,rate.pos_neg,cost,sep='_'),
                date()))
    
    
    # data standardization
    ftr.train_pos$class <- rep(1,nrow(ftr.train_pos))
    ftr.train_neg$class <- rep(0,nrow(ftr.train_neg))
    ftr.train <- rbind(ftr.train_pos,
                       ftr.train_neg,
                       row.names = FALSE)
    
    
    ftr.test$class <- 0
    ftr.test$class <- as.integer(ftr.test$class)
    if (exists('data.test_label') & nrow(data.test_label) > 0) {
      idx <- ftr.test$ui %in% data.test_label$ui
      ftr.test[idx,'class'] <- 1
      ftr.test <- ftr.test[order(ftr.test$class,decreasing = TRUE),]
    }
    # more feature:time and day
#     ftr.train$tdA <- ftr.train$btA / ftr.train$btA_t
#     ftr.train$tdB <- ftr.train$btB / ftr.train$btB_t
#     ftr.train$tdC <- ftr.train$btC / ftr.train$btC_t
#     ftr.test$tdA <- ftr.test$btA / ftr.test$btA_t
#     ftr.test$tdB <- ftr.test$btB / ftr.test$btB_t
#     ftr.test$tdC <- ftr.test$btC / ftr.test$btC_t
#     ftr.train[is.na(ftr.train)] <- 0
#     ftr.test[is.na(ftr.test)] <- 0
    # model establishment

    num_field <- c('user_geohash','item_category','item_geohash',
                   'A_count', 'A_meanH', 'A_count6','A_count12','A_count24','A_count36','A_count48','A_countout',
                   'B_count', 'B_meanH', 'B_count6','B_count12','B_count24','B_count36','B_count48','B_countout',
                   'C_count', 'C_meanH', 'C_count6','C_count12','C_count24','C_count36','C_count48','C_countout')
#                    'tdA','tdB','tdC')
#     model <- svm(rbind(ftr.train[,num_field],ftr.test[,num_field]),
#                  c(ftr.train$class,ftr.test$class),
    model <- svm(ftr.train[,num_field],
                 ftr.train$class,
                 type="C-classification", 
                 cost = cost, 
                 kernel = 'radial', 
                 prob = TRUE)
    # predict on train
    result_train <- predict(model, newdata = data.frame(ftr.train[,num_field]), prob = TRUE)
    ftr.train_predict <- ftr.train[result_train == 1,]
    real_train <- subset(ftr.train,class == 1)
    TP_train <- nrow(subset(ftr.train_predict,ftr.train_predict$ui %in% real_train$ui))
    FP_train <- nrow(ftr.train_predict) - TP_train
    FN_train <- nrow(real_train) -TP_train
    prec_train <- TP_train/(TP_train + FP_train)
    rec_train <- TP_train/(TP_train + FN_train)
  
    # predict on test
    drop <- subset(ftr.test,A_count == 1 & C_count == 0 & C_count == 0)
    result_test <- predict(model, newdata = data.frame(ftr.test[,num_field]), prob = TRUE)
    ftr.test_predict <- ftr.test[result_test == 1,]
    real_test <- data.test_label
    TP <- nrow(subset(ftr.test_predict,ftr.test_predict$ui %in% real_test$ui))
    FP <- nrow(ftr.test_predict) - TP
    FN <- nrow(real_test) - TP
    prec <- TP/(TP + FP)
    rec <- TP/(FP + FN)
    # save
    r <- data.frame(para = as.character(in_name),
                    prec = prec,
                    rec = rec,
                    f1 = 2*prec*rec/(prec + rec),
                    len_prec = TP + FP,
                    len_real = TP + FN,
                    prec_train = prec_train,
                    rec_train = rec_train,
                    f1_train = 2*prec_train*rec_train/(prec_train+rec_train),
                    len_prec_train = TP_train + FP_train,
                    len_real = TP_train + FN_train)
    # predict result and save
    pred_posui <- ftr.test_predict[,1:2]
    write.csv(file = csv_file, x = pred_posui, row.names=FALSE)
    save(pred_posui,file = out_file)
    return(r)
  }
  
}
####################################################################################################################
evaluate <- function(test_label_start,
                     itr,ite,
                     vari_trainlabel,
                     rate.pos_neg,svm_cost,
                     in_dir,file_in_predix){
  
  # predict data
  file_name <- paste(file_in_predix,itr,ite,
                     vari_trainlabel,rate.pos_neg,svm_cost,sep='_')
  in_file <- paste(in_dir,file_name,'.Rda',sep='')
  r <- 0
  if (itr+ite <= 28 & file.exists(in_file)) {
    print(paste('EVALUATE: ',file_name))
    load(in_file)
    predict.ui <- pred_posui$user_id*10000000000+pred_posui$item_id
    real.ui <- as.numeric(as.character(testbuy$ui))
    
    # evaluation
    TP <- sum(predict.ui %in% real.ui)
    FP <- length(predict.ui) - TP
    FN <- length(real.ui) -TP
    prec <- TP/(TP + FP)
    rec <- TP/(TP + FN)
    r <- data.frame(para = as.character(file_name),
                    prec = prec,
                    rec = rec,
                    f1 = 2*prec*rec/(prec + rec),
                    len_prec = TP + FP,
                    len_real = TP + FN)
  }
  return(r)
}



