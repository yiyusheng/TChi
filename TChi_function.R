# function of TChi: 
#   data_seperate,feature,model

####################################################################################################################
data_seperate <- function(user,data.item,
                          test_label_start,
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
    
  # train
    data.train <- subset(user, time >= train_start & time < train_end)
    label.train <- subset(user[c('user_id','item_id','time','behavior_type','ui')], time >= train_label_start & 
                            time < train_label_end & behavior_type == 4)
    label.train <- label.train[!duplicated(label.train['ui']),c('user_id','item_id','ui')]
    
    
  # seperate posivite data and negtive data in train (UIpair in label.train)
    #data.train_pos <- merge(x = label.train, y = data.train, all.x = TRUE)
    data.train_pos <- 0 #may lead some problem
    data.train_pos_rmna <- subset(data.train, data.train$ui %in% label.train$ui)
    data.train_neg <- subset(data.train, !(data.train$ui %in% label.train$ui))
    
  # test
    data.test <- subset(user, time >= test_start & time < test_end)
    data.test <- subset(data.test, data.test$item_id %in% data.item$item_id)
  
  # save
    file_name <- paste(file_out_predix,itr,ite,vari_trainlabel,sep='_')
    out_file <- paste(Data_dir,file_name,'.Rda',sep = '')
    save(data.train_neg,
         data.train_pos,data.train_pos_rmna,
         data.test,
         file = out_file)
    print(paste('end_time:',date()))
}

####################################################################################################################
feature_each <- function(ds,
                         last_date,
                         max_len) {
  # time calculate and sort by ui
    ds$time_before <- as.numeric(as.POSIXct(last_date) - ds$time)
    ds <- ds[with(ds, order(user_id,item_id)),]   #order
    uipair <- ds[!duplicated(ds['ui']),c('user_id','item_id','ui')]
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
                 'item_category','user_geohash','item_geohash'
                 'btA','btB','btC',
                 'btA_t','btB_t','btC_t'
                 )
    ftr <- data.frame(matrix(0,nrow = len_uipair,ncol = length(colname)))
    colnames(ftr) <- colname
    ftr[,c('user_id','item_id','ui')] <- uipair
    for (i in 1:3) {
      sset <- subset(reduce.ds,behavior_type == i)
      ff <- as.numeric(sset$ui)
      a <- tapply(sset$ui,ff,length)
      b <- tapply(sset$time_before,ff,mean)
      c <- unique(as.character(sset$ui))
      ftr[match(c,ftr$ui),c(colname[i+5],colname[i+8])] <- c(as.numeric(a),as.numeric(b))      
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
    file_name <- paste(file_in_predix,itr,ite,vari_trainlabel,sep='_')
    in_file <- paste(in_dir,file_name,'.Rda',sep='')
    load(in_file)
    print(paste('FEATURE: inter_train:',itr,'inter_test:',ite,
                'vari:',vari_trainlabel,'rate:',rate.pos_neg,
                'time:',date(),sep=''))
    
  # predict parameter
    test_label_end <- test_label_start + 1
    test_end <- test_label_start
    test_start <- test_end - ite
    train_label_start <- test_start
    train_label_end <- test_end
    train_end <- train_label_start
    train_start <- train_end - itr
  # feature_each
    print(paste('train_pos: ', nrow(data.train_pos_rmna), 'lines',sep=''))
    r <- feature_each(data.train_pos_rmna,test_end,0)
    ftr.train_pos <- r$feature
    print(paste('train_neg: ', nrow(data.train_neg), 'lines',sep=''))
    r <- feature_each(data.train_neg,test_end,r$uipair_len*rate.pos_neg)
    ftr.train_neg <- r$feature
    print(paste('test: ', nrow(data.test), 'lines',sep=''))
    r <- feature_each(data.test,test_end,0)
    ftr.test <- r$feature
  # save
    file_name <- paste(file_out_predix,itr,ite,vari_trainlabel,rate.pos_neg,sep='_')
    out_file <- paste(out_dir,file_name,'.Rda',sep = '')
    save(ftr.test,ftr.train_neg,ftr.train_pos,file = out_file)
    print(paste('end_time:',date()))
}

####################################################################################################################
svmf <- function(test_label_start,
                 itr,ite,
                 vari_trainlabel,
                 rate.pos_neg,
                 in_dir,file_in_predix,
                 out_dir,file_out_predix){
  # load library
    library(e1071)
    library(gplots)
    library(ROCR)
    
  # change wd to read data
    file_name <- paste(file_in_predix,itr,ite,vari_trainlabel,rate.pos_neg,sep='_')
    in_file <- paste(in_dir,file_name,'.Rda',sep='')
    load(in_file)
    print(paste('SVMF: inter_train:',itr,'inter_test:',
                ite,'vari:',vari_trainlabel,'rate:',rate.pos_neg,
                'time:',date(),sep=''))
  
  # train
    ftr.train_pos$class <- rep(1,nrow(ftr.train_pos))
    ftr.train_neg$class <- rep(0,nrow(ftr.train_neg))
    ftr.train <- rbind(ftr.train_pos,
                       ftr.train_neg)
#     ftr.train <- data.frame(ftr.train)
#     ftr.train[is.nan(ftr.train)] <- 0
#     x <- as.numeric(ftr.train[,4:(ncol(ftr.train)-1)])
    num_field <- c('btA','btB','btC','btA_t','btB_t','btC_t')
    x <- ftr.train[,num_field]
    x <- as.matrix(x)
    y <- as.numeric(ftr.train[,ncol(ftr.train)])
    df <- data.frame(x = x, y = y)
    model <- svm(y ~ x, data = df, type="C-classification", cost = 1, kernel = 'radial', prob = TRUE)
    
  # test
    #ftr.test[is.nan(ftr.test)] <- 0
    x <- ftr.test[,num_field]
    x <- as.matrix(x)
    result <- predict(model, newdata = data.frame(x = x), prob = TRUE)
    num.result <- as.numeric(result) - 1
    #perf <- data.frame(predict = num.result, real = y)
  
  # predict result and save
    pred_posui <- ftr.test[num.result == 1,1:2]
    file_name <- paste(file_out_predix,itr,ite,vari_trainlabel,rate.pos_neg,sep='_')
    out_file <- paste(out_dir,file_name,'.Rda',sep='')
    csv_name <- paste(out_dir,file_name,'.csv',sep='')
    write.csv(file = csv_name, x = pred_posui, row.names=FALSE)
    save(pred_posui,file = out_file)
    print(paste('end_time:',date()))
}





