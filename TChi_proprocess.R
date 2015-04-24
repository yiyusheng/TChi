# data preprocess: 
#     duplicate
#     order
#     item set filter

# change wd to read data
  rm(list = ls())
  if (.Platform$OS.type == 'windows') {
    Git_dir <- "D:/Git/TChi/"
    Data_dir <- "D:/Data/TChi/"
  }else {
    Git_dir <- "/home/yiyusheng/Git/TChi/"
    Data_dir <- "/home/yiyusheng/Data/"
  }
  item_file <- 'tianchi_mobile_recommend_train_item_0420.csv'
  user_file_0402 <- 'tianchi_mobile_recommend_train_user_0402.csv'
  user_file_0420 <- 'tianchi_mobile_recommend_train_user_0420.csv'
  item <- read.csv(paste(Data_dir,item_file,sep=''))
  user_0402 <- read.csv(paste(Data_dir,user_file_0402,sep=''))
  user_0420 <- read.csv(paste(Data_dir,user_file_0420,sep=''))
  user <- rbind(user_0402,user_0420)
  
#duplicated and order
  dup.user <- user[!duplicated(user[c("user_id","item_id","time")]),]
  order.user <- dup.user[order(dup.user["user_id"]),]
  dup.item <- item[!duplicated(item[c("item_id","item_category","item_geohash")]),]
  order.item <- dup.item[order(dup.item["item_id"]),]
  ismember.user <- order.user[order.user$item_id %in% order.item$item_id,]
  
# rename
  data.specuser <- ismember.user
  data.alluser <- order.user
  data.item <- order.item
  
# type convert(geohash -> numeric)
  item_g <- data.item$item_geohash
  user_g <- data.alluser$user_geohash
  users_g <- data.specuser$user_geohash
  len_ig <- length(item_g)
  len_ug <- length(user_g)
  leng_usg <- length(users_g)
  merge_g <- as.factor(c(as.character(item_g),
                       as.character(user_g),
                       as.character(users_g)))
  item_g <- merge_g[1:len_ig]
  user_g <- merge_g[(len_ig+1):(len_ig+len_ug)]
  users_g <- merge_g[(len_ig+len_ug+1):length(merge_g)]
  data.item$item_geohash <- as.numeric(item_g)
  data.specuser$user_geohash <- as.numeric(users_g)
  data.alluser$user_geohash <- as.numeric(user_g)
  
# type convert(time -> Date)
  data.specuser$time <- as.numeric(as.POSIXct(data.specuser$time, format = "%Y-%m-%d %H"))
  data.alluser$time <- as.numeric(as.POSIXct(data.alluser$time, format = "%Y-%m-%d %H"))

# item_geohash add
  data.alluser$item_geohash <- rep(1,nrow(data.alluser))
  data.alluser$item_geohash[match(item$item_id,data.alluser$item_id)] <- data.item$item_geohash
  data.specuser$item_geohash <- rep(1,nrow(data.specuser))
  data.specuser$item_geohash[match(item$item_id,data.specuser$item_id)] <- data.item$item_geohash

# ui column generate (user_id item_id paste)
  data.alluser$ui <- as.factor(data.alluser$user_id*10000000000+data.alluser$item_id)
  data.specuser$ui <- as.factor(data.specuser$user_id*10000000000+data.specuser$item_id)
# ic column generate (item_id item_category)
  data.alluser$ic <- as.factor(data.alluser$item_category*10000000000+data.alluser$item_id)
  data.specuser$ic <- as.factor(data.specuser$item_category*10000000000+data.specuser$item_id)
  data.item$ic <- as.factor(data.item$item_category*10000000000+data.item$item_category)
# save
#   data.specuser <- data.frame(as.matrix(data.specuser))
#   data.alluser <- data.frame(as.matrix(data.alluser))
#   data.item <- data.frame(as.matrix(data.item))
  save(data.specuser,data.alluser,data.item,file = paste(Data_dir,'TChi_data.Rda',sep=''))
