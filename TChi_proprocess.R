# data preprocess: 
#     duplicate
#     order
#     item set filter

rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()

# change wd to read data
setwd(data_dir)
item_file <- 'tianchi_mobile_recommend_train_item_0420.csv'
user_file_0402 <- 'tianchi_mobile_recommend_train_user_0402.csv'
user_file_0420 <- 'tianchi_mobile_recommend_train_user_0420.csv'
item <- read.csv(item_file)
user_0402 <- read.csv(user_file_0402)
user_0420 <- read.csv(user_file_0420)
user <- rbind(user_0402,user_0420)
#duplicated and order
dup.user <- user[!duplicated(user[c("user_id","item_id","time")]),]
order.user <- dup.user[order(dup.user["user_id"]),]
dup.item <- item[!duplicated(item[c("item_id","item_category","item_geohash")]),]
order.item <- dup.item[order(dup.item["item_id"]),]
ismember.user <- order.user[order.user$item_id %in% order.item$item_id,]
# length
len.item <- dim(order.item)[1]
len.user <- length(unique(ismember.user$user_id))
# rename and save
data.specuser <- ismember.user
data.alluser <- order.user
data.item <- order.item
# type convert
data.item$item_geohash <- as.character(data.item$item_geohash)
data.specuser$user_geohash <- as.character(data.specuser$user_geohash)
data.specuser$time <- as.Date(data.specuser$time, format = "%Y-%m-%d %H")
data.alluser$user_geohash <- as.character(data.alluser$user_geohash)
data.alluser$time <- as.Date(data.alluser$time, format = "%Y-%m-%d %H")
# ui column generate (user_id item_id paste)
data.alluser$ui <- paste(data.alluser$user_id,data.alluser$item_id,'_')
data.specuser$ui <- paste(data.specuser$user_id,data.specuser$item_id,'_')
# ic column generate (item_id item_category)
data.alluser$ic <- paste(data.alluser$item_category,data.alluser$item_id,'_')
data.specuser$ic <- paste(data.specuser$item_category,data.specuser$item_id,'_')
data.item$ic <- paste(data.item$item_category,data.item$item_category,'_')
# save
save(data.specuser,data.alluser,data.item,file = 'TChi_data.Rda')
