data_dir <- "D:/Data/TChi/"
item_file <- 'tianchi_mobile_recommend_train_item.csv'
user_file <- 'tianchi_mobile_recommend_train_user.csv'
cur_dir <- getwd()
# change wd to read data
setwd(data_dir)
# read data
item <- read.csv(item_file)
item$item_geohash <- as.character(item$item_geohash)
user <- read.csv(user_file)
user$user_geohash <- as.character(user$user_geohash)
user$time <- as.Date(user$time, format = "%Y-%m-%d %H")
#duplicated and order
dup.user <- user[!duplicated(user[c("user_id","item_id")]),]
order.user <- dup.user[order(dup.user["user_id"]),]
dup.item <- item[!duplicated(item[c("item_id","item_category","item_geohash")]),]
order.item <- dup.item[order(dup.item["item_id"]),]
ismember.user <- order.user[order.user$item_id %in% order.item$item_id,]
# length
len.item <- dim(order.item)[1]
len.user <- length(unique(ismember.user$user_id))
########################################################################################################
#predict parameter
train_start <- '2014-11-17'
train_end <- '2014-12-18'
train_label_start <- '2014-12-18'
train_label_end <- '2014-12-19'
test_start <- '2014-12-18'
test_end <- '2014-12-19'
test_label_start <- ''
test_label_end <- ''

#train_pos <- unique()
data.train_pos_label <- ismember.user[time %between% c(train_label_start,train_label_end)]
uipair.train_pos_label <- unique(ismember.user[c("user_id","item_id")])
train_pos_data <- ismember.user[time %between% c(train_start,train_end), ]
