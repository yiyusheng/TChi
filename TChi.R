data_dir <- "D:/Data/TChi/"
item_file <- 'tianchi_mobile_recommend_train_item.csv'
user_file <- 'tianchi_mobile_recommend_train_user.csv'
cur_dir <- getwd()
# change wd to read data
setwd(data_dir)
# read data
item <- read.csv(item_file)
user <- read.csv(user_file)
#duplicated and order
dup.user <- user[!duplicated(user[c("user_id","item_id","time")]),]
order.user <- dup.user[order(dup.user["user_id"]),]
dup.item <- item[!duplicated(item[c("item_id","item_category","item_geohash")]),]
order.item <- dup.item[order(dup.item["item_id"]),]
ismember.user <- order.user[order.user$item_id %in% order.item$item_id,]
# length
len.item <- dim(order.item)[1]
len.user <- length(unique(ismember.user$user_id))



# user calculation
uni.uid <- unique(ismember.user$user_id)
len.uid <- length(uni.uid)
user.feature <- matrix(0, nrow = len.uid, ncol = 4)
p <- 1
curr.uid <- ismember.user$user_id[1]
user.feature[1,ismember.user$behavior_type[p]] <- 1
for (i in 2:dim(ismember.user)[1]) {
  if (curr.uid == ismember.user$user_id[i]){
    user.feature[p,ismember.user$behavior_type[i]] <- user.feature[p,ismember.user$behavior_type[i]] + 1
  }
  else{
    p <- p + 1
    user.feature[p,ismember.user$behavior_type[i]] <- 1
    curr.uid <- ismember.user$user_id[i]
  }
}


# item calculation
ismember.user <- ismember.user[order(ismember.user$item_id),]
uni.iid <- unique(ismember.user$item_id)
len.iid <- length(uni.iid)
item.feature <- matrix(0, nrow = len.iid, ncol = 4)
p <- 1
curr.iid <- ismember.user$item_id[1]
item.feature[1,ismember.user$behavior_type[p]] <- 1
for (i in 2:dim(ismember.user)[1]) {
  if (curr.iid == ismember.user$item_id[i]){
    item.feature[p,ismember.user$behavior_type[i]] <- item.feature[p,ismember.user$behavior_type[i]] + 1
  }
  else{
    p <- p + 1
    item.feature[p,ismember.user$behavior_type[i]] <- 1
    curr.iid <- ismember.user$item_id[i]
  }
}

