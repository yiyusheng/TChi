# new feature
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

# choose sample to test
time <- as.character(ismember.user$time)
real.time <- as.Date(time, format = "%y-%m-%d %H")