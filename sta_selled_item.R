# statistic for selled item in both global and itemset
rm(list = ls())
data_dir <- "D:/Data/TChi/"
cur_dir <- getwd()

# change wd to read data
setwd(data_dir)
load('TChi_data.Rda')

# date generate
date.start <- as.Date('2014-11-18')
date.end <- as.Date('2014-12-18')
date.range <- seq(from = date.start, to = date.end, by = 'day')
sta_all <- matrix(0, nrow = length(date.range), ncol = 4)
sta_spec <- matrix(0, nrow = length(date.range), ncol = 4)
count = 1
#statistic
for (i in 1:length(date.range)) {
  d <- date.range[i]
  curr_alluser <- subset(data.alluser,time == d)
  curr_specuser <- subset(data.specuser,time == d)
  for (j in 1:4) {
    sta_all[count,j] = sum(curr_alluser$behavior_type == j)
    sta_spec[count,j] = sum(curr_specuser$behavior_type == j)
  }
  # number of selled item which is watched/collected/added before sell
  count = count + 1
}
save(sta_all,sta_spec,date.range,file = 'sta_selled_item.Rda')
