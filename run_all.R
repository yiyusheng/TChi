# run all function
rm(list = ls())
git_dir <- "D:/Git/TChi/"
source(paste(git_dir,'TChi_trainset_testset.R',sep=''))
git_dir <- "D:/Git/TChi/"
source(paste(git_dir,'TChi_feature.R',sep=''))
git_dir <- "D:/Git/TChi/"
source(paste(git_dir,'TChi_svm.R',sep=''))
