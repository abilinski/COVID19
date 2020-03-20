setwd("/Users/linzhu/Box Sync/Lin Zhu's Files/git/COVID19/0 - Parameters")
source("/Users/linzhu/Box Sync/Lin Zhu's Files/git/COVID19/1 - Model/Most recent/model_3strat_18_mar_2020_detect_rate.R")

params <- read.csv("parameters_18_mar_2020.csv")

test1 <- run_param_vec(params[1,])

test2 <- run_param_vec(params[11,])
