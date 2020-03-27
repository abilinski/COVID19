#cd /Users/linzhu/Box Sync/Lin Zhu's Files/git/COVID19/1 - R Package
setwd("~/Box Sync/Lin Zhu's Files/git/COVID19/1 - R Package")
#source("~/Box Sync/Lin Zhu's Files/git/COVID19/1 - R Package/R/model.R")
devtools::load_all()

params <- load_parameters_table()

test1 <- run_param_vec(params[1,])

test2 <- run_param_vec(params[11,])

test1 <- test1[,c(1,44)]
test1$scenario <- "base"

test2 <- test2[,c(1,44)]
test2$scenario <- "screen"

test <- rbind(test1, test2)

ggplot(test, aes(x=time, y=I_1_cum, group=scenario, color=scenario)) +
  geom_line()

#debugging for e not functioning
source('~/Downloads/model_3strat_18_mar_2020 (1).R')

params <- read.csv("/Users/linzhu/Box Sync/Lin Zhu's Files/git/COVID19/0 - Parameters/parameters_17_mar_2020.csv")
params$s <- 0.5
params[2,"e"] <- 0.5
params[3,"e"] <- 0.9

testi1 = run_param_vec(params = params[1,], params2 = params[2,], days_out1 = 15,
                      days_out2 = 30, model_type = run_int)

testi2 = run_param_vec(params = params[1,], params2 = params[3,], days_out1 = 15,
                      days_out2 = 30, model_type = run_int)

testp1 <- testi1[, c(1, 33)]
testp1$e <- 0.5
testp2 <- testi2[, c(1, 33)]
testp2$e <- 0.9
test <- rbind(testp1,testp2)
test$e <- as.factor(test$e)

ggplot(test, aes(x=time, y=I_2_cum, group=e)) +
  geom_line()


params <- read.csv("/Users/linzhu/Box Sync/Lin Zhu's Files/git/COVID19/0 - Parameters/parameters_17_mar_2020.csv")
params$e <- 1
params[2,"s"] <- 0
params[3,"s"] <- 1

testi1 = run_param_vec(params = params[1,], params2 = params[2,], days_out1 = 2,
                       days_out2 = 30, model_type = run_int)

testi2 = run_param_vec(params = params[1,], params2 = params[3,], days_out1 = 2,
                       days_out2 = 30, model_type = run_int)

testp1 <- testi1[, c(1, 4, 32)]
testp1$s <- 0
testp1$cum_I_1 <- cumsum(testp1$I_1)

testp2 <- testi2[, c(1, 19, 35)]
testp2$s <- 1
testp2$cum_I_1 <- cumsum(testp2$I_1Q)
colnames(testp2)[2] <- "I_1"
colnames(testp2)[3] <- "I_1_cum"

test <- rbind(testp1,testp2)
test$s <- as.factor(test$s)

ggplot(test, aes(x=time, y=I_1_cum, group=s, color=s)) +
  geom_line()
ggplot(test, aes(x=time, y=cum_I_1, group=s, color=s)) +
  geom_line()

a <- testi1[, c(4, 32)]
