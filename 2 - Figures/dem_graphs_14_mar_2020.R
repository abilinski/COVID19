#***************************************** GRAPHS *****************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
# By: Alyssa Bilinski                                                                      #
# Created: March 14, 2020                                                                  #
#******************************************************************************************#

#### SETUP ####

# libraries
library(tidyverse)
library(stringr)

# set working directory
setwd("~/Dropbox/COVID19/0 - Parameters")

#### DEMOGRAPHIC GRAPHS ####

df = read.csv("Demographic data 14_mar_2020.csv") %>% gather(var, value, LT18, X18to64, X65plus) %>%
  mutate(var2 = ifelse(var=="LT18", "<20", "20-64"),
         var2 = ifelse(var=="X65plus", "65+", var2),
         Location = str_wrap(Location, width = 20))

g = ggplot(df, aes(x = Location, y = value, fill = var2, group = var2)) + geom_bar(stat = "identity") + 
  theme_minimal() + scale_fill_discrete(name = "") + geom_text(aes(label = value),
                                                               position = position_stack(vjust = 0.5)) + 
  #labs(y = "Percentage", title = "Age distribution by location") + 
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### OUTPUT GRAPHS ####
setwd("~/Dropbox/COVID19/2 - Figures")


pdf("dem.pdf",width=5, height=5)
g
dev.off()

#### DEMOGRAPHIC GRAPHS ####

source("~/Dropbox/COVID19/1 - Model/Most recent/model_3strat_18_mar_2020.R")

# set working directory
setwd("~/Dropbox/COVID19/0 - Parameters")

# read in parameters
df = read.csv("parameters_17_mar_2020.csv", as.is = T)

# Josh scenario
  params = df[2,]

  # run scenario for 30 days
  test = run_param_vec(params = df[2,], params2 = NULL, days_out1 = 30,
                     days_out2 = NULL, model_type = run_basic)

pdf("fig1.pdf",width=7, height=3)
  
  # make plots
  f = make_plots(test, params = params)
  multiplot(f[[8]], f[[2]], cols = 2)
  
dev.off()
  
  
# Low kid infectivity
  params = df[18,]
  
  # run scenario for 30 days
  test = run_param_vec(params = df[16,], params2 = NULL, days_out1 = 30,
                       days_out2 = NULL, model_type = run_basic)
  
  pdf("fig2.pdf",width=7, height=3)
  
  # make plots
  g = make_plots(test, params = params)
  multiplot(g[[8]], g[[2]], cols = 2)
  
  dev.off()
  
  
# Schools close
  params_int = df[4,]
  
  # run base for 15 days
  test = run_param_vec(params = df[2,], params2 = NULL, days_out1 = 15,
                       days_out2 = NULL, model_type = run_basic)

  # run intervention for another 30 days
  test_int = run_param_vec(params = df[2,], params2 = params_int, days_out1 = 15,
                           days_out2 = 30, model_type = run_int)
  h = make_plots(test_int, params = params_int)
  
  # make plots
  multiplot(f[[2]], h[[2]], cols = 2)
  
  pdf("fig3.pdf",width=7, height=3)
  
  multiplot(f[[2]], h[[2]], cols = 2)
  
  dev.off()
  
  # Schools close
  params_int = df[5,]
  
  # run base for 15 days
  test = run_param_vec(params = df[2,], params2 = NULL, days_out1 = 15,
                       days_out2 = NULL, model_type = run_basic)
  
  # run intervention for another 30 days
  test_int = run_param_vec(params = df[2,], params2 = params_int, days_out1 = 15,
                           days_out2 = 30, model_type = run_int)
  i = make_plots(test_int, params = params_int)
  
  # make plots

  pdf("fig4.pdf",width=7, height=3)
  
  multiplot(f[[2]], i[[2]], cols = 2)
  
  dev.off()
  

