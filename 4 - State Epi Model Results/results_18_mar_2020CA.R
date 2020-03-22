#***************************************** GRAPHS *****************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
# By: Alyssa Bilinski                                                                      #
#******************************************************************************************#

#### SETUP ####

# libraries
library(tidyverse)
library(stringr)

# set working directory
setwd("~/Dropbox/COVID19/4 - Serosurveys")
df = read.csv("parameters_18_mar_2020_CA_v2.csv", as.is = T)

# source files
source("~/Dropbox/COVID19/1 - Model/Most recent/model_3strat_18_mar_2020.R")

#### CHOOSE THESE ####

# day on which to start calibration
# we tried both 1 day and 16 day fits
day_start = 16

# day on which to end calibration
day_end = 16


############## CALIBRATION (CUMULATIVE CASES)
# Observed data (Santa Clara data. Cumulative number of cases)
ts = read.csv("~/Dropbox/COVID19/4 - Serosurveys/CA-CASES-Guess.csv", as.is = T) # These rows are for March 1st - 15th# Set a reasonable range of p

# function to run calibration
run_calib = function(p_cand, vec, obs = ts$cum_cases, cum = T, day_start = 1, day_end = 15){
  sumsq <- c()
  for (i in 1:length(p_cand)) {  # Select a candidate value of parameter p
    
    test = run_param_vec(params = vec, params2 = NULL, days_out1 = 60,
                         days_out2 = NULL, model_type = run_basic, obs.adj = p_cand[i])
    est = vec$k_report*vec$c*(test$I_1_cum+test$I_1Q_cum+test$A_1_cum+test$A_1Q_cum) + 
      vec$c*(test$I_2_cum+test$I_2Q_cum+test$A_2_cum+test$A_2Q_cum +
               test$I_3_cum+test$I_3Q_cum+test$A_3_cum+test$A_3Q_cum)
    # c is the reporting rate for adults
    # k_report is the relative reporting rate for kids  # Calculate the squared difference.
 
    if(cum) sumsq[i] = sum((est[day_start:day_end] - obs[day_start:day_end])^2)
    if(!cum)  sumsq[i] = sum((diff(est[day_start:(day_end-1)]) - diff(obs[day_start:(day_end-1)]))^2)
  }
  
  return(list(best.p = p_cand[which.min(sumsq)], est, obs))
  
}

#### RUN CALIBRATION ####

out = c()
keep = data.frame()

# loop over scenarios
for(k in c(1:4)){
  p_cand = 110:325
  # run initially 
  calib1 = run_calib(p_cand, df[k,], day_start = day_start, day_end = day_end)
  
  # pull fit
  results = run_calib(calib1[[1]], df[k,])
  out[k] = calib1[[1]]
  
  # store fit
  temp = data.frame(id = 1:16, cases = results[[2]][1:16], scenario = paste(df$Scenario[k], " (seed=", out[k], ")", sep = ""))
  keep = bind_rows(keep, temp)
}

# pull together results
keep = bind_rows(keep, data.frame(id = 1:16, cases = results[[3]], scenario = "Observed")) %>% mutate(lty = ifelse(scenario == "Observed", "2", "1"))

diff = day_end-day_start+1
# make plot 1
pdf(paste("fit_fig_", diff, ".pdf", sep = ""),width=6, height=3.5)

ggplot(keep, aes(x = id, y = cases, group = scenario, col = scenario, lty = lty)) + geom_line() + theme_minimal() + 
  labs(x = "Day", y = "Cumulative detected cases", title = paste("Fit to", diff, "day(s)")) + scale_linetype(guide = F) + 
  scale_color_brewer(name = "", palette = "Set2")

dev.off()

#### CALCULATE CUMULATIVE CASES ####

make_plots = function(test){
  
  # formatting
  out = test %>%
    gather(var, value, -time, -scenario) %>% separate(var, into = c("comp", "strat", "cum"), sep = "_") %>%
    mutate(cum = ifelse(is.na(cum), F, T),
           
           # reformat compartments
           comp2 = ifelse(comp=="A", "Asymptomatic", "Symptomatic"),
           comp2 = ifelse(comp=="E", "Exposed", comp2),
           comp2 = ifelse(comp=="R", "Recovered", comp2),
           comp2 = ifelse(comp=="S", "Susceptible", comp2),
           comp2 = factor(comp2, levels = c("Susceptible", "Exposed", "Asymptomatic",
                                            "Symptomatic", "Recovered")),
           
           # reformat strata
           strat2 = ifelse(strat=="1", "<20", "<20 (SD)"),
           strat2 = ifelse(strat=="2", "21-65", strat2),
           strat2 = ifelse(strat=="2Q", "21-65 (SD)", strat2),
           strat2 = ifelse(strat=="3", ">65", strat2),
           strat2 = ifelse(strat=="3Q", ">65 (SD)", strat2),
           strat2 = factor(strat2, levels = c("<20", "<20 (SD)",
                                              "21-65", "21-65 (SD)",
                                              ">65", ">65 (SD)")),
           
           # get only age
           strat3 = factor(sub(" \\(SD\\)", "", strat2), levels = c("<20", "21-65", ">65"))
    )
  
  # Cases by age
  out_cases = out %>% filter(cum == T & comp!="D") %>% group_by(time, comp, scenario) %>% 
    summarize(val2 = sum(value)) %>% group_by(time, scenario) %>% mutate(Total = sum(val2)) %>%
    gather(var, value, Total) %>% mutate(lab = ifelse(var=="Total", "Cumulative cases", "Hospitalized cases"))
  
  a = ggplot(out_cases, aes(x = time, y = value, group = scenario, col = scenario)) + geom_line() +
    theme_minimal() + scale_color_brewer(name = "", palette = "Set1") + labs(x = "Time (days)", y = "") + 
    facet_wrap(.~lab, scales = "free") + 
    theme(strip.text = element_text(size=12, face="bold")) + ylim(0, 150000)
  return(list(a, out_cases))
}

# reset test
test = data.frame()

# store seeds from before
df$obs = rep(out, 4)

# run estimates
# label is the label you want
# ind is the values to run over
run_ests = function(label, ind){
# loop over vectors
  for(i in ind){
  
      # run intervention for another 30 days
    test_int = run_param_vec(params = df[i,], params2 = df[i+4,], days_out1 = 15,
                             days_out2 = 45, model_type = run_int) %>% mutate(scenario = df$Scenario[i])
    test = bind_rows(test, test_int)
    
    
  }
  
  # make output
  out = make_plots(test)
  
  # store output
  write.csv(out[[2]] %>% filter(comp=="I"), file = paste0("Estimates_", label, ".csv"))
  
  # make plots
  pdf(paste0("rl_fig2_", label, ".pdf"),width=6.5, height=3.5)
  
  out[[1]]
   
  dev.off()

}

# run estimates
run_ests("No_SD", 1:4)
run_ests("SD", 9:12)

