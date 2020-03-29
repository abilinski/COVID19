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
#setwd("~/Dropbox/COVID19/4 - Serosurveys")
wd <- getwd()
df = read.csv("parameters_18_mar_2020_NYC.csv", as.is = T)

setwd('..')

# source files
source("./1 - R Package/R/model.R")

setwd(wd)
#### CHOOSE THESE ####




############## CALIBRATION (CUMULATIVE CASES)
# Observed data (Santa Clara data. Cumulative number of cases)
ts = read.csv("NYC-Cases_Mar26.csv", as.is = T) # These rows are for March 1st - 15th# Set a reasonable range of p

#get length of time series
tot_days =dim(ts)[1]

# day on which to start calibration
# we tried both 1 day and 16 day fits
day_start = tot_days - 6; #21 #21

# day on which to end calibration
day_end = tot_days

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
#make vector of baseline scenarios to run
scen_vec = c(1:6,25:30,49:54)


# loop over scenarios
for(k in c(1:length(scen_vec))){
  p_cand = seq(100, 13000, 300) #110:325
  # run initially 
  calib1 = run_calib(p_cand, df[scen_vec[k],], day_start = day_start, day_end = day_end)
  
  # pull fit
  results = run_calib(calib1[[1]], df[scen_vec[k],])
  out[k] = calib1[[1]]
  
  # store fit
  temp = data.frame(id = 1:tot_days, cases = results[[2]][1:tot_days], scenario = paste(df$Scenario[scen_vec[k]], " (R0 = ", df$R0[scen_vec[k]], ", seed=", out[k], ")", sep = ""))
  keep = bind_rows(keep, temp)
}

# pull together results
keep = bind_rows(keep, data.frame(id = 1:tot_days, cases = results[[3]], scenario = "Observed")) %>% mutate(lty = ifelse(scenario == "Observed", "2", "1"))

diff = day_end-day_start+1
# make plot 1
# ggplot(keep, aes(x = id, y = cases, group = scenario, col = scenario, lty = lty)) + geom_line() + theme_minimal() + 
#   labs(x = "Day", y = "Cumulative detected cases", title = paste("Fit to", diff, "day(s)")) + scale_linetype(guide = F) #+ 
# #scale_color_brewer(name = "", palette = "Set2")
# 
# ggsave(paste("fit_fig_", diff, ".pdf", sep = ""),width=6, height=3.5)

#make plots for each R0
R0_labels <-c('3.2','3.5','3.8')
#get rows of keep that correspond to each R0 (379:399 is observed for all)
obs_inds = (18*tot_days+1):(19*tot_days)
R0_inds <- cbind(c(1:(6*tot_days),obs_inds),c((1+6*tot_days):(12*tot_days),obs_inds),c((1+12*tot_days):(18*tot_days),obs_inds)) #fix this so it's automatic
for (i in c(1:3)){
ggplot() + geom_line(keep[R0_inds[,i],], mapping = aes(x = id, y = cases, group = scenario, col = scenario, lty = lty)) + #geom_line() +
    geom_point(subset(keep[obs_inds,],id>=day_start), mapping= aes(x = id, y = cases)) + 
    theme_minimal() + 
  labs(x = "Day", y = "Cumulative detected cases", title = paste("Fit to", diff, "day(s)")) + scale_linetype(guide = F) + 
  scale_color_brewer(name = "", palette = "Set2")

ggsave(paste("fit_fig_", R0_labels[i],"_", diff, ".pdf", sep = ""),width=6, height=3.5)
}

#### CALCULATE CUMULATIVE CASES ####

make_plots = function(test,label){
  
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
  out_cases <- out %>% filter(cum == T & comp!="D") %>% group_by(time, comp, scenario) %>% 
    summarize(val2 = sum(value)) %>% group_by(time, scenario) %>% mutate(Total = sum(val2)) %>%
    gather(var, value, Total) %>% mutate(lab = ifelse(var=="Total", "Cumulative cases", "Hospitalized cases"))
  
  # make plots
  print(ggplot(out_cases, aes(x = time, y = value, group = scenario, col = scenario)) + geom_line() +
    theme_minimal() + scale_color_brewer(name = "", palette = "Set1") + labs(x = "Time (days)", y = "") + 
    facet_wrap(.~lab, scales = "free") + ggtitle(label) +
    theme(strip.text = element_text(size=12, face="bold")) + ylim(0, 8000000))
 # save plots
  ggsave(paste0("rl_fig2_", label, ".pdf"),width=6.5, height=3.5)
  
  return(list(out_cases))
}

# reset test
test = data.frame()

# store seeds from before
#df$obs = rep(out, 4)
#populate correct obs into each R0 scenario
df$obs[1:24]=rep(out[1:6],4) #R0 2.8
df$obs[25:48]=rep(out[7:12],4) #R0 2.2
df$obs[49:72]=rep(out[13:18],4) #R0 3.2 # change this back in future runs


# run estimates
# label is the label you want
# ind is the values to run over
run_ests = function(label, ind){
# loop over vectors
  for(i in ind){
   
      # run intervention for another 30 days
    test_int = run_param_vec(params = df[i,], params2 = df[i+6,], days_out1 = 15,
                             days_out2 = 60, model_type = run_int) %>% mutate(scenario = df$Scenario[i])
    test = bind_rows(test, test_int)
    
    
  }
  
  # make output
  out = make_plots(test,label)
  
  # store output
  write.csv(out[[1]] %>% filter(comp=="I"), file = paste0("Estimates_", label, ".csv"))
  return(out[[1]] %>% filter(comp=="I"))

}

# run estimates
no_sd_r03_2<-run_ests("No_SD_R0_3.2", 1:6)
no_sd_r03_5<-run_ests("No_SD_R0_3.5", 25:30)
no_sd_r03_8<-run_ests("No_SD_R0_3.8", 49:54)
sd_r03_2<-run_ests("SD25_R0_3.2", 13:18)
sd_r03_5<-run_ests("SD25_R0_3.5", 37:42)
sd_r03_8<-run_ests("SD25_R0_3.8", 61:66)

# use this if parameters have been changed to 50%
sd_r03_2<-run_ests("SD50_R0_3.2", 13:18)
sd_r03_5<-run_ests("SD50_R0_3.5", 37:42)
sd_r03_8<-run_ests("SD50_R0_3.8", 61:66)

diff_plots <- function(no_sd_mat,sd_mat,day,R0,SDpc){
  
  # This function makes a boxplot for percentage of cumulative cases averted (applies for hospitalizations too) 
  # at a certain time (given by the input day) across a range of scenarios for a given R0
  pdf(paste0("boxplot_R0_", R0,"_SD_",SDpc, ".pdf"),width=4.5, height=3.5)
  tmp_diff <- 100*(-no_sd_mat[no_sd_mat$time==day,'value']+sd_mat[sd_mat$time==day,'value'])/no_sd_mat[no_sd_mat$time==day,'value']
  boxplot(tmp_diff,main=paste0("% Change in Cumulative Cases\n R0 = ", R0, ", SD reduction = ", SDpc,"%"), ylab ="Percentage",ylim=c(-100, 0))
  
  dev.off()
  
}

diff_plots(no_sd_r03_2,sd_r03_2,50,3.2,25)
diff_plots(no_sd_r03_5,sd_r03_5,50,3.5,25)
diff_plots(no_sd_r03_8,sd_r03_8,50,3.8,25)

# use this if parameters have been changed to 50%
diff_plots(no_sd_r03_2,sd_r03_2,50,3.2,50)
diff_plots(no_sd_r03_5,sd_r03_5,50,3.5,50)
diff_plots(no_sd_r03_8,sd_r03_8,50,3.8,50)

library(data.table)
#calculate which day hospital beds exceeds 53000 count
#calculate difference with and without SD
hosp_days_plots <-function(no_sd_mat,sd_mat,beds,R0,SDpc){
  #turn time series into datatables
  setDT(no_sd_mat)
  setDT(sd_mat)
  #get first day that beds go over capacity for no sd and sd
  no_sd_mat<-no_sd_mat[no_sd_mat$value*.05 >beds,.SD[which.min(time)], by = scenario]
  sd_mat<-sd_mat[sd_mat$value*.05 >beds,.SD[which.min(time)], by = scenario]
  #take difference in days between sd and no sd for scenarios where matrix exists for sd
  hosp_days <-sd_mat$time -no_sd_mat[no_sd_mat$scenario %in% sd_mat$scenario,'time']
  pdf(paste0("ExtraBedDays_R0_", R0,"_SD_",SDpc, ".pdf"),width=6.5, height=3.5)
  boxplot(hosp_days,main=paste0("Extra Days before Exceeding Hosp Bed Capacity\n R0 = ", R0, ", SD reduction = ", SDpc,"%"), ylab ="Days")
  dev.off()
  return(as.list(no_sd_mat))
  
}

nyc_beds = 27000
hosp_days_plots(no_sd_r03_2,sd_r03_2,nyc_beds,3.2,25)
hosp_days_plots(no_sd_r03_5,sd_r03_5,nyc_beds,3.5,25)
hosp_days_plots(no_sd_r03_8,sd_r03_8,nyc_beds,3.8,25)

# use this if parameters have been changed to 50%
hosp_days_plots(no_sd_r03_2,sd_r03_2,nyc_beds,3.2,50)
hosp_days_plots(no_sd_r03_5,sd_r03_5,nyc_beds,3.5,50)
hosp_days_plots(no_sd_r03_8,sd_r03_8,nyc_beds,3.8,50)


# Default bar plot
hosp_time_plots <- function(scen_mat,hosp_pc){
  #input output from run_vecs
  #get mean, min and max cases
  cases_means<-out_cases %>%
    +     group_by(time) %>%
    +     summarise(mean = mean(value),min = min(value), max=max(value))
  #calculate hospital beds
  hosp_cases = cases_means*hosp_pc
  ggplot(hosp_cases,aes(time,mean))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=min, ymax=max))+
    theme_minimal() + scale_color_brewer(name = "", palette = "Set1") + labs(x = "Time (days)", y = "") +theme(strip.text = element_text(size=12, face="bold"))
}

