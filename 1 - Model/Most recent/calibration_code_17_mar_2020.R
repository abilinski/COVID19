#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

############## WORKING DIRECTOY AND MODEL FUNCTIONS
setwd("~/Dropbox/COVID19/0 - Parameters/")
source("~/Dropbox/COVID19/1 - Model/Most recent/model_3strat_18_mar_2020_1520EST.R")

n = 1938000

# these match US proportions
young = .24
medium = .6
old = .15

############## CALIBRATION (CUMULATIVE CASES)
# Observed data (Santa Clara data. Cumulative number of cases)
ts = read.csv("time_series_SCC.csv", as.is = T)[6:20,] # These rows are for March 1st - 15th# Set a reasonable range of p

# run calibration
run_calib = function(p_cand, vec, obs = ts$cum_cases, cum = T, day_start = 1, day_end = 15){
  sumsq <- c()
  for (i in 1:length(p_cand)) {  # Select a candidate value of parameter p

    test = run_param_vec(vec, p.adj = p_cand[i])
    est = vec$k_report*vec$c*(test$I_1_cum+test$I_1Q_cum+test$A_1_cum+test$A_1Q_cum) + 
      vec$c*(test$I_2_cum+test$I_2Q_cum+test$A_2_cum+test$A_2Q_cum +
           test$I_3_cum+test$I_3Q_cum+test$A_3_cum+test$A_3Q_cum)
    # c is the reporting rate for adults
    # k_report is the relative reporting rate for kids  # Calculate the squared difference.
    print(est[15])
    if(cum) sumsq[i] = sum((est[day_start:day_end] - obs[day_start:day_end])^2)
    if(!cum)  sumsq[i] = sum((diff(est[day_start:(day_end-1)]) - diff(obs[day_start:(day_end-1)]))^2)
  }
  
  return(list(best.p = p_cand[which.min(sumsq)], est, obs))
  
}

#### CUMULATIVE CASES

# run initially 
calib1 = run_calib(p_cand, vec, day_start = 15, day_end = 15)
  
# pull fit
results = run_calib(calib1[[1]], vec)
plot_data = data.frame(cases = c(results[[2]], results[[3]]), day = c(1:30, 1:15), 
                       id = c(rep("Observed", 30), rep("Actual", 15)))

ggplot(plot_data, aes(x = day, y = cases, group = id, col = id)) + geom_line() + 
  theme_minimal() + scale_color_discrete(name = "") + xlim(0, 15) + ylim(0, 100)


#### CHANGE IN CASES

calib2 = run_calib(p_cand, vec, cum = F)

# pull fit
results = run_calib(calib2[[1]], vec)
plot_data = data.frame(cases = c(results[[2]], results[[3]]), day = c(1:30, 1:15), 
                       id = c(rep("Observed", 30), rep("Actual", 15)))

ggplot(plot_data, aes(x = day, y = cases, group = id, col = id)) + geom_line() + 
  theme_minimal() + scale_color_discrete(name = "") + xlim(0, 15) + ylim(0, 100)

### R0
R0 = c()
for(i in 1:length(p_cand)){
  R0[i] = R_0(vec, n, p_cand[i], young, medium, old)
}

p_cand[which.min(abs(R0-2.28))]
R_0(vec, n, 0.05, young, medium, old)

