#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#

############## STRATIFIED MODEL
model_strat <- function (t, x, ...) {
  
  # initial conditions
  S1 = x[1]; E1 = x[2]; I1 = x[3]; A1 = x[4]; R1 = x[5]
  S2 = x[6]; E2 = x[7]; I2 = x[8]; A2 = x[9]; R2 = x[10]
  S3 = x[11]; E3 = x[12]; I3 = x[13]; A3 = x[14]; R3 = x[15]
  S1Q = x[16]; E1Q = x[17]; I1Q = x[18]; A1Q = x[19]; R1Q = x[20]
  S2Q = x[21]; E2Q = x[22]; I2Q = x[23]; A2Q = x[24]; R2Q = x[25]
  S3Q = x[26]; E3Q = x[27]; I3Q = x[28]; A3Q = x[29]; R3Q = x[30]
  
  # attach parameters into workspace
  #attach(params)
  
  ###### Equations
  ### YOUNG
  dS1dt = -S1*k_susp*p*(k_inf*v11*I1/N1 + vA11*A1/N1 + v21*I2/N2 + vA21*A2/N2 + v31*I3/N3 + vA31*A3/N3 + k_inf*v1Q1*I1Q/N1Q + vA1Q1*A1Q/N1Q + v2Q1*I2Q/N2Q + vA2Q1*A2Q/N2Q + v3Q1*I3Q/N3Q + vA3Q1*A3Q/N3Q)
  dE1dt = -delta*E1+ S1*k_susp*p*(k_inf*v11*I1/N1 + vA11*A1/N1 + v21*I2/N2 + vA21*A2/N2 + v31*I3/N3 + vA31*A3/N3 + k_inf*v1Q1*I1Q/N1Q + vA1Q1*A1Q/N1Q + v2Q1*I2Q/N2Q + vA2Q1*A2Q/N2Q + v3Q1*I3Q/N3Q + vA3Q1*A3Q/N3Q)
  dI1dt = (1-alpha1)*delta*E1- (m1*omega + (1-m1)*gamma)*I1                                       
  dA1dt = alpha1*delta*E1 - gamma*A1                                                              
  dR1dt = gamma*A1+ (1-m1)*gamma*I1
  It1 = (1-alpha1)*delta*E1
  At1 = alpha1*delta*E1
  Dt1 = m1*omega*I1
  
  ### MEDIUM
  dS2dt = -S2*p*(k_inf*v12*I1/N1 + vA12*A1/N1 + v22*I2/N2 + vA22*A2/N2 + v32*I3/N3 + vA32*A3/N3 + k_inf*v1Q2*I1Q/N1Q + vA1Q2*A1Q/N1Q + v2Q2*I2Q/N2Q + vA2Q2*A2Q/N2Q + v3Q2*I3Q/N3Q + vA3Q2*A3Q/N3Q)
  dE2dt = -delta*E2+ S2*p*(k_inf*v12*I1/N1 + vA12*A1/N1 + v22*I2/N2 + vA22*A2/N2 + v32*I3/N3 + vA32*A3/N3 + k_inf*v1Q2*I1Q/N1Q + vA1Q2*A1Q/N1Q + v2Q2*I2Q/N2Q + vA2Q2*A2Q/N2Q + v3Q2*I3Q/N3Q + vA3Q2*A3Q/N3Q)
  dI2dt = (1-alpha2)*delta*E2- (m2*omega + (1-m2)*gamma)*I2
  dA2dt = alpha2*delta*E2 - gamma*A2
  dR2dt = gamma*A2+ (1-m2)*gamma*I2
  It2 = (1-alpha2)*delta*E2
  At2 = alpha2*delta*E2
  Dt2 = m2*omega*I2
  
  ### OLD
  dS3dt = -S3*p*(k_inf*v13*I1/N1 + vA13*A1/N1 + v23*I2/N2 + vA23*A2/N2 + v33*I3/N3 + vA33*A3/N3 + k_inf*v1Q3*I1Q/N1Q + vA1Q3*A1Q/N1Q + v2Q3*I2Q/N2Q + vA2Q3*A2Q/N2Q + v3Q3*I3Q/N3Q + vA3Q3*A3Q/N3Q)
  dE3dt = -delta*E3+ S3*p*(k_inf*v13*I1/N1 + vA13*A1/N1 + v23*I2/N2 + vA23*A2/N2 + v33*I3/N3 + vA33*A3/N3 + k_inf*v1Q3*I1Q/N1Q + vA1Q3*A1Q/N1Q + v2Q3*I2Q/N2Q + vA2Q3*A2Q/N2Q + v3Q3*I3Q/N3Q + vA3Q3*A3Q/N3Q)
  dI3dt = (1-alpha3)*delta*E3- (m3*omega + (1-m3)*gamma)*I3
  dA3dt = alpha3*delta*E3 - gamma*A3
  dR3dt = gamma*A3+ (1-m3)*gamma*I3
  It3 = (1-alpha3)*delta*E3
  At3 = alpha3*delta*E3
  Dt3 = m3*omega*I3
  
  ### YOUNG - SOCIALLY DISTANCED
  dS1Qdt = -S1Q*k_susp*p*(k_inf*v11Q*I1/N1 + vA11Q*A1/N1 + v21Q*I2/N2 + vA21Q*A2/N2 + v31Q*I3/N3 + vA31Q*A3/N3 + k_inf*v1Q1Q*I1Q/N1Q + vA1Q1Q*A1Q/N1Q + v2Q1Q*I2Q/N2Q + vA2Q1Q*A2Q/N2Q + v3Q1Q*I3Q/N3Q + vA3Q1Q*A3Q/N3Q)
  dE1Qdt = -delta*E1Q+ S1Q*k_susp*p*(k_inf*v11Q*I1/N1 + vA11Q*A1/N1 + v21Q*I2/N2 + vA21Q*A2/N2 + v31Q*I3/N3 + vA31Q*A3/N3 + k_inf*v1Q1Q*I1Q/N1Q + vA1Q1Q*A1Q/N1Q + v2Q1Q*I2Q/N2Q + vA2Q1Q*A2Q/N2Q + v3Q1Q*I3Q/N3Q + vA3Q1Q*A3Q/N3Q)
  dI1Qdt = (1-alpha1Q)*delta*E1Q- (m1Q*omega + (1-m1Q)*gamma)*I1Q
  dA1Qdt = alpha1Q*delta*E1Q - gamma*A1Q
  dR1Qdt = gamma*A1Q+ (1-m1Q)*gamma*I1Q
  It1Q = (1-alpha1Q)*delta*E1Q
  At1Q = alpha1Q*delta*E1Q
  Dt1Q = m1Q*omega*I1Q
  
  ### MEDIUM - SOCIALLY DISTANCED
  dS2Qdt = -S2Q*p*(k_inf*v12Q*I1/N1 + vA12Q*A1/N1 + v22Q*I2/N2 + vA22Q*A2/N2 + v32Q*I3/N3 + vA32Q*A3/N3 + k_inf*v1Q2Q*I1Q/N1Q + vA1Q2Q*A1Q/N1Q + v2Q2Q*I2Q/N2Q + vA2Q2Q*A2Q/N2Q + v3Q2Q*I3Q/N3Q + vA3Q2Q*A3Q/N3Q)
  dE2Qdt = -delta*E2Q+ S2Q*p*(k_inf*v12Q*I1/N1 + vA12Q*A1/N1 + v22Q*I2/N2 + vA22Q*A2/N2 + v32Q*I3/N3 + vA32Q*A3/N3 + k_inf*v1Q2Q*I1Q/N1Q + vA1Q2Q*A1Q/N1Q + v2Q2Q*I2Q/N2Q + vA2Q2Q*A2Q/N2Q + v3Q2Q*I3Q/N3Q + vA3Q2Q*A3Q/N3Q)
  dI2Qdt = (1-alpha2Q)*delta*E2Q- (m2Q*omega + (1-m2Q)*gamma)*I2Q
  dA2Qdt = alpha2Q*delta*E2Q - gamma*A2Q
  dR2Qdt = gamma*A2Q+ (1-m2Q)*gamma*I2Q
  It2Q = (1-alpha2Q)*delta*E2Q
  At2Q = alpha2Q*delta*E2Q
  Dt2Q = m2Q*omega*I2Q
  
  ### OLD - SOCIALLY DISTANCED
  dS3Qdt = -S3Q*p*(k_inf*v13Q*I1/N1 + vA13Q*A1/N1 + v23Q*I2/N2 + vA23Q*A2/N2 + v33Q*I3/N3 + vA33Q*A3/N3 + k_inf*v1Q3Q*I1Q/N1Q + vA1Q3Q*A1Q/N1Q + v2Q3Q*I2Q/N2Q + vA2Q3Q*A2Q/N2Q + v3Q3Q*I3Q/N3Q + vA3Q3Q*A3Q/N3Q)
  dE3Qdt = -delta*E3Q+ S3Q*p*(k_inf*v13Q*I1/N1 + vA13Q*A1/N1 + v23Q*I2/N2 + vA23Q*A2/N2 + v33Q*I3/N3 + vA33Q*A3/N3 + k_inf*v1Q3Q*I1Q/N1Q + vA1Q3Q*A1Q/N1Q + v2Q3Q*I2Q/N2Q + vA2Q3Q*A2Q/N2Q + v3Q3Q*I3Q/N3Q + vA3Q3Q*A3Q/N3Q)
  dI3Qdt = (1-alpha3Q)*delta*E3Q- (m3Q*omega + (1-m3Q)*gamma)*I3Q
  dA3Qdt = alpha3Q*delta*E3Q - gamma*A3Q
  dR3Qdt = gamma*A3Q+ (1-m3Q)*gamma*I3Q
  It3Q = (1-alpha3Q)*delta*E3Q
  At3Q = alpha3Q*delta*E3Q
  Dt3Q = m3Q*omega*I3Q
  
  # results
  output <- c(dS1dt, dE1dt, dI1dt, dA1dt, dR1dt, 
              dS2dt, dE2dt, dI2dt, dA2dt, dR2dt,
              dS3dt, dE3dt, dI3dt, dA3dt, dR3dt, 
              dS1Qdt, dE1Qdt, dI1Qdt, dA1Qdt, dR1Qdt, 
              dS2Qdt, dE2Qdt, dI2Qdt, dA2Qdt, dR2Qdt, 
              dS3Qdt, dE3Qdt, dI3Qdt, dA2Qdt, dR3Qdt, 
              It1, It2, It3, It1Q, It2Q, It3Q,
              At1, At2, At2, At1Q, At2Q, At3Q,
              Dt1, Dt2, Dt2, Dt1Q, Dt2Q, Dt3Q)
  
  # list it!
  list(output)
}

############## RUN ODE
run_model <- function(func, xstart, times, params, method = "lsodes") {
  return(as.data.frame(ode(func = func, y = xstart, times = times, parms = params, method = method)))
}

############## POST-PROCESSING
  
make_plots = function(test, params){
  
  k_report = params$k_report
  c = params$c
  
  # formatting
  out = test %>%
    gather(var, value, -time) %>% separate(var, into = c("comp", "strat", "cum"), sep = "_") %>%
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
  
  
  # make graphs of output over time
  out_age = out %>% group_by(comp, cum) %>% summarize(sum(value))

  # Flows by compartment
  a = ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2) %>% summarize(value = sum(value)), 
         aes(x = time, y = value, group = comp2, col = comp2)) + geom_line() + theme_minimal() + 
    scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
  
  # Cases by age
  out_cases = out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3) %>% 
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2),
                                                               val_obs = ifelse(strat3=="<20", k_report*c*val2, c*val2),
                                                               Total_obs = sum(val_obs))
  b = ggplot(out_cases, aes(x = time, y = val2, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                           title = "Cumulative cases by age")
  
  # Effective R
  out_Re = out %>% filter(comp=="I") %>% spread(cum, value) %>% group_by(time, comp2) %>%
    summarize(existing_inf = sum(`FALSE`), new_inf = sum(`TRUE`), ratio = new_inf/existing_inf)
  c = ggplot(out_Re, aes(x = time, y = ratio)) + geom_line() + 
           theme_minimal() + scale_color_discrete(name = "") + 
           labs(x = "Time (days)", y = "", title = "Ratio of new to existing cases")
         
  
  # Observed cases by age
  d = ggplot(out_cases, aes(x = time, y = val_obs,
                        group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total_obs), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Observed cumulative cases by age")
  
  
  # Deaths by age
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3) %>% 
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2))
  e = ggplot(out_death, aes(x = time, y = val2, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative deaths by age")
  
  
  # Cases by symptoms
  out_symp = out %>% filter(cum == T & comp!="D" & !is.na(strat3)) %>% group_by(time, comp2) %>% 
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2))
  f = ggplot(out_symp, aes(x = time, y = val2, group = comp2, col = comp2)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative cases by symptoms")
  
  # Flows by compartment
  g = ggplot(out %>% filter(cum ==F), aes(x = time, y = value, group = comp, col = comp2)) + geom_line() + 
    facet_wrap(.~strat, ncol = 2) + theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
  
  return(list(a,b,c,d,e,f,g))
}

############## RUN CODE

# libraries
library(tidyverse)
library(deSolve)
library(ggthemes)
library(tictoc)

# set working directory
setwd("~/Dropbox/COVID19/0 - Parameters")

# read in parameters
params = read.csv("parameters_17_mar_2020.csv", as.is = T)[1,]
  attach(params)
  
  #### ADJUSTMENTS BASED ON MODEL SIMPLIFICATIONS 
  # same time to infection whether recovery or death
  omega = gamma
  
  # asymptomatic and mortality prob same whether socially distanced (Q) or no
  alpha1Q = alpha1
  alpha2Q = alpha2
  alpha3Q = alpha3
  m1Q = m1
  m2Q = m2
  m3Q = m3
  
  # contact rates for socially distanced
  # contact rates between socially distanced and non-socially distanced are social distance rates
  v1Q1Q = v1Q1 = v11Q = e*v11
  v1Q2Q = v1Q2 = v12Q = e*v12
  v1Q3Q = v1Q3 = v13Q = e*v13
  
  v2Q1Q = v2Q1 = v21Q = e*v21
  v2Q2Q = v2Q2 = v22Q = e*v22
  v2Q3Q = v2Q3 = v23Q = e*v23
  
  v3Q1Q = v3Q1 = v31Q = e*v31
  v3Q2Q = v3Q2 = v32Q = e*v32
  v3Q3Q = v3Q3 = v33Q = e*v33
  
  # contact rates are same whether asymptomatic or no
  # (this might be adjusted with some interventions)
  # but they have lower infection prob (kappa)
  vA11 = v11*kappa
  vA12 = v12*kappa
  vA13 = v13*kappa
  vA11Q = v11Q*kappa
  vA12Q = v12Q*kappa
  vA13Q = v13Q*kappa
  
  vA21 = v21*kappa
  vA22 = v22*kappa
  vA23 = v23*kappa
  vA21Q = v21Q*kappa
  vA22Q = v22Q*kappa
  vA23Q = v23Q*kappa
  
  vA31 = v31*kappa
  vA32 = v32*kappa
  vA33 = v33*kappa
  vA31Q = v31Q*kappa
  vA32Q = v32Q*kappa
  vA33Q = v33Q*kappa
  
  vA1Q1 = v1Q1*kappa
  vA1Q2 = v1Q2*kappa
  vA1Q3 = v1Q3*kappa
  vA1Q1Q = v1Q1Q*kappa
  vA1Q2Q = v1Q2Q*kappa
  vA1Q3Q = v1Q3Q*kappa
  
  vA2Q1 = v2Q1*kappa
  vA2Q2 = v2Q2*kappa
  vA2Q3 = v2Q3*kappa
  vA2Q1Q = v2Q1Q*kappa
  vA2Q2Q = v2Q2Q*kappa
  vA2Q3Q = v2Q3Q*kappa
  
  vA3Q1 = v3Q1*kappa
  vA3Q2 = v3Q2*kappa
  vA3Q3 = v3Q3*kappa
  vA3Q1Q = v3Q1Q*kappa
  vA3Q2Q = v3Q2Q*kappa
  vA3Q3Q = v3Q3Q*kappa
  
  ############## SET INITIAL CONDITIONS
  
  # demographics
  p = 0.05
  n = 1938000
  
  # these match US proportions
  young = .24
  medium = .6
  old = .15
  
  # assuming you don't have specific reporting rates
  # you can instead pull from obs_adults and obs_kids by 
  # adding to parameter vector
  #*** this code is being grumpy (need more edits down the line) so I held off for now
  #obs_adults = obs*(medium + old)
  #obs_kids = obs*young
  #start = obs_adults/(c*(medium+old))
  #start_kids = obs_kids/(c*k_report*young)
  start = start_kids = 24
  
  N1 = if_else(n*(1-s)*young == 0, 1, n*(1-s)*young)
  N2 = if_else(n*(1-s)*medium == 0, 1, n*(1-s)*medium)
  N3 = if_else(n*(1-s)*old == 0, 1, n*(1-s)*old)
  N1Q = if_else(n*(s)*young == 0, 1, n*(s)*young)
  N2Q = if_else(n*(s)*medium == 0, 1, n*(s)*medium)
  N3Q = if_else(n*(s)*old == 0, 1, n*(s)*old)
  
  x = data.frame(
    
    # initial conditions
    S_1 = n*(1-s)*young - start_kids*young*(1-s),
    E_1 = start_kids*(1-s)*young,
    I_1 = start_kids*(1-s)*young*(1-alpha1),
    A_1 = start_kids*(1-s)*young*(alpha1),
    R_1 = 0,
    
    S_2 = n*(1-s)*medium - start*medium*(1-s),
    E_2 = start*(1-s)*medium,
    I_2 = start*(1-s)*medium*(1-alpha2),
    A_2 = start*(1-s)*medium*(alpha2),
    R_2 = 0,
    
    S_3 = n*(1-s)*old - start*old*(1-s),
    E_3 = start*(1-s)*old,
    I_3 = start*(1-s)*old*(1-alpha3)*(1-s),
    A_3 = start*(1-s)*old*(alpha3)*(1-s),
    R_3 = 0,
    
    S_1Q = n*(s)*young - start_kids*young*(s),
    E_1Q = start_kids*(s)*young,
    I_1Q = start_kids*(s)*young*(alpha1),
    A_1Q = start_kids*(s)*young*(alpha1),
    R_1Q = 0,
    
    S_2Q = n*(s)*medium - start*medium*(s),
    E_2Q = start*(s)*medium, 
    I_2Q = start*(s)*medium*(1-alpha2),
    A_2Q = start*(s)*medium*(alpha2),
    R_2Q = 0,
    
    S_3Q = n*(s)*old - start*old*(s),
    E_3Q = start*(s)*old,
    I_3Q = start*(s)*old*(1-alpha3)*(s),
    A_3Q = start*(s)*old*(alpha3)*(s),
    R_3Q = 0,
    
    I_1_cum = 0,
    I_2_cum = 0,
    I_3_cum = 0,
    I_1Q_cum = 0,
    I_2Q_cum = 0,
    I_3Q_cum = 0,
    
    A_1_cum = 0,
    A_2_cum = 0,
    A_3_cum = 0,
    A_1Q_cum = 0,
    A_2Q_cum = 0,
    A_3Q_cum = 0,
    
    D_1_cum = 0,
    D_2_cum = 0,
    D_3_cum = 0,
    D_1Q_cum = 0,
    D_2Q_cum = 0,
    D_3Q_cum = 0
    
  )
  
  ############## RUN MODEL
  
  # very roughly estimated
  p = .05
  
  # run the model
  tic()
  test = run_model(model_strat, xstart = as.numeric(x), times = c(1:30), params, method = "lsodes")
  toc()
  
  # rename columns (a pain to do in tidyverse)
  names(test)[2:ncol(test)] = names(x)
  
  f = make_plots(test, params = params)
  #multiplot(f[[1]], f[[2]],
  #          f[[3]], f[[4]],
  #          f[[5]], f[[6]])

  multiplot(f[[2]], f[[3]], cols = 2)
  
############## RUN MODEL CHANGING HALFWAY THROUGH
  
  # run the model
  test = run_model(model_strat, xstart = as.numeric(x), times = c(1:15), params, method = "lsodes")
  names(test)[2:ncol(test)] = names(x)
  
  # cut contact matrix in half
  x2 = tail(test, n = 1)[-1]
  v11 = v11/2; v12 = v12/2; v13= v13/2; v21=v21/2; v22= v22/2; v23 = v23/2; v31 = v31/2; v32 = v32/2; v33 = v33/2
  test2 = run_model(model_strat, xstart = as.numeric(x2), times = c(1:15), params, method = "lsodes")
  names(test2)[2:ncol(test2)] = names(x)
  test2$time = c(16:30)
  
  out = bind_rows(test, test2)
  f = make_plots(out, params = params)
  multiplot(f[[2]], f[[3]], cols = 2)
  
  
  
  
