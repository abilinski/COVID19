# This function takes model outputs and paramters of base and intervention scenarios to make comparison plots
source("source/model_3strat_18_mar_2020.R")
make_plots_int = function(test, params, test_int, params_int){
  
  k_report = params$k_report
  c = params$c
  k_report_int = params_int$k_report
  c_int = params_int$c
  
  # formatting
  out_base = test %>%
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
  
  out_int = test_int %>%
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
  
  out = bind_rows(out_base %>% mutate(int = "Base"), out_int %>% mutate(int = "Intervention"))
  
  
  # make graphs of output over time
  out_age = out %>% group_by(comp, cum) %>% summarize(sum(value))
  
  # Flows by compartment
  a = ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2, int) %>% summarize(value = sum(value)), 
             aes(x = time, y = value, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) + theme_minimal() + 
    scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment") + scale_linetype(name ="")
  
  # Cases by age
  out_cases = out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3, int) %>% 
    summarize(val2 = sum(value)) %>% group_by(time, int) %>% mutate(Total = sum(val2),
                                                                    val_obs = ifelse(strat3=="<20", k_report*c*val2, c*val2),
                                                                    Total_obs = sum(val_obs),
                                                                    Hospital = .17*.13*Total,
                                                                    Ventilator = .05*Total)
  b = ggplot(out_cases, aes(x = time, y = val2, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cumulative cases by age")
  b2 = ggplot(out_cases %>% gather(var, value, Hospital, Ventilator), 
              aes(x = time, y = value, group = interaction(var, int), col = var)) + geom_line(aes(lty = int)) + 
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cases needing advanced care")
  
  # Effective R
  out_Re = out %>% filter(comp=="I") %>% spread(cum, value) %>% group_by(time, comp2, int) %>%
    summarize(existing_inf = sum(`FALSE`), new_inf = sum(`TRUE`), ratio = new_inf/existing_inf)
  c = ggplot(out_Re, aes(x = time, y = ratio)) + geom_line(aes(lty = int)) + 
    theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Ratio of new to existing cases")
  
  
  # Observed cases by age
  d = ggplot(out_cases, aes(x = time, y = val_obs,
                            group = interaction(strat3,int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total_obs, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Observed cumulative cases by age")
  
  
  # Deaths by age
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3, int) %>% 
    summarize(val2 = sum(value)) %>% group_by(time,int) %>% mutate(Total = sum(val2))
  e = ggplot(out_death, aes(x = time, y = val2, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative deaths by age")
  
  
  # Cases by symptoms
  out_symp = out %>% filter(cum == T & comp!="D" & !is.na(strat3)) %>% group_by(time, comp2, int) %>% 
    summarize(val2 = sum(value)) %>% group_by(time, int) %>% mutate(Total = sum(val2))
  f = ggplot(out_symp, aes(x = time, y = val2, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative cases by symptoms")
  
  # Flows by compartment
  g = ggplot(out %>% filter(cum ==F), aes(x = time, y = value, group = interaction(comp, int), col = comp2)) + geom_line(aes(lty = int)) + 
    facet_wrap(.~strat, ncol = 2) + theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
  
  # Check fit (won't include intervention, since we are only fitting 15 days data for now)
  ts = read.csv("source/time_series_SCC.csv", as.is = T)[6:20,] %>% # These rows are for March 1st - 15th# Set a reasonable range of p
    mutate(time = 1:15, Total_obs = cum_cases, int = "Base")
  out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% group_by(int) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed"))
  h = ggplot(out_fit, aes(x = time, y = Total_obs, group = interaction(int,id), col=id)) + geom_line(aes(lty = int)) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Calibration") + 
    scale_linetype(name = "")
  
  return(list(a,b,c,d,e,f,g,h,b2))
}