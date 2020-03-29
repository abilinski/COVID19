


#' Plot Flows by Compartment
plot_flows_by_compartment <- function(out) {
  ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2) %>% summarize(value = sum(value)), 
    aes(x = time, y = value, group = comp2, col = comp2)) + geom_line() + theme_minimal() + 
  scale_color_discrete(name = "") + 
  labs(x = "Time (days)", y = "", title = "Flows by compartment")
}


#' Compute Cases from Simulation Outcomes
compute_cases <- function(out) {
  out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3, comp3) %>% 
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) %>% 
    mutate(Total = sum(Infected),
           Total_obs = sum(Detected),
           Hospital = .1*Total,
           Ventilator = .05*Total)
}


#' Plot Cumulative Cases by Age and Cases Needing Advanced Care
plot_cumulative_cases_by_age <- function(out_cases) {
  
  plot1 <- ggplot(out_cases, aes(x = time, y = Infected, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") + scale_linetype(guide = F) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                           title = "Cumulative cases by age")

  plot2 <- ggplot(out_cases %>% gather(var, value, Hospital, Ventilator), 
              aes(x = time, y = value, group = var, col = var)) + geom_line() + 
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cases needing advanced care")

  return(c(plot1, plot2))
}


#' Plot Ratio of New To Exisitng Cases
plot_ratio_of_new_to_existing_cases <- function(out) {

  out_Re = out %>% subset(select=-comp3) %>% filter(comp %in% c("UI","DI","I", "UA", "DA", "A")) %>% 
    mutate(comp=replace(comp,comp %in% c("UI","DI","UA","DA"), "I")) %>% 
    group_by(time,cum) %>% summarize(val2=sum(value)) %>% spread(cum, val2) %>% 
    rename(existing_inf = "FALSE", cum_inf = "TRUE") %>% as.data.frame() %>% 
    mutate(new_inf=ifelse(time==1, cum_inf, cum_inf-lag(cum_inf)), ratio = new_inf/existing_inf)

  ggplot(out_Re, aes(x = time, y = ratio)) + geom_line() + 
           theme_minimal() + scale_color_discrete(name = "") + 
           labs(x = "Time (days)", y = "", title = "Ratio of new to existing cases")
}


#' Plot Cumulative Cases by Age
#' Note that these are "observed" meaning diagnosed
plot_diagnosed_cumulative_cases_by_age <- function() {
  ggplot(out_cases, aes(x = time, y = Detected,
                          group = strat3, col = strat3)) + geom_line() +
      geom_line(aes(y = Total_obs), col = "black") +
      theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
        title = "Observed cumulative cases by age")
}

#' Plot Deaths by Age
plot_deaths_by_age <- function(out) {
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3) %>% 
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2))

  ggplot(out_death, aes(x = time, y = val2, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative deaths by age")
  
}

#' Plot cases by symptom status
plot_cases_by_symptom_status <- function(out) {
  out_symp = out %>% filter(cum == T & comp!="D" & !is.na(strat3)) %>% group_by(time, comp2) %>% 
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2))

  ggplot(out_symp, aes(x = time, y = val2, group = comp2, col = comp2)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative cases by symptoms")
}

#' Plot Flows by Compartment Version 2
#' 
#' Not sure what the difference between this and plot_flows_by_compartment is
#' 
plot_flows_by_compartment2 <- function(out) {
  ggplot(out %>% filter(cum ==F), aes(x = time, y = value, group = comp, col = comp2)) + geom_line() + 
    facet_wrap(.~strat, ncol = 2) + theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
}

#' Plot Fit to Observed Data
plot_fit_to_observed_data <- function(out) {
  ts = read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T)[6:20,] %>% # These rows are for March 1st - 15th# Set a reasonable range of p
    mutate(time = 1:15, Total_obs = cum_cases)
  
  out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed"))
  
  ggplot(out_fit, aes(x = time, y = Total_obs, group = id, col=id)) + geom_line() +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Calibration") + 
    scale_linetype(name = "")
}

############## POST-PROCESSING-------------------
#' Make Plots
#' 
#' @export
make_plots = function(test, params){

  # formatting
  out = test %>%
    gather(var, value, -time) %>% { suppressWarnings(separate(., var, into = c("comp", "strat", "cum"), sep = "_")) } %>%
    mutate(cum = ifelse(is.na(cum), F, T),
           
           # reformat compartments
           comp2 = ifelse(comp %in% c("UA","DA","A"), "Asymptomatic", "Symptomatic"),
           comp2 = ifelse(comp=="E", "Exposed", comp2),
           comp2 = ifelse(comp=="R", "Recovered", comp2),
           comp2 = ifelse(comp=="S", "Susceptible", comp2),
           comp2 = factor(comp2, levels = c("Susceptible", "Exposed", "Asymptomatic",
                                            "Symptomatic", "Recovered")),
           
           # compartments with U/D
           comp3 = ifelse(comp %in% c("DI","DA"), "Detected", "Undetected"),
           comp3 = ifelse(comp %in% c("I","A"), "Infected", comp3),
           comp3 = ifelse(comp=="E", "Exposed", comp3),
           comp3 = ifelse(comp=="R", "Recovered", comp3),
           comp3 = ifelse(comp=="S", "Susceptible", comp3),
           comp3 = factor(comp3, levels = c("Susceptible", "Exposed", 
                                            "Detected", "Undetected", "Infected",
                                            "Recovered")),
           
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

  # Flows by compartment (SEAIR)
  a = plot_flows_by_compartment(out)

  # Compute cases from simulation outcomes
  out_cases <- compute_cases(out)

  # Cases by age
  cases_and_those_needing_advanced_care_plots <- plot_cumulative_cases_by_age(out_cases)
  b <- cases_and_those_needing_advanced_care_plots[[1]]
  b2 <- cases_and_those_needing_advanced_care_plots[[2]]
  
  #  Ratio of Daily New Cases to Existing Cases
  c <- plot_ratio_of_new_to_existing_cases(out)
  
  # Observed cases by age
  d <- plot_diagnosed_cumulative_cases_by_age(out_cases)
  
  # Deaths by age
  e <- plot_deaths_by_age(out)
  
  # Cases by symptoms
  f <- plot_cases_by_symptom_status(out)
  
  # Flows by compartment (devided by strata and quarentine)
  g <- plot_flows_by_compartment2(out)
  
  # Check fit
  h <- plot_fit_to_observed_data(out)
  
  return(list(a,b,c,d,e,f,g,h,b2))
}

#' Make Plots Comparing Base Case and Intervention
#' 
#' @export
make_plots_int = function(test, params, test_int, params_int){
  # formatting
  out_base = test %>%
    gather(var, value, -time) %>% { suppressWarnings(separate(., var, into = c("comp", "strat", "cum"), sep = "_")) } %>%
    mutate(cum = ifelse(is.na(cum), F, T),
           
           # reformat compartments
           comp2 = ifelse(comp %in% c("UA","DA","A"), "Asymptomatic", "Symptomatic"),
           comp2 = ifelse(comp=="E", "Exposed", comp2),
           comp2 = ifelse(comp=="R", "Recovered", comp2),
           comp2 = ifelse(comp=="S", "Susceptible", comp2),
           comp2 = factor(comp2, levels = c("Susceptible", "Exposed", "Asymptomatic",
                                            "Symptomatic", "Recovered")),
           
           # compartments with U/D
           comp3 = ifelse(comp %in% c("DI","DA"), "Detected", "Undetected"),
           comp3 = ifelse(comp %in% c("I","A"), "Infected", comp3),
           comp3 = ifelse(comp=="E", "Exposed", comp3),
           comp3 = ifelse(comp=="R", "Recovered", comp3),
           comp3 = ifelse(comp=="S", "Susceptible", comp3),
           comp3 = factor(comp3, levels = c("Susceptible", "Exposed", 
                                            "Detected", "Undetected", "Infected",
                                            "Recovered")),
           
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
    gather(var, value, -time) %>% { suppressWarnings(separate(., var, into = c("comp", "strat", "cum"), sep = "_")) } %>%
    mutate(cum = ifelse(is.na(cum), F, T),
           
           # reformat compartments
           comp2 = ifelse(comp %in% c("UA","DA","A"), "Asymptomatic", "Symptomatic"),
           comp2 = ifelse(comp=="E", "Exposed", comp2),
           comp2 = ifelse(comp=="R", "Recovered", comp2),
           comp2 = ifelse(comp=="S", "Susceptible", comp2),
           comp2 = factor(comp2, levels = c("Susceptible", "Exposed", "Asymptomatic",
                                            "Symptomatic", "Recovered")),
           
           # compartments with U/D
           comp3 = ifelse(comp %in% c("DI","DA"), "Detected", "Undetected"),
           comp3 = ifelse(comp %in% c("I","A"), "Infected", comp3),
           comp3 = ifelse(comp=="E", "Exposed", comp3),
           comp3 = ifelse(comp=="R", "Recovered", comp3),
           comp3 = ifelse(comp=="S", "Susceptible", comp3),
           comp3 = factor(comp3, levels = c("Susceptible", "Exposed", 
                                            "Detected", "Undetected", "Infected",
                                            "Recovered")),
           
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
  out_age = out %>% group_by(comp, cum) %>% summarize(sum(value)) %>% ungroup()
  
  # Flows by compartment (SEAIR)
  a = ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2, int) %>% summarize(value = sum(value)), 
             aes(x = time, y = value, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) + theme_minimal() + 
    scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment") + scale_linetype(name ="")
  
  # Cases by age
  out_cases = out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3, comp3, int) %>% 
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time, int) %>% 
    mutate(Total = sum(Infected),
           Total_obs = sum(Detected),
           Hospital = .17*.13*Total, ########this multipliers are different from the one used in no interventions
           Ventilator = .05*Total) %>% ungroup()
  
  b = ggplot(out_cases, aes(x = time, y = Infected, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cumulative cases by age")
  b2 = ggplot(out_cases %>% gather(var, value, Hospital, Ventilator), 
              aes(x = time, y = value, group = interaction(var, int), col = var)) + geom_line(aes(lty = int)) + 
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cases needing advanced care")
  
  # Effective R
  out_Re = out %>% subset(select=-comp3) %>% filter(comp %in% c("UI","DI","I")) %>% mutate(comp=replace(comp,comp!="I", "I")) %>% 
    group_by(time,cum,int) %>% summarize(val2=sum(value))%>% spread(cum, val2) %>% group_by(time,int) %>%
    summarize(existing_inf = sum(`FALSE`), new_inf = sum(`TRUE`), ratio = new_inf/existing_inf) %>% ungroup()
  c = ggplot(out_Re, aes(x = time, y = ratio)) + geom_line(aes(lty = int)) + 
    theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Ratio of new to existing cases")
  
  
  # Observed cases by age
  d = ggplot(out_cases, aes(x = time, y = Detected,
                            group = interaction(strat3,int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total_obs, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Observed cumulative cases by age")
  
  
  # Deaths by age
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3, int) %>% 
    summarize(val2 = sum(value)) %>% group_by(time,int) %>% mutate(Total = sum(val2)) %>% ungroup()
  e = ggplot(out_death, aes(x = time, y = val2, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative deaths by age")
  
  
  # Cases by symptoms
  out_symp = out %>% filter(cum == T & comp!="D" & !is.na(strat3)) %>% group_by(time, comp2, int) %>% 
    summarize(val2 = sum(value)) %>% group_by(time, int) %>% mutate(Total = sum(val2)) %>% ungroup()
  f = ggplot(out_symp, aes(x = time, y = val2, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative cases by symptoms")
  
  # Flows by compartment (devided by strata and quarentine)
  g = ggplot(out %>% filter(cum ==F), aes(x = time, y = value, group = interaction(comp, int), col = comp2)) + geom_line(aes(lty = int)) + 
    facet_wrap(.~strat, ncol = 2) + theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
  
  # Check fit (won't include intervention, since we are only fitting 15 days data for now)
  ts = read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T)[6:20,] %>% # These rows are for March 1st - 15th# Set a reasonable range of p
    mutate(time = 1:15, Total_obs = cum_cases, int = "Base")
  #out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% group_by(int) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed")) %>% ungroup()
  out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed")) #this was copied from yuhan's update, but this update was not markered as different from prvious commit, so may have been changed long ago
  h = ggplot(out_fit, aes(x = time, y = Total_obs, group = interaction(int,id), col=id)) + geom_line(aes(lty = int)) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Calibration") + 
    scale_linetype(name = "")
  
  return(list(a,b,c,d,e,f,g,h,b2))
}

