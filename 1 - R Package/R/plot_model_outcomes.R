


#' Plot Flows by Compartment
plot_flows_by_compartment <- function(out) {
  ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2) %>% summarize(value = sum(value)),
    aes(x = time, y = value, group = comp2, col = comp2)) + geom_line() + theme_minimal() +
  scale_color_discrete(name = "") +
  labs(x = "Time (days)", y = "", title = "Flows by compartment")
}

#' Plot Flows by Compartment for Interventions
plot_flows_by_compartment_int <- function(out) {
  ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2, int) %>% summarize(value = sum(value)),
               aes(x = time, y = value, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) + theme_minimal() +
      scale_color_discrete(name = "") +
      labs(x = "Time (days)", y = "", title = "Flows by compartment") + scale_linetype(name ="")
}


#' Compute Cases from Simulation Outcomes
compute_cases <- function(out) {
  out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3, comp3) %>%
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) %>%
    mutate(Total = sum(Infected),
           Total_obs = sum(Detected),
           Hospital = .17*Total,
           Ventilator = .05*Total)
}

#' Compute Cases from Simulation Outcomes - Intervention
compute_cases_intervention <- function(out) {
  out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3, comp3, int) %>%
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time, int) %>%
    mutate(Total = sum(Infected),
           Total_obs = sum(Detected),
           Hospital = .17*Total, ########this multipliers are different from the one used in no interventions
           Ventilator = .05*Total) %>% ungroup()
}


#' Plot Cumulative Cases by Age
plot_cumulative_cases_by_age <- function(out_cases) {

  ggplot(out_cases, aes(x = time, y = Infected, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") + scale_linetype(guide = F) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "",
                                                           title = "Cumulative cases by age")
}


#' Plot Cumulative Cases by Age - Intervention
plot_cumulative_cases_by_age_int <- function(out_cases) {
  ggplot(out_cases, aes(x = time, y = Infected, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "",
                                                             title = "Cumulative cases by age")
}

#' Plot Cases Needing Advanced Care
plot_cases_needing_advanced_care <- function(out_cases) {
  ggplot(out_cases %>% gather(var, value, Hospital, Ventilator),
              aes(x = time, y = value, group = var, col = var)) + geom_line() +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "",
                                                             title = "Cases needing advanced care")
}

#' Plot Cases Needing Advanced Care - Intervention
plot_cases_needing_advanced_care_int <- function(out_cases) {
  ggplot(out_cases %>% gather(var, value, Hospital, Ventilator),
              aes(x = time, y = value, group = interaction(var, int), col = var)) + geom_line(aes(lty = int)) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "",
                                                             title = "Cases needing advanced care")
}


#' Plot Ratio of New To Existing Cases
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


#' Plot Ratio of New To Exisitng Cases - Intervention
plot_ratio_of_new_to_existing_cases_int <- function(out) {

  out_Re = out %>% subset(select=-comp3) %>% filter(comp %in% c("UI","DI","I")) %>% mutate(comp=replace(comp,comp!="I", "I")) %>%
    group_by(time,cum,int) %>% summarize(val2=sum(value))%>% spread(cum, val2) %>% group_by(time,int) %>%
    summarize(existing_inf = sum(`FALSE`), new_inf = sum(`TRUE`), ratio = new_inf/existing_inf) %>% ungroup()

  ggplot(out_Re, aes(x = time, y = ratio)) + geom_line(aes(lty = int)) +
    theme_minimal() + scale_color_discrete(name = "") +
    labs(x = "Time (days)", y = "", title = "Ratio of new to existing cases")
}


#' Plot Diagnosed Cumulative Cases by Age
#' Note that these are "observed" meaning diagnosed
plot_diagnosed_cumulative_cases_by_age <- function(out_cases) {
  ggplot(out_cases, aes(x = time, y = Detected,
                          group = strat3, col = strat3)) + geom_line() +
      geom_line(aes(y = Total_obs), col = "black") +
      theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "",
        title = "Observed cumulative cases by age")
}


#' Plot Diagnosed Cumulative Cases by Age
#' Note that these are "observed" meaning diagnosed
plot_diagnosed_cumulative_cases_by_age_int <- function(out_cases) {
  ggplot(out_cases, aes(x = time, y = Detected,
                            group = interaction(strat3,int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total_obs, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Observed cumulative cases by age")
}

#' Plot Deaths by Age
plot_deaths_by_age <- function(out) {
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3) %>%
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2))

  ggplot(out_death, aes(x = time, y = val2, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative deaths by age")

}


#' Plot Deaths by Age - Intervention
plot_deaths_by_age_int <- function(out) {
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3, int) %>%
    summarize(val2 = sum(value)) %>% group_by(time,int) %>% mutate(Total = sum(val2)) %>% ungroup()
  ggplot(out_death, aes(x = time, y = val2, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
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


#' Plot cases by symptom status - Intervention
plot_cases_by_symptom_status_int <- function(out) {

  out_symp = out %>% filter(cum == T & comp!="D" & !is.na(strat3)) %>% group_by(time, comp2, int) %>%
    summarize(val2 = sum(value)) %>% group_by(time, int) %>% mutate(Total = sum(val2)) %>% ungroup()

  ggplot(out_symp, aes(x = time, y = val2, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
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


#' Plot Flows by Compartment Version 2 - Intervention
#'
#' Not sure what the difference between this and plot_flows_by_compartment is
#'
plot_flows_by_compartment2_int <- function(out) {
  ggplot(out %>% filter(cum ==F), aes(x = time, y = value, group = interaction(comp, int), col = comp2)) + geom_line(aes(lty = int)) +
    facet_wrap(.~strat, ncol = 2) + theme_minimal() + scale_color_discrete(name = "") +
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
}

#' Plot Fit to Observed Data
plot_fit_to_observed_data <- function(out,
  observed_data =
    load_SCC_time_series()) # read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T))
{
  ts = observed_data %>%  mutate(Total_obs = cumulative_cases) %>% rename(time = day)

  out_fit = bind_rows(out_cases %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed"))

  ggplot(out_fit, aes(x = time, y = Total_obs, group = id, col=id)) +
    geom_line() +
    geom_point(data = filter(out_fit, id = 'Observed')) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "",
                                                             title = "Calibration") +
    scale_linetype(name = "")
}


#' Plot Fit to Observed Data - Intervention 
#plot_fit_to_observed_data_int <- function(out_cases) {

#  # Check fit (won't include intervention, since we are only fitting 15 days data for now)
#  ts = read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), 
#    as.is = T)[6:20,] %>% # These rows are for March 1st - 15th# Set a reasonable range of p

#  mutate(time = 1:15, Total_obs = cum_cases, int = "Base")
#  #out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% group_by(int) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed")) %>% ungroup()

#  out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed")) 
#  #this was copied from yuhan's update, but this update was not markered as different from prvious commit, so may have been changed long ago


plot_fit_to_observed_data_int <- function(out_cases,
  observed_data = load_SCC_time_series() # read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T) 
  ) {

  # Check fit (won't include intervention, since we are only fitting 15 days data for now)
  # These rows are for March 1st - 15th# Set a reasonable range of p
  ts = observed_data %>% mutate(Total_obs = cumulative_cases, int = "Base") %>%
    rename(time = day)

  out_fit = bind_rows(out_cases %>% filter(time <= max(observed_data$day)+2) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed"))

  ggplot(out_fit, aes(x = time, y = Total_obs, group = interaction(int,id), col=id)) + geom_line(aes(lty = int)) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "",
                                                             title = "Comparison to Data") +
    scale_linetype(name = "")
}

#' Format Model Outcomes for Plotting
format_simulation_outcomes_for_plotting <- function(sim_outcomes) {
  sim_outcomes %>%
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
}


#' Format Model Outcomes for Plotting - Intervention
format_simulation_outcomes_for_plotting_int <- function(sim_outcomes, sim_outcomes_int) {

  out_base = sim_outcomes %>%
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

  out_int = sim_outcomes_int %>%
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
  return(out)
}

#' Plot hospital bed needs over time along with capacity
#'
#' @export
#'
#' @examples
#'  params <- load_parameters()
#'  det_table <- load_detection_table()
#'  sim_out <- run_param_vec(params = params, days_out1 = 30, days_out2 = NULL,
#'    model_type = run_basic, params2 = NULL, det_table = det_table)
#'  out_formatted <- format_simulation_outcomes_for_plotting(sim_out)
#'  hosp_time_plots(out_formatted, beds=100)
#'
hosp_time_plots <- function(out_formatted,beds,num_scens=6,hosp_time=10,hosp_pc=.05){
  #input output from run_vecs
  out_formatted= out_formatted %>% filter(comp=="I") #data.frame(out_formatted)
  out_formatted$hosp_beds = out_formatted$value
  out_formatted$hosp_beds[out_formatted$time>hosp_time]=diff(out_formatted$value,num_scens*hosp_time)
  #calculate hospital beds
  out_formatted$hosp_beds = hosp_pc*out_formatted$hosp_beds
  #get mean, min and max cases
  hosp_cases<-out_formatted %>% group_by(time) %>% summarize(mean = mean(hosp_beds),min = min(hosp_beds), max=max(hosp_beds))
  # get color values for  max > beds, mean > beds, min >beds
  hosp_cases$Capacity <-"Not Exceeded"
  hosp_cases$Capacity[hosp_cases$max>beds]<-"Possibly Exceeded"
  hosp_cases$Capacity[hosp_cases$mean>beds]<-"Likely Exceeded"
  hosp_cases$Capacity[hosp_cases$min>beds]<-"Definitely Exceeded"
  hosp_cases$Capacity<-as.factor(hosp_cases$Capacity)
  pal <- c("red", "orange", "forestgreen", "yellow"  )
  #hosp_cases = cases_means*hosp_pc
  print(ggplot(hosp_cases,aes(time,mean, fill=Capacity))+geom_bar(stat="identity")+geom_errorbar(aes(ymin=min, ymax=max))+
          theme_minimal() + scale_fill_manual(values = pal) + geom_hline(yintercept =beds)  +
          labs(x = "Time (days)", y = "") +
          theme(strip.text = element_text(size=12, face="bold")) +
          geom_text(aes(7,beds,label = "Available\n Hospital Beds", vjust = -.1)) +
          ggtitle("Projected Hospital Bed Requirements"))
  ggsave(paste0("hosp_fig.pdf"),width=6.5, height=3.5)

}

#' calculate which day hospital beds exceeds count
#'
#' calculate difference with and without SD
#' takes as inputs the outputs of run_vecs for scenarios with and without social distancing
#'
#' @export
#'
#' @examples
#'
#'  # I wasn't sure how to get this working ...
#'
#'  # I tried different ways to call hosp_days_plots with outcomes from the simulation
#'  # model and I couldn't quite figure out what I should do
#'
#'  # Below I tried to make the data frame look like what I anticipated the function wants
#'  # based on the variables it accesses, but I still couldn't get it to work.
#'
#'   params <- load_parameters()
#'   params2 <- load_parameters(8) # SD 50% scenario
#'
#'   det_table <- load_detection_table()
#'
#'   sim_out <- run_param_vec(params = params, days_out1 = 30, days_out2 = NULL,
#'     model_type = run_basic, params2 = NULL, det_table = det_table)
#'
#'   sim_out_int <- run_param_vec(params = params, days_out1 = 15, days_out2 = 30,
#'     model_type = run_int, params2 = params2, det_table = det_table)
#'
#'   sim_out <- format_simulation_outcomes_for_plotting(sim_out)
#'   sim_out_int <- format_simulation_outcomes_for_plotting(sim_out_int)
#'
#'   sim_out <- compute_cases(sim_out)
#'   sim_out_int <- compute_cases(sim_out_int)
#'
#'   sim_out %<>% group_by(time) %>% summarize(
#'     Detected = sum(Detected),
#'     Infected = sum(Infected),
#'     value = sum(Total))
#'
#'   sim_out_int %<>% group_by(time) %>% summarize(
#'     Detected = sum(Detected),
#'     Infected = sum(Infected),
#'     value = sum(Total))
#'
#'   sim_out$scenario <- 'base case'
#'   sim_out_int$scenario <- 'social distancing 50%'
#'
#'   # still throws an error
#'   hosp_days_plots(no_sd_mat = sim_out, sd_mat = sim_out_int, beds = 100)
#'
hosp_days_plots <-function(no_sd_mat,sd_mat,beds,num_scens=6,hosp_time=10,hosp_pc=.05, output_file){
  #turn time series into datatables
  setDT(no_sd_mat)
  setDT(sd_mat)
  #Calculate cumulative cases with 10 day lag to account for discharges
  no_sd_mat$hosp_beds = no_sd_mat$value
  no_sd_mat$hosp_beds[no_sd_mat$time>hosp_time]=diff(no_sd_mat$value,num_scens*hosp_time)
  no_sd_mat$hosp_beds = hosp_pc*no_sd_mat$hosp_beds
  sd_mat$hosp_beds = sd_mat$value
  sd_mat$hosp_beds[sd_mat$time>hosp_time]=diff(sd_mat$value,num_scens*hosp_time)
  sd_mat$hosp_beds = hosp_pc*sd_mat$hosp_beds
  #get first day that beds go over capacity for no sd and sd

  no_sd_mat<-no_sd_mat[no_sd_mat$hosp_beds >beds,.SD[which.min(time)], by = scenario]
  sd_mat<-sd_mat[sd_mat$hosp_beds >beds,.SD[which.min(time)], by = scenario]
  #take difference in days between sd and no sd for scenarios where matrix exists for sd
  hosp_days <-sd_mat$time -no_sd_mat[no_sd_mat$scenario %in% sd_mat$scenario,'time']
  plot <-
    boxplot(hosp_days,main="Extra Days before Exceeding Hospital Bed Capacity", ylab ="Days")

  if (! is.missing(output_file)) {
    pdf(paste0(output_file),width=6.5, height=3.5)
      plot
    dev.off()
  }

  return(plot)
}

############## POST-PROCESSING-------------------
#' Make Plots
#'
#' @export
make_plots = function(test, params){

  # formatting
  out = format_simulation_outcomes_for_plotting(test)

  # make graphs of output over time
  out_age = out %>% group_by(comp, cum) %>% summarize(sum(value))

  # Flows by compartment (SEAIR)
  a = plot_flows_by_compartment(out)

  # Compute cases from simulation outcomes
  out_cases <- compute_cases(out)

  # Cases by age
  b <- plot_cumulative_cases_by_age(out_cases)
  b2 <- plot_cases_needing_advanced_care(out_cases)

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
  h <- plot_fit_to_observed_data(out_cases)

  return(list(a,b,c,d,e,f,g,h,b2))
}

#' Make Plots Comparing Base Case and Intervention
#'
#' @export
#'
#' @examples
#'
#' param_vec_int <- param_vec <- load_parameters()
#' param_vec_int[['s']] <- .5
#'
#' det_table <- load_detection_table()
#'
#' test = run_param_vec(
#'   params = param_vec, params2=NULL, days_out1 = 30,
#'   days_out2 = NULL, days_out3 = 30, model_type = run_basic, det_table = det_table)
#'
#' test_int = run_param_vec(params = param_vec, params2 = param_vec_int,
#'                          days_out1 = 10, days_out2 = 30, days_out3 = 30, model_type = run_int, det_table = det_table)
#'
#' plots <- make_plots_int(test = test, params = param_vec, test_int = test_int,
#'   params_int = param_vec_int, observed_data = filter_states_data("MA"))
#'
make_plots_int = function(test, params, test_int, params_int, observed_data){
  # formatting
  out <- format_simulation_outcomes_for_plotting_int(test, test_int)

  # make graphs of output over time
  out_age = out %>% group_by(comp, cum) %>% summarize(sum(value)) %>% ungroup()

  # Flows by compartment (SEAIR)
  a = plot_flows_by_compartment_int(out)

  # Cases by age
  out_cases <- compute_cases_intervention(out)
  b <- plot_cumulative_cases_by_age_int(out_cases)
  b2 <- plot_cases_needing_advanced_care_int(out_cases)

  # Ratio of New cases to existing cases
  c <- plot_ratio_of_new_to_existing_cases_int(out)

  # Observed cases by age
  d <- plot_diagnosed_cumulative_cases_by_age_int(out_cases)

  # Deaths by age
  e <- plot_deaths_by_age_int(out)

  # Cases by symptoms
  f <- plot_cases_by_symptom_status_int(out)

  # Flows by compartment (devided by strata and quarentine)
  g <- plot_flows_by_compartment2_int(out)

  # Check fit 
  h <- plot_fit_to_observed_data_int(out_cases, observed_data)
  
  return(list(a,b,c,d,e,f,g,h,b2))
}

