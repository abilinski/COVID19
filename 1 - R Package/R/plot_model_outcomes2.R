

# This R file holds the functions which the plotting functions make use of but
# which are not themselves plotting functions!
#

#' Format Model Outcomes for Plotting
#' 
#' Format Simulation Outcomes in a "tidy" (Long & Lean) fashion (as opposed to 
#' wide, with compartments in columns). 
#' 
#' @examples
#' # setup
#' param_vec <- load_parameters()
#' det_table <- make_detection_table(t = 180, rdetecti = .1, rdetecta = 0)
#' 
#' # run simulation
#' sim_out <- run_param_vec(param_vec, days_out1 = 180, days_out3 = 180, det_table = det_table)
#' 
#' # format data for plotting
#' sim_out_formatted <- format_simulation_outcomes_for_plotting(sim_out)
#' 
#' # plot! 
#' plot_flows_by_compartment(sim_out_formatted)
#' 
format_simulation_outcomes_for_plotting <- function(sim_outcomes) {

  sim_outcomes %>%
    # the first step is to 'gather' the columns into a variable which contains the original 
    # column name (which is the compartment name, here)
    gather(var, value, -time) %>% 

    # then we split out that var column (with compartment names) into "comp" "strat" and 
    # "cum" (for cumulative). 
    #
    # the suppressWarnings is used here to suppress the separate command from telling us 
    # that some of the compartment names don't have a _cum attached to them and therefore 
    # its inserting NA values for those columns.  
    # 
    { suppressWarnings(separate(., var, into = c("comp", "strat", "cum"), sep = "_")) } %>%

    mutate(

      # We recode the NA values from separate into TRUE and FALSE indicating whether
      # this row/record indicates a cumulative outcome.
      cum = ifelse(is.na(cum), F, T),

      # We provide a more user-friendly names (rather than S, E, ...) we write 
      # Susceptible, Exposed, ... in the comp2 column

      comp2 = ifelse(comp %in% c("UA","DA","A"), "Asymptomatic", "Symptomatic"),
      comp2 = ifelse(comp=="E", "Exposed", comp2),
      comp2 = ifelse(comp=="R", "Recovered", comp2),
      comp2 = ifelse(comp=="S", "Susceptible", comp2),
      comp2 = ifelse(comp=="D", "Dead", comp2),
      comp2 = factor(comp2, levels = c("Susceptible", "Exposed", "Asymptomatic",
          "Symptomatic", "Recovered", "Dead")),

      # Provide comp3 compartments with U/D with user-friendly names

      comp3 = ifelse(comp %in% c("DI","DA"), "Detected", "Undetected"),
      comp3 = ifelse(comp %in% c("I","A"), "Infected", comp3),
      comp3 = ifelse(comp=="E", "Exposed", comp3),
      comp3 = ifelse(comp=="R", "Recovered", comp3),
      comp3 = ifelse(comp=="S", "Susceptible", comp3),
      comp3 = ifelse(comp=="D", "Dead", comp3),
      comp3 = factor(comp3, levels = c("Susceptible", "Exposed",
          "Detected", "Undetected", "Infected",
          "Recovered", "Dead")),

      # Provide strat2 with user friendly names indicating the age group and whether 
      # or not they are in a socially-distanced compartment

      strat2 = ifelse(strat=="1", "<20", "<20 (Socially Distanced)"),
      strat2 = ifelse(strat=="2", "21-65", strat2),
      strat2 = ifelse(strat=="2Q", "21-65 (Socially Distanced)", strat2),
      strat2 = ifelse(strat=="3", ">65", strat2),
      strat2 = ifelse(strat=="3Q", ">65 (Socially Distanced)", strat2),
      strat2 = factor(strat2, 
        levels = c(
          "<20", "<20 (Socially Distanced)",
          "21-65", "21-65 (Socially Distanced)",
          ">65", ">65 (Socially Distanced)")),

      # Provide strat3 indicating their age group without social distancing status

      strat3 = factor(sub(" \\(Socially Distanced\\)", "", strat2), levels = c("<20", "21-65", ">65"))
    )
}


#' Format Model Outcomes for Plotting - Intervention
format_simulation_outcomes_for_plotting_int <- function(sim_outcomes, sim_outcomes_int) {

  # Reformat the base case and interventions for plotting
  out_base <- format_simulation_outcomes_for_plotting(sim_outcomes)
  out_int <- format_simulation_outcomes_for_plotting(sim_outcomes_int)

  # Stitch together along with int = "Base" vs. "Intervention" column
  out = bind_rows(out_base %>% mutate(int = "Base"), out_int %>% mutate(int = "Intervention"))

  return(out)
}


#' Add Deaths Compartment
#' 
#' @examples
#' # setup
#' param_vec <- load_parameters()
#' det_table <- make_detection_table(180, .1, 0)
#' 
#' param_vec_int <- param_vec
#' param_vec_int['s'] <- .5
#' 
#' # run simulations
#' sim_out <- run_param_vec(params = param_vec, days_out1 = 180, det_table=det_table)
#' # run intervention scenario
#' sim_out_int <- run_param_vec(params = param_vec, params2 = param_vec_int, 
#'    days_out1 = 30, days_out2 = 180, days_out3 = 180, det_table=det_table,
#'    model_type = run_int)
#' 
#' # reformat for plotting
#' sim_outcomes_formatted <- format_simulation_outcomes_for_plotting_int(sim_out, sim_out_int)
#' 
#' # add daily deaths outcome
#' sim_outcomes_formatted %<>% add_daily_deaths_int
#' 
#' # plot!
#' plot_flows_by_compartment_strata_int(sim_outcomes_formatted)
add_daily_deaths_int <- function(sim_outcomes_formatted) {

  sim_outcomes_with_deaths <- 
    sim_outcomes_formatted %>% 

      # first we filter for the cumulative deaths outcome
      filter(comp == 'D', cum == 'TRUE') %>% 
      group_by(comp, strat, int) %>% 
      arrange(time) %>% 
      mutate(
        cum = FALSE,
        value = value - lag(value))

      bind_rows(
        sim_outcomes_formatted,
        sim_outcomes_with_deaths)
}
