#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#' Our model penalty function is calculated as the sum of squared errors for the 
#' case series data provided
model_loss_function <- function(params, observed_data) {

  # setup for model simulation
  det_table <- data.frame(
    time = 1:nrow(observed_data),
    rdetecti = rep(params$rdetecti, nrow(observed_data)),
    rdetecta = rep(params$rdetecta, nrow(observed_data)))

  # run model
  model_simulation <- run_param_vec(params = params, params2 = NULL, days_out1 = ceiling(nrow(observed_data)*2),
    days_out2 = nrow(observed_data), model_type = run_basic, det_table = det_table) 

  # get cumulative diagnosed cases from model simulation
  
  # cumulative diagnosed infections column names
  # this code gives all the combinations of DI_1_cum, DI_2Q_cum, etc
  diagnosed_cumulative_colnames <- 
    apply(expand.grid("DI_", 1:3, c("", "Q"), "_cum"), 1, paste0, collapse='')

  # get diagnosed cumulative infections
  diagnosed_cumulative_cases <- rowSums(model_simulation[,diagnosed_cumulative_colnames])

  # reformat as data frame
  diagnosed_cumulative_cases <- 
    data.frame(day = 1:length(diagnosed_cumulative_cases), 
      model_diagnosed_cumulative_cases = diagnosed_cumulative_cases) 

  # rename for clarity
  observed_data %<>% rename(observed_cumulative_cases = cumulative_cases)

  # merge
  merge_df <- merge(observed_data, diagnosed_cumulative_cases, by = 'day')

  sse <- sum(
    # squared error
    (merge_df$observed_cumulative_cases - merge_df$model_diagnosed_cumulative_cases)^2
    )

  return(sse)
}

#' Make a Loss Function
make_loss_function <- function(params, observed_data) {
  loss_function_for_p <- function(optim_vec, params) { 
    params$td <- optim_vec[[1]]
    params$obs <- optim_vec[[2]]
    model_loss_function(params, observed_data)
  }
}

#' Fit the Model
#' 
#' @examples
#' 
#'   params <- load_parameters()
#'   observed_data <- filter_states_data('MA')
#'   fit_model(params, observed_data)
#'   params$td <- 1 
#' 
#' 
fit_model <- function(params, observed_data) { 

  loss_function <- make_loss_function(params, observed_data)

  variables <- c(td = 2.5, obs = 100)

  optim(par = variables, fn = loss_function, method = 'Nelder-Mead',
    params=params)
}


#' Calibration scripts
#' 
#' @export
calibrate <- function(p_cand = seq(.001, .15, by = 0.005)) { 

  n = 1938000

  # these match US proportions
  young = .24
  medium = .6
  old = .15

  ############## CALIBRATION (CUMULATIVE CASES)
  # Observed data (Santa Clara data. Cumulative number of cases)
  ts = read.csv(system.file("time_series/time_series_SCC.csv",
      package="covid.epi"), as.is = T)[6:20,] # These rows are for March 1st - 15th # Set a reasonable range of p

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

}
