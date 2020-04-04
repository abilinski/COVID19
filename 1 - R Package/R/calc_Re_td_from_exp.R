#' This function takes the input time series, S(t), and the parameter vector as inputs and calculates the
#' corresponding R0 and doubling time
#' 
#' Calculate R0 and td from exponential curve
#' @export
calc_Re_td_from_exp <- function(params, time_series=NULL, S=0.9) {
  if (is.null(time_series)) {
    time_series <- read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T)
  }
  fit <- lm(log(time_series$cumulative_cases) ~ time_series$day)
  a <- exp(fit$coefficients[[1]])
  b <- exp(fit$coefficients[[2]])
  lamda <- b - 1
  td <- log(2)/lamda
  Re <- S*(lamda+params$delta)*(lamda+params$gamma)/(params$delta*params$gamma)
  return(c(td, Re))
}