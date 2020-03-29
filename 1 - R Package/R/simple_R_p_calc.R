# fit exponential model with time series data to calculate R and transmission p
exp_calc <- function(Tc=7.5, C=10, T=5) {
  #Tc: mean generation interval, mean duration between time of infection of a secondary infectee and the time of infection of its primary infector
  #C: mean contact (calculated from contact matrix)
  #T: infectiou period
  #data from 2/25/20 to 3/15/20: it was announced on 2/27/20 that people who do not have travel/contact histry can be tested, so not taking data from too early
  time_series <- read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T)
  fit <- lm(log(time_series$cum_cases) ~ time_series$time)
  # plot(time_series$time, time_series$cum_cases)
  # lines(time_series$time, exp(fitted(fit)))
  #coefficients of exponential model
  a <- exp(fit$coefficients[[1]])
  b <- exp(fit$coefficients[[2]])
  # rate of exponential growth, per capita change in number of new cases per unit of time
  r <- b - 1 
  # effective reproductive number
  R <- 1 + r * Tc
  # Transmission probability
  p <- R/(C*T) 
  return(data.frame(R=R, trans_p=p))
}
