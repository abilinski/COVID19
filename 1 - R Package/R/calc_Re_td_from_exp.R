calc_Re_td_from_exp <- function(params, time_series=NA, S=0.9) {
  if (is.na(time_series)) {
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