
load_SCC_time_series <- function() {
  readr::read_csv(system.file("time_series/time_series_SCC.csv", package='covid.epi'))
}
