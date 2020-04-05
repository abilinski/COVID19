
load_SCC_time_series <- function() {
  suppressMessages(
    readr::read_csv(system.file("time_series/time_series_SCC.csv", package='covid.epi'))
  )
}

load_states_data <- function() {
  suppressMessages(
    df <- readr::read_csv(system.file("states-daily.csv", package='covid.epi'))
    )
  df$date %<>% lubridate::ymd()
  df %<>% arrange(date)

  return(df)
}

filter_states_data <- function(state) {
  df <- load_states_data() %>% filter(state == !! state)

  # add a day column
  df$day <- df$date %>% as.factor() %>% as.integer()

  df %<>% rename(cumulative_cases = positive)

  df %<>% select(day, cumulative_cases)

  df
}


#' Read In Detection Rates
#' 
#' @export
load_detection_rates <- function() {
  det_table <- read.csv(system.file("detection_input.csv", package="covid.epi"), as.is = T)
  return(det_table)
}

#' Make Detection Table
#'
#' Helper to make a detection table
#' 
#' Currently assumes constants over time
#' 
#' @export
#' 
make_detection_table <- function(t, rdetecti, rdetecta) {
  data.frame(time = 1:t, rdetecti = rdetecti, rdetecta = rdetecta)
}
