#' Load parameters table
#' 
#' Load the parameters.csv file as a table.
#' 
load_parameters_table <- function() {

  # we don't need readr telling us that it parsed the CSV with the following types every 
  # time this function is called: 
  #
  #> Parsed with column specification:
  #> cols(
  #>   .default = col_double(),
  #>   Scenario = col_character()
  #> )
  #> See spec(...) for full column specifications.
  # 
  # we prefer to use readr::read_csv to utils::read.csv because read.csv
  # defaults to stringsAsFactors behavior which can be annoying.

  suppressMessages( 
    parameters_table <- readr::read_csv(system.file('parameters.csv', package='covid.epi'))
  )
  
  return(parameters_table)
}

#' Load Parameters Vector from Parameters Table
#'
#' @param scenario An integer level selecting one of the rows of the
#' parameters csv. 1 corresponds to the base case. Others correspond to
#' intervention scenarios.
#'
load_parameters <- function (scenario = 1) {

  parameters_table <- load_parameters_table()

  # grab the "scenario-th" row of the parameters table. 
  # skip the first column, since these indicate the scenario names, 
  # e.g. base case, "Josh", "School closing", etc.
  return(parameters_table[scenario, 2:ncol(parameters_table)])

}
