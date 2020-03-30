
#' @importFrom XML readHTMLTable
load_population_sizes <- function() {

  data_source <- system.file(
    "population_size_estimates/List of states and territories of the United States by population - Wikipedia.html",
    package='covid.epi')

  extracted_tables <- XML::readHTMLTable(data_source)

  # first table has the population sizes by country we want
  df <- extracted_tables[[1]]

  df %<>% rename(
    state = V3,
    popsize = V4) %>% 
  select(state, popsize) %>% 
  mutate(popsize = as.numeric(gsub(",", "", popsize)))

  return(df)
}


