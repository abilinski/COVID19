library(rmarkdown)

# Rmarkdown Configuration Settings
 
# local level specific parameters
param_vec <- param_vec_reactive()
param_vec_int <- param_vec_int_reactive()

sim_time <- input$sim_time
days_out1 <- input$interventionInterval[1]
days_out3 <- input$interventionInterval[2]

det_table <- data.frame(
  time = 1:(input$sim_time),
  rdetecti = rep(input$rdetecti, input$sim_time),
  rdetecta = rep(input$rdetecta, input$sim_time))

det_table_int <- data.frame(
  time = 1:(input$sim_time),
  rdetecti = c(rep(input$rdetecti, input$interventionInterval[1]), 
    rep(input$rdetecti_int, (input$sim_time - input$interventionInterval[1]))),
  rdetecta = c(rep(input$rdetecta, input$interventionInterval[1]), 
    rep(input$rdetecta_int, (input$sim_time - input$interventionInterval[1]))))

hospitalized <- input$hospitalized
respirator <- input$respirator

observed_data_cases <- observed_data$cases
observed_data_hospitalizations <- observed_data$hospitalizations
observed_data_deaths <- observed_data$deaths

# Output File Path
output_file <- file.path(tempdir(), "/covid_app_report.pdf")

# Render the document
rmarkdown::render(
  # system.file gets the path to the rmarkdown from inside the package
  system.file("rmd/app_report/app_report.rmd", package = 'covid.epi'), 
  output_format = 'pdf_document',
  output_file = output_file
  )
