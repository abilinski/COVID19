
#' Run the Interactive Projection Tool for COVID-19 Interventions Application
#' @export
runApp <- function() { 
  shiny::shinyApp(ui = generate_ui(), server = server)
}
