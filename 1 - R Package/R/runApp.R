
runApp <- function() { 
  shiny::shinyApp(ui = generate_ui(), server = server)
}
