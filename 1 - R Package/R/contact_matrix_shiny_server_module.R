#' Contact Matrix Shiny Server Module
#'
#' This Shiny module watches for when the user updates one of the 
#' contact matrix parameters and applies the appropriate updates to the other 
#" parameters. 
#" 
#" Let SD be an abbreviation for the socially distanced compartments" contact matrices and 
#" NSD be an abbreviation for the not-socially-distanced compartments" contact matrices.
#" 
#" Additionally, _int refers to the contact matrices for an intervention (as opposed to 
#" without referring to the base case contact matrices).
#" 
#" The cascade of updates performed by this shiny module include updating: 
#"   - NSD <- SD * e 
#"   - Updates to SD cascade to SD_int, but SD_int can also be updated independently 
#"     of SD by modifying it *after* setting SD.
#"   - NSD_int <- NSD
#"
#" @seealso contact_matrix_ui_for_base_case contact_matrix_ui_for_intervention

contact_matrix_server_module <- function(input, output, session) {
  observeEvent(input$v11, {
      updateNumericInput(session, "v11Q", value = input$v11 * input$e)
      updateNumericInput(session, "v11Q_int", value = input$v11 * input$e)
    })
  observeEvent(input$v12, {
      updateNumericInput(session, "v12Q", value = input$v12 * input$e)
      updateNumericInput(session, "v12Q_int", value = input$v12 * input$e)
    })
  observeEvent(input$v13, {
      updateNumericInput(session, "v13Q", value = input$v13 * input$e)
      updateNumericInput(session, "v13Q_int", value = input$v13 * input$e)
    })
  observeEvent(input$v21, {
      updateNumericInput(session, "v21Q", value = input$v21 * input$e)
      updateNumericInput(session, "v21Q_int", value = input$v21 * input$e)
    })
  observeEvent(input$v22, {
      updateNumericInput(session, "v22Q", value = input$v22 * input$e)
      updateNumericInput(session, "v22Q_int", value = input$v22 * input$e)
    })
  observeEvent(input$v23, {
      updateNumericInput(session, "v23Q", value = input$v23 * input$e)
      updateNumericInput(session, "v23Q_int", value = input$v23 * input$e)
    })
  observeEvent(input$v31, {
      updateNumericInput(session, "v31Q", value = input$v31 * input$e)
      updateNumericInput(session, "v31Q_int", value = input$v31 * input$e)
    })
  observeEvent(input$v32, {
      updateNumericInput(session, "v32Q", value = input$v32 * input$e)
      updateNumericInput(session, "v32Q_int", value = input$v32 * input$e)
    })
  observeEvent(input$v33, {
      updateNumericInput(session, "v33Q", value = input$v33 * input$e)
      updateNumericInput(session, "v33Q_int", value = input$v33 * input$e)
    })

    observeEvent(input$e, {
      updateNumericInput(session, "v11Q", value = input$v11 * input$e)
      updateNumericInput(session, "v12Q", value = input$v12 * input$e)
      updateNumericInput(session, "v13Q", value = input$v13 * input$e)
      updateNumericInput(session, "v21Q", value = input$v21 * input$e)
      updateNumericInput(session, "v22Q", value = input$v22 * input$e)
      updateNumericInput(session, "v23Q", value = input$v23 * input$e)
      updateNumericInput(session, "v31Q", value = input$v31 * input$e)
      updateNumericInput(session, "v32Q", value = input$v32 * input$e)
      updateNumericInput(session, "v33Q", value = input$v33 * input$e)
      updateNumericInput(session, "v11Q_int", value = input$v11_int * input$e)
      updateNumericInput(session, "v12Q_int", value = input$v12_int * input$e)
      updateNumericInput(session, "v13Q_int", value = input$v13_int * input$e)
      updateNumericInput(session, "v21Q_int", value = input$v21_int * input$e)
      updateNumericInput(session, "v22Q_int", value = input$v22_int * input$e)
      updateNumericInput(session, "v23Q_int", value = input$v23_int * input$e)
      updateNumericInput(session, "v31Q_int", value = input$v31_int * input$e)
      updateNumericInput(session, "v32Q_int", value = input$v32_int * input$e)
      updateNumericInput(session, "v33Q_int", value = input$v33_int * input$e)
    })
}
