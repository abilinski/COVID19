
contact_matrix_ui_for_base_case <- function(param_vec) {
  fluidRow(
    column(12,
      h4("Contact Rates (not-socially-distanced)"),
      column(4,
        numericInput("v11", "[0,20) [0,20)", param_vec[['v11']], min=0),
        numericInput("v12", "[0,20) [20,65)", param_vec[['v12']], min=0),
        numericInput("v13", "[0,20) 65+", param_vec[['v13']], min=0)
        ),
      column(4,
        numericInput("v21", "[20,65) [0,20)", param_vec[['v21']], min=0),
        numericInput("v22", "[20,65) [20,65)", param_vec[['v22']], min=0),
        numericInput("v23", "[20,65) 65+", param_vec[['v23']], min=0)
        ),
      column(4,
        numericInput("v31", "65+ [0,20)", param_vec[['v31']], min=0),
        numericInput("v32", "65+ [20,65)", param_vec[['v32']], min=0),
        numericInput("v33", "65+ 65+", param_vec[['v33']], min=0)
      )
      ),
    column(12,
      h4("Contact Rates (socially-distanced)"),
      column(4,
        disabled(numericInput("v11Q", "[0,20) [0,20)", 0, min=0)),
        disabled(numericInput("v12Q", "[0,20) [20,65)", 0, min=0)),
        disabled(numericInput("v13Q", "[0,20) 65+", 0, min=0))
        ),
      column(4,
        disabled(numericInput("v21Q", "[20,65) [0,20)", 0, min=0)),
        disabled(numericInput("v22Q", "[20,65) [20,65)", 0, min=0)),
        disabled(numericInput("v23Q", "[20,65) 65+", 0, min=0))
        ),
      column(4,
        disabled(numericInput("v31Q", "65+ [0,20)", 0, min=0)),
        disabled(numericInput("v32Q", "65+ [20,65)", 0, min=0)),
        disabled(numericInput("v33Q", "65+ 65+", 0, min=0))
      )
    )
  )

}

contact_matrix_ui_for_intervention <- function(param_vec) {
  fluidRow(
            column(12,
              h4("Contact Rates (not-socially-distanced)"),
              column(4,
                numericInput("v11_int", "[0,20) [0,20)", param_vec[['v11']], min=0),
                numericInput("v12_int", "[0,20) [20,65)", param_vec[['v12']], min=0),
                numericInput("v13_int", "[0,20) 65+", param_vec[['v13']], min=0)
                ),
              column(4,
                numericInput("v21_int", "[20,65) [0,20)", param_vec[['v21']], min=0),
                numericInput("v22_int", "[20,65) [20,65)", param_vec[['v22']], min=0),
                numericInput("v23_int", "[20,65) 65+", param_vec[['v23']], min=0)
                ),
              column(4,
                numericInput("v31_int", "65+ [0,20)", param_vec[['v31']], min=0),
                numericInput("v32_int", "65+ [20,65)", param_vec[['v32']], min=0),
                numericInput("v33_int", "65+ 65+", param_vec[['v33']], min=0)
              )
              ),
            column(12,
              h4("Contact Rates (socially-distanced)"),
              column(4,
                disabled(numericInput("v11Q_int", "[0,20) [0,20)", 0, min=0)),
                disabled(numericInput("v12Q_int", "[0,20) [20,65)", 0, min=0)),
                disabled(numericInput("v13Q_int", "[0,20) 65+", 0, min=0))
                ),
              column(4,
                disabled(numericInput("v21Q_int", "[20,65) [0,20)", 0, min=0)),
                disabled(numericInput("v22Q_int", "[20,65) [20,65)", 0, min=0)),
                disabled(numericInput("v23Q_int", "[20,65) 65+", 0, min=0))
                ),
              column(4,
                disabled(numericInput("v31Q_int", "65+ [0,20)", 0, min=0)),
                disabled(numericInput("v32Q_int", "65+ [20,65)", 0, min=0)),
                disabled(numericInput("v33Q_int", "65+ 65+", 0, min=0))
              )
            )
    )
}
