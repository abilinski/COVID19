
#' Generate Shiny UI Layout 
#' 
#' @export

generate_ui <- function() { 

  df <- load_parameters_table()

  old_vec = df[1,]
  param_vec = df[1,]
  param_names_base = c(colnames(df)[1], colnames(df)[11:34])[!c(colnames(df)[1], colnames(df)[11:34]) %in% c("epsilon","e_ratio")]
  param_names_int = c(colnames(df)[1], unlist(lapply(param_names_base[-1],function(x) paste(x,"int", sep="_"))))

  # Define UI for application
  ui <- fluidPage(

      # Application title
      titlePanel("Interactive Projection Tool for COVID-19 Interventions"),
      shinyjs::useShinyjs(),
      tabsetPanel(
          tabPanel("Fits", plotOutput("fit"), p("*Currently only fit to data for 15 days")),
          tabPanel("Comp flows", plotOutput("comp_flow")),
          tabPanel("Cumulative cases", 
            column(6, 
              plotOutput('cumulative_infections_by_age')
              ), 
            column(6, 
              plotOutput('cumulative_diagnosed_by_age')
              )
            ), 
          tabPanel("Deaths & New case ratio", 
            column(6,
              plotOutput('deaths_by_age')
              ),
            column(6,
              plotOutput('effective_reproductive_number')
              )
            ),
          tabPanel("Advanced care & Symptoms", 
            column(6,
              plotOutput('cases_needing_advanced_care')
              ),
            column(6,
              plotOutput('cumulative_cases_by_symptoms')
              )
          )
          
      ),
      actionButton("reset_inputs", "Reset All Parameters"),
      
      hr(),
      column(6,
             h3("Base case parameters"),
             wellPanel(
          fluidRow(#class = "text-center",
              column(4,
                     numericInput("sim_time", label="simulation time (days)", value=30),
                     #numericInput("int_time", label="intervention starts at", value=15),
                     #numericInput("R0", label="R0", value=2.2),
                     numericInput("td", label="Doubling Time", value=2.5),
                     numericInput("delta", label=HTML("&delta;: 1/(dur of incub)"), value=0.2),
                     numericInput("gamma", label=HTML("&gamma;: 1/(dur of infectious)"), value=0.2),
                     numericInput("obs", label="obs cases at day1", value=100),
                     numericInput("n", label="n: total population", value=1938000),
                     #maybe put matrix here
                     sliderInput("rdetecti", label = "Symptomatic detection rate", 
                       min = 0, max = 1, value = 0.1),
                     sliderInput("rdetecta", label = "Asymptomatic detection rate", 
                       min = 0, max = 1, value = 0.01)
              ),
              column(4,
                     sliderInput("s", label = "s: Frc socially distanced", min = 0, 
                                 max = 1, value = 0),
                     sliderInput("e", label = "e: Social distance multiplier", min = 0, 
                                 max = 1, value = 0),
                    # sliderInput("p", label = "p: Pr(transmission/contact)", min = 0.01, 
                     #            max = 0.1, value = 0.05),
                     sliderInput("kappa", label = HTML("&kappa;: rel. Pr(trans) for asymp"), min = 0, 
                                 max = 1, value = 0.375),
                     sliderInput("m1", label = "m1: mortality yng", min = 0, 
                                 max = 1, value = 0),
                     sliderInput("m2", label = "m2: mortality med", min = 0, 
                                 max = 1, value = 0.005),
                     sliderInput("m3", label = "m3: mortality old", min = 0, 
                                 max = 1, value = 0.1),
                     sliderInput("k_report", label = "k_report: rel rep rate for yng", min = 0, 
                                 max = 1, value = 1)
              ),
              column(4,
                     sliderInput("alpha1", label = HTML("&alpha;1: Pr(asymp) yng"), min = 0, 
                                 max = 1, value = 0.75),
                     sliderInput("alpha2", label = HTML("&alpha;2: Pr(asymp) med"), min = 0, 
                                 max = 1, value = 0.3),
                     sliderInput("alpha3", label = HTML("&alpha;3: Pr(asymp) old"), min = 0, 
                                 max = 1, value = 0.3),
                     sliderInput("c", label = "c: Reporting rate", min = 0, 
                                 max = 1, value = 0.13),
                     sliderInput("young", label = "Frc youth", min = 0, 
                                 max = 1, value = 0.24),
                     sliderInput("medium", label = "Frc adults", min = 0, 
                                 max = 1, value = 0.6),
                     disabled(sliderInput("old", label = "Frc older adults", min = 0, 
                                 max = 1, value = 0.15)),
                     sliderInput("k_inf", label = "k_inf: rel infectiousness for yng", min = 0, 
                                 max = 1, value = 1),
                     sliderInput("k_susp", label = "k_susp: rel. suscep for yng", min = 0, 
                                 max = 1, value = 1)
                     #p("*young+medium+old <= 1"),
                  ),
              downloadButton("download", "Download parameters")
                    
              )
             )
      ),
      
      column(6,
             h3("Intervention parameters"),
             wellPanel(
                 fluidRow(#class = "text-center",
                     column(4,
                            #umericInput("sim_time", label="simulation time (days)", value=30),
                            numericInput("int_time", label="intervention starts at", value=15),
                            numericInput("R0_int", label="R0", value=2.2),
                            numericInput("delta_int", label=HTML("&delta;: 1/(dur of incub)"), value=0.2),
                            numericInput("gamma_int", label=HTML("&gamma;: 1/(dur of infectious)"), value=0.2),
                            numericInput("obs_int", label="obs cases at day1", value=100),
                            numericInput("n_int", label="n: total population", value=1938000)
                            #maybe put matrix here
                     ),
                     column(4,
                            sliderInput("s_int", label = "s: Frc socially distanced", min = 0, 
                                        max = 1, value = 0),
                            sliderInput("e_int", label = "e: Social distance multiplier", min = 0, 
                                        max = 1, value = 0),
                           # sliderInput("p_int", label = "p: Pr(transmission/contact)", min = 0.01, 
                            #            max = 0.1, value = 0.05),
                            sliderInput("kappa_int", label = HTML("&kappa;: rel. Pr(trans) for asymp"), min = 0, 
                                        max = 1, value = 0.375),
                            sliderInput("m1_int", label = "m1: mortality yng", min = 0, 
                                        max = 1, value = 0),
                            sliderInput("m2_int", label = "m2: mortality med", min = 0, 
                                        max = 1, value = 0.005),
                            sliderInput("m3_int", label = "m3: mortality old", min = 0, 
                                        max = 1, value = 0.1),
                            sliderInput("k_report_int", label = "k_report: rel rep rate for yng", min = 0, 
                                        max = 1, value = 1)
                     ),
                     column(4,
                            sliderInput("alpha1_int", label = HTML("&alpha;1: Pr(asymp) yng"), min = 0, 
                                        max = 1, value = 0.75),
                            sliderInput("alpha2_int", label = HTML("&alpha;2: Pr(asymp) med"), min = 0, 
                                        max = 1, value = 0.3),
                            sliderInput("alpha3_int", label = HTML("&alpha;3: Pr(asymp) old"), min = 0, 
                                        max = 1, value = 0.3),
                            sliderInput("c_int", label = "c: Reporting rate", min = 0, 
                                        max = 1, value = 0.13),
                            sliderInput("young_int", label = "Frc youth", min = 0, 
                                        max = 1, value = 0.24),
                            sliderInput("medium_int", label = "Frc adults", min = 0, 
                                        max = 1, value = 0.6),
                            disabled(sliderInput("old_int", label = "Frc older adults", min = 0, 
                                        max = 1, value = 0.15)),
                            sliderInput("k_inf_int", label = "k_inf: rel infectiousness for yng", min = 0, 
                                        max = 1, value = 1),
                            sliderInput("k_susp_int", label = "k_susp: rel. suscep for yng", min = 0, 
                                        max = 1, value = 1)
                            #p("*young+medium+old <= 1"),
                     ),
                     downloadButton("download_int", "Download parameters")
                     
                 )
             )
      )
  )

  return(ui)
}
