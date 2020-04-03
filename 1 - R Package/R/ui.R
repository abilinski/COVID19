
#' Generate Shiny UI Layout 
#' 
#' @export

generate_ui <- function() { 

  # Define UI for application
  ui <- fluidPage(

    # Application title
    titlePanel("Interactive Projection Tool for COVID-19 Interventions"),

    # shinyjs is a package that provides some handy javascript functions that
    # can be called from in the shiny ui and server. one example we use is
    # disabled inputs which are presented as greyed out and not editable, but
    # still dynamically depend on other enabled user inputs (e.g. R0 depends on
    # doubling time)

    shinyjs::useShinyjs(),
    tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),

    # Plot Outcomes

    tabsetPanel(
      selected = "Cumulative cases",
      tabPanel("Data Input",
        column(12,
          column(5, 
          h4("Specify data to compare model outcomes against"),
          selectInput(inputId = "cases_hospitalizations_or_deaths", label = "What data are you entering?", choices = c("Cases", "Hospitalizations", "Deaths")),
          rHandsontableOutput('observedData')
          ),
        column(7,
          h4("Where can I find data to use here?"),
          p("Check out sources such as:"),
          tags$a("covidtracker.com", href='https://covidtracker.com'),
          br(),
          tags$a("Johns Hopkins CSSE Repository", href='https://github.com/CSSEGISandData/COVID-19'),
          br(),
          tags$a("New York Times Data Repository", href='https://github.com/nytimes/covid-19-data'),
          )
        )
        ),
      tabPanel("Comparison to Data", 
        plotOutput("fit"),
        selectInput("comparisonDataPlotChoice", "Select a Measure for Comparing Model Outcomes to Data", choices = c("Cases", "Hospitalizations", "Deaths")),
        selectInput("comparisonDataPlotCumulative", "How do you want the plot formatted?", choices = c("Cumulative", "Daily Counts"))
        ),

      tabPanel("Comp flows", plotOutput("comp_flow")),

      tabPanel("Cumulative cases", 
        # Side by side layout for cumulative infections and diagnosed cases by age
        column(6, 
          plotOutput('cumulative_infections_by_age')
          ), 
        column(6, 
          plotOutput('cumulative_diagnosed_by_age')
        )
        ), 
      tabPanel("Deaths & New case ratio", 
        # side by side layout for deaths by age and new cases / existing cases
        column(6,
          plotOutput('deaths_by_age')
          ),
        column(6,
          plotOutput('effective_reproductive_number')
        )
        ),
      tabPanel("Advanced care & Symptoms", 
        # side by side layout for those needing advanced care and cumulative
        # cases by symptom status
        column(6,
          plotOutput('cases_needing_advanced_care')
          ),
        column(6,
          plotOutput('cumulative_cases_by_symptoms')
        )
      )

      ),

    hr(),
    column(6,
      h3("Base case parameters"),
      wellPanel(
        fluidRow(
        tabsetPanel(
          tabPanel(
            title = "transmission parameters",
            tags$div(
              style = 'padding-top:12pt',
              column(4,
                numericInput("sim_time", label="simulation time (days)", value=180),
                # we want to show r0 and td from calculation later
                disabled(numericInput("R0", label="R0", value=1.0)),
                disabled(numericInput("p", label="p: Pr(transmission/contact)", value=0.05)),
                numericInput("td", label="Doubling Time", value=2.5),
                numericInput("delta", label=HTML("&delta;: 1/(dur of incub)"), value=0.2),
                numericInput("gamma", label=HTML("&gamma;: 1/(dur of infectious)"), value=0.2),
                numericInput("obs", label="obs cases at day1", value=100),
                numericInput("n", label="n: total population", value=1938000),
                sliderInput("rdetecti", label = "Symptomatic detection rate", 
                  min = 0, max = 1, value = 0.1),
                sliderInput("rdetecta", label = "Asymptomatic detection rate", 
                  min = 0, max = 1, value = 0.01)
                ),
              column(4,
                sliderInput("s", label = "s: Frc socially distanced", min = 0.01, 
                  max = .999, value = .01, step=0.01),
                sliderInput("e", label = "e: Social distance multiplier", min = 0, 
                  max = 1, value = 0),
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
                ),
              downloadButton("download", "Download parameters"),

              # reset_inputs triggers an observeEvent in the server which takes 
              # all of the user inputs and resets them to their default values
              actionButton("reset_inputs", "Reset All Parameters"),
              )
            ),
          tabPanel(
            title = "contact matrix",
            tags$div(
              style = 'padding-top:12pt',
              contact_matrix_ui_for_base_case(load_parameters()) 
            )
          )
          )
        )
      )
      ),

    column(6,
      h3("Intervention parameters"),
      wellPanel(
        fluidRow(
          tabsetPanel(
            tabPanel(
              title = "transmission parameters",
              tags$div(
                style = 'padding-top:12pt',
              column(4,
                uiOutput('interventionInterval'),
                disabled(numericInput("R0_int", label="R0", value=1.0)),
                disabled(numericInput("p_int", label="p: Pr(transmission/contact)", value=0.05)),
                disabled(numericInput("td_int", label="Doubling Time", value=2.5)),
                numericInput("delta_int", label=HTML("&delta;: 1/(dur of incub)"), value=0.2),
                numericInput("gamma_int", label=HTML("&gamma;: 1/(dur of infectious)"), value=0.2),
                disabled(numericInput("obs_int", label="obs cases at day1", value=100)),
                disabled(numericInput("n_int", label="n: total population", value=1938000)),
                sliderInput("rdetecti_int", label = "Symptomatic detection rate", 
                  min = 0, max = 1, value = 0.1),
                sliderInput("rdetecta_int", label = "Asymptomatic detection rate", 
                  min = 0, max = 1, value = 0.01)
                ),
              column(4,
                sliderInput("s_int", label = "s: Frc socially distanced", min = 0.01, 
                  max = .999, value = 0.1, step=0.01),
                disabled(sliderInput("e_int", label = "e: Social distance multiplier", min = 0, 
                  max = 1, value = 0)),
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
                disabled(sliderInput("alpha1_int", label = HTML("&alpha;1: Pr(asymp) yng"), min = 0, 
                  max = 1, value = 0.75)),
                disabled(sliderInput("alpha2_int", label = HTML("&alpha;2: Pr(asymp) med"), min = 0, 
                  max = 1, value = 0.3)),
                  disabled(sliderInput("alpha3_int", label = HTML("&alpha;3: Pr(asymp) old"), min = 0, 
                  max = 1, value = 0.3)),
                disabled(sliderInput("young_int", label = "Frc youth", min = 0, 
                                     max = 1, value = 0.24)),
                disabled(sliderInput("medium_int", label = "Frc adults", min = 0, 
                                     max = 1, value = 0.6)),
                disabled(sliderInput("old_int", label = "Frc older adults", min = 0, 
                                     max = 1, value = 0.15)),
                sliderInput("k_inf_int", label = "k_inf: rel infectiousness for yng", min = 0, 
                  max = 1, value = 1),
                sliderInput("k_susp_int", label = "k_susp: rel. suscep for yng", min = 0, 
                  max = 1, value = 1)
                )
              ),
              downloadButton("download_int", "Download parameters")
            ),
          tabPanel(
            title = "contact matrix",
              tags$div(
                style = 'padding-top:12pt',
                contact_matrix_ui_for_intervention(load_parameters())
              )
          )
          )
        )
      )
    )
  )

  return(ui)
}
