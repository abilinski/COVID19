
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
      selected = "Cases by age",
      tabPanel("Data Input",
        column(12,
          column(5, 
          h4("Specify data to compare model outcomes against"),
          selectInput(inputId = "cases_hospitalizations_or_deaths", label = "What data are you entering?", choices = c("Cases", "Hospitalizations", "Deaths")),
          rHandsontableOutput('observedData')
          ),
        column(7,
          br(),
          h4("Instructions"),
          p("Enter data in the data table to compare with model outcomes."),
          p("Daily observed data entered will be automatically cumulated and available for visualization as daily and cumulative outcomes in the Comparison to Data tab."),
          p("Feel free to tune the model parameters to reflect what you believe best represents the trend in the data."),
          br(),
          br(),
          h4("Where can I find data to use here?"),
          p("Check out sources such as:"),
          tags$a("covidtracking.com", href='https://covidtracking.com'),
          br(),
          tags$a("Johns Hopkins CSSE Repository", href='https://github.com/CSSEGISandData/COVID-19'),
          br(),
          tags$a("New York Times Data Repository", href='https://github.com/nytimes/covid-19-data'),
          br(),
          tags$a("U.S. CDC - COVID-19 Cases in U.S.", href='https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/cases-in-us.html')
          )
        )
        ),
      tabPanel("Comparison to Data", 
        h4("Enter observed data in the Data Input page and compare model outcomes against it here."),
        selectInput("comparisonDataPlotChoice", "Select the Comparison Data Set", choices = c("Cases", "Hospitalizations", "Deaths")),
        plotOutput("fit"),
        selectInput("comparisonDataPlotCumulative", "Outcomes format:", choices = c("Cumulative", "Daily Rates"))
        ),

      tabPanel("Compartment flows", 
        plotOutput("comp_flow", height='auto'),
        checkboxGroupInput(
          inputId = 'compartmentFlowSelectedComps', 
          label = 'Select Compartments Shown',
          choices = c('Susceptible', 'Exposed', 'Asymptomatic', 'Symptomatic', 'Recovered'),
          selected = c('Susceptible', 'Exposed', 'Asymptomatic', 'Symptomatic', 'Recovered')),
        downloadButton('compartments_download',
          label = 'Download Compartment Flows Data')
        ),

      tabPanel("Cases by age", 
        # Side by side layout for cumulative infections and diagnosed cases by age
        column(6, 
          plotOutput('cumulative_infections_by_age')
          ), 
        column(6, 
          plotOutput('cumulative_diagnosed_by_age')
        ),
        selectInput(
          inputId = 'cases_cumulative', 
          label = "Outcomes format:",
          choices = c("Cumulative", "Daily Rates"),
          ),
        downloadButton('cases_download',
          label = 'Download Cases Data')
        ), 
      tabPanel("Deaths & New case ratio", 
        # side by side layout for deaths by age and new cases / existing cases
        column(6,
          plotOutput('deaths_by_age')
          ),
        column(6,
          plotOutput('effective_reproductive_number')
        ),
        selectInput(
          inputId = 'deaths_cumulative', 
          label = "Outcomes format for deaths:",
          choices = c("Cumulative", "Daily Rates"),
          ),
        downloadButton('deaths_download',
          label = 'Download Deaths Data'),
        downloadButton('ncr_download',
          label = 'Download New Case Ratio Data')
        ),
      tabPanel("Advanced care & Symptoms", 
        # side by side layout for those needing advanced care and cumulative
        # cases by symptom status
        column(6,
          plotOutput('cases_needing_advanced_care')
          ),
        column(6,
          plotOutput('cumulative_cases_by_symptoms')
        ),
        selectInput(
          inputId = 'adv_care_and_symptoms_cumulative', 
          label = "Outcomes format:",
          choices = c("Cumulative", "Daily Rates"),
          ),
        downloadButton('advanced_care_download',
          label = 'Download Advanced Cases Data'),
        downloadButton('symptoms_download',
          label = 'Download Symptoms Data')
      ),
      tabPanel("Documentation",
        fluidPage(
          includeHTML(system.file("documentation/app/app_documentation.html", package='covid.epi')),
          downloadButton("download_doc", "Download our Technical Documentation")
          ),
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
                  numericInput("p", label="p: Pr(transmission per contact)", value=0.05),
                  disabled(numericInput("td", label="Disease Free Equilibrium Doubling Time", value=2.5)),
                  numericInput("delta", label=HTML("&delta;: 1/(dur of incub)"), value=0.2),
                  numericInput("gamma", label=HTML("&gamma;: 1/(dur of infectious)"), value=0.2),
                  numericInput("obs", label="ongoing infections at day 1", value=100),
                  numericInput("n", label="n: total population", value=1938000),
                  sliderInput("rdetecti", label = "Symptomatic detection rate", 
                    min = 0, max = 1, value = 0.1),
                  sliderInput("rdetecta", label = "Asymptomatic detection rate", 
                    min = 0, max = 1, value = 0.01),
                  sliderInput("k_det_c", label = HTML("k_det_c: rel. rate of transmission for detected"), min = 0, 
                    max = 1, value = 0.5),
                  downloadButton("download", "Download parameters"),
                  br(),
                  br(),

                  # reset_inputs triggers an observeEvent in the server which takes 
                  # all of the user inputs and resets them to their default values
                  actionButton("reset_inputs", "Reset All Parameters")
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
                    max = 1, value = 1),
                  sliderInput("hospitalized", label = "hosp: hospitalization rate for infected", min = 0, max = 1, value = 0.17),
                  sliderInput("respirator", label = "resp: rate of needing respirator for infected", min = 0, max = 1, value = 0.05)
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
                  )
              )
              ),
            tabPanel(
              title = "contact matrix",
              tags$div(
                style = 'padding-top:12pt',
                contact_matrix_ui_for_base_case(load_parameters()) 
              )
              ),
            tabPanel(
              title = "compute doubling time",
              tags$div(
                style = 'padding-top:12pt',
                uiOutput('doublingTimeInterval'),
                htmlOutput('doublingTime'),
                # numericOutput('doublingTime'),
                br(),
                h5('* Please note that the estimated doubling time and Re are only sensible during exponentail growth period.')
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
                disabled(numericInput("p_int", label="p: Pr(transmission per contact)", value=0.05)),
                disabled(numericInput("td_int", label="Disease Free Equilibrium Doubling Time", value=2.5)),
                numericInput("delta_int", label=HTML("&delta;: 1/(dur of incub)"), value=0.2),
                numericInput("gamma_int", label=HTML("&gamma;: 1/(dur of infectious)"), value=0.2),
                disabled(numericInput("obs_int", label="ongoing infections at day 1", value=100)),
                disabled(numericInput("n_int", label="n: total population", value=1938000)),
                sliderInput("rdetecti_int", label = "Symptomatic detection rate", 
                  min = 0, max = 1, value = 0.1),
                sliderInput("rdetecta_int", label = "Asymptomatic detection rate", 
                  min = 0, max = 1, value = 0.01),
                sliderInput("k_det_c_int", label = HTML("k_det_c: rel. rate of transmission for detected"), min = 0, 
                  max = 1, value = 0.5),
                downloadButton("download_int", "Download parameters")
                ),
              column(4,
                sliderInput("s_int", label = "s: Frc socially distanced", min = 0.01, 
                  max = .999, value = 0.01, step=0.01),
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
                  max = 1, value = 1),
                disabled(sliderInput("hospitalized_int", label = "hosp: hospitalization rate for infected", min = 0, max = 1, value = 0.17)),
                disabled(sliderInput("respirator_int", label = "resp: rate of needing respirator", min = 0, max = 1, value = 0.05))
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
                )
              ),
            tabPanel(
              title = "contact matrix",
              tags$div(
                style = 'padding-top:12pt',
                contact_matrix_ui_for_intervention(load_parameters())
              )
              ),
            tabPanel(
              title = "compute doubling time",
              tags$div(
                style = 'padding-top:12pt',
                uiOutput('doublingTimeIntervalInt'),
                htmlOutput('doublingTimeInt'),
                br(),
                h5('* Please note that the estimated doubling time and Re are only sensible during exponentail growth period.')
              )
            )
          )
        )
      )
    )#,
    # column(2,
    #   downloadButton("downloadReport", 'Download All Plots'),
    #   br(),
    #   br()
    # )
  )

  return(ui)
}
