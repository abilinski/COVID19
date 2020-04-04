#' Define server logic required to run the model and display the results
#' 
#' This shiny server allows users to interact with a deterministic
#' compartmental model of covid-19 transmission fit to observed case data. 
#' 
#' Parameters are exposed so that users may edit the base case and additionally
#' define an intervention starting at a given time with different parameters.
#' In particular, users can define interventions that simulate the effects of
#' social distancing measures among others. 
#' 
#' Learn more about Shiny here: https://shiny.rstudio.com/
#' 
#' To run this application, after installing/loading this package, call the
#' covid.epi::runApp() function.
#' 
#' Two critical technologies we make use of here (outside of the underlying model, 
#' specifically in our shiny app) are shinyjs and shiny modules.
#' 
#' Read more about them here: 
#' https://github.com/daattali/shinyjs
#' https://shiny.rstudio.com/articles/modules.html
#' 
#' 
#' Future directions in progress include: 
#' 
#'   - Adding documentation [started on branch shiny_documentation_page]
#' 
#'   - Improve Calibration to User-Uploaded Case Series
#' 
#'   - More Comprehensive User Downloads: 
#'     - Parameters Downloads
#'     - Download an Rmarkdown Generated Report 
#'     - Downloadable Version of In-App Report Generated
#' 
#'   - A "Run Model" button will prevent the model from lagging
#'     when the user updates a bunch of parameters really quickly,
#'     when they likely wouldn't have wanted the model to run 
#'     in between each parameter update anyway. 
#'
#'   - Fix issue where when e = 1, s = 0 or s = 1  have different simulation 
#'     outcomes from when s is in (0,1).  We would expect e = 1 implies 
#'     socially distanced contact matrix == not socially distanced contact matrix,
#'     so varying s should have no effect on simulation outcomes.
#' 
#'   - Show the users imputed parameters, like R0 and p are determined
#'     based off the doubling time parameter td
#' 
#'   - Parameter Validation, making sure frc young + medium + old == 1 [done!]
#' 
#'   - Someday we should think about caching plots: 
#'     https://shiny.rstudio.com/articles/plot-caching.html 
#' 
#' @seealso generate_ui runApp 
#' 
#' @export
server <- function(input, output, session) {

  # Get the default parameter vector for the model
  default_param_vec = load_parameters() 

  # render the intervention time period interval as a slider 
  # between 0 and the total sim time
  output$interventionInterval <- renderUI({
    sliderInput(
      inputId = 'interventionInterval', 
      label = 'Intervention Time Period',
      min = 0,
      max = input$sim_time,
      value = c(30, input$sim_time))
  })
 
  # Observed data reactive values list
  observed_data <- reactiveValues(
    cases = data.frame(day = 1:30, daily_cases = 0, cumulative_cases = 0),
    hospitalizations = data.frame(day = 1:30, daily_hospitalizations = 0, cumulative_hospitalizations = 0),
    deaths = data.frame(day = 1:30, daily_deaths = 0, cumulative_deaths = 0))

  # userInputDataTable
  # Render cases / hospitalizations / deaths as separate data tables
  output$observedData <- renderRHandsontable({
    df <- switch(input$cases_hospitalizations_or_deaths,
      "Cases" = observed_data$cases, 
      "Hospitalizations" = observed_data$hospitalizations,
      "Deaths" = observed_data$deaths
    ) 

    df$day = 1:nrow(df)

    df[[3]] <- cumsum(df[[2]])

    df %>% 
    rhandsontable() %>% 
    hot_col(col = "day", readOnly = TRUE) %>%
    hot_col(col = 3, readOnly = TRUE)
  })


  # If the data table is updated, update the reactiveValues list 
  # observed_data  to store the data the user is giving us. 
  # 
  # use shiny::isolate on this so we don't overwrite their data when they
  # change the choice of what data they're inputting
  observe({
    if (!is.null(input$observedData)) { 
      isolate({
      switch(input$cases_hospitalizations_or_deaths,
        "Cases" = { 
          observed_data$cases <- hot_to_r(input$observedData) 
        }, 
        "Hospitalizations" = { 
          observed_data$hospitalizations <- hot_to_r(input$observedData) 
        },
        "Deaths" = { 
          observed_data$deaths <- hot_to_r(input$observedData) 
        }
    )
    })
    }
  })


  # this makes the list v11, v12, v13, v21, ... for use in param_names_base
  social_distancing_params <- 
    apply(expand.grid(1:3, 1:3), 1, 
      function(x) { paste0("v", paste0(x,collapse="")) })

  # define the parameters we're going to be using
  param_names_base <- c(social_distancing_params, "s", "e", "p", "kappa",
    "alpha1", "alpha2", "alpha3", "delta","gamma","m1", "m2", "m3","c",
    "obs","k_report","k_inf", "k_susp", "young", "medium", "old", "n",
    "rdetecti", "rdetecta","td")

  # append _int to construct the names of intervention parameters
  param_names_int <- paste0(param_names_base, "_int")

  # reactive that returns the param_vec
  param_vec_reactive <- reactive({

    ### give warning if population doesn't add up to 1
    validate(
      need(input$young<=1, 'young + medium + old must be equal to 1'),
      need(input$medium<=1, 'young + medium + old must be equal to 1'),
      need(input$young+input$medium<=1, 'young + medium + old must be equal to 1')
    )

    ### update the params using inputs
    user_inputs<-c(unlist(reactiveValuesToList(input)))
    param_vec <- load_parameters()
    param_vec[param_names_base]<-as.numeric(user_inputs[param_names_base])
    param_vec$Scenario<-'Base'
    return(param_vec)
  })

  # reactive that returns the param_vec_int
  param_vec_int_reactive <- reactive({
    ### update the params using inputs
    user_inputs<-c(unlist(reactiveValuesToList(input)))
    param_vec_int <- load_parameters()
    for (param_name in param_names_base) {
      param_name_int <- paste0(param_name, "_int")
      if (! is.na(user_inputs[param_name_int])) { 
        param_vec_int[param_name]<-as.numeric(user_inputs[param_name_int])
      }
    }

    param_vec_int$Scenario<-'Intervention'
    return(param_vec_int)
  })
  
  ### calculate R0 and p
  # Rename this to R0_td_value (done)
  R0_td_value <- reactive({
    param_vec <- param_vec_reactive()
    param_vec_int <- param_vec_int_reactive()

    # Edit this to compute R0 and td from p (done)
    R0 = as.numeric(calc_R0_td_from_p(param_vec, param_vec$p)[1])
    p = as.numeric(calc_R0_td_from_p(param_vec, param_vec$p)[2])
    
    return (c(R0, p))
  })
  
  # use exponential curve to estimate td and re
  Re_td_value_exp <- reactive({
    req(input$doublingTimeInterval)
    param_vec <- param_vec_reactive()

    simulation_outcomes <- runSimulations()[[1]]
    
    start_t = input$doublingTimeInterval[1]
    end_t = input$doublingTimeInterval[2]
    cum_case = data.frame(day=c(start_t:end_t), cumulative_cases=(simulation_outcomes$I_1_cum + simulation_outcomes$I_2_cum + simulation_outcomes$I_3_cum + simulation_outcomes$I_1Q_cum + simulation_outcomes$I_2Q_cum + simulation_outcomes$I_3Q_cum)[start_t:end_t])
    s_t = data.frame(day=c(1:dim(simulation_outcomes)[1]), s_t = (simulation_outcomes$S_1+simulation_outcomes$S_2+simulation_outcomes$S_3+simulation_outcomes$S_1Q+simulation_outcomes$S_2Q+simulation_outcomes$S_3Q))
    avg_s_t = mean(s_t[start_t:end_t,2])/param_vec$n
    # Edit this to compute Re and td from exponential curve (done)
    Re = round(as.numeric(calc_Re_td_from_exp(param_vec,cum_case, avg_s_t)[1]), digits=3)
    p = round(as.numeric(calc_Re_td_from_exp(param_vec,cum_case, avg_s_t)[2]), digits=3)
    
    return (c(Re, p))
  })

  # use exponential curve to estimate td and re in the intervention scenario
  # Re_td_value_exp <- reactive({
  Re_td_value_exp_int <- reactive({
    req(input$doublingTimeInterval)
    param_vec <- param_vec_int_reactive()

    simulation_outcomes <- runSimulations()[[2]]
    
    start_t = input$doublingTimeInterval[1]
    end_t = input$doublingTimeInterval[2]
    cum_case = data.frame(day=c(start_t:end_t), cumulative_cases=(simulation_outcomes$I_1_cum + simulation_outcomes$I_2_cum + simulation_outcomes$I_3_cum + simulation_outcomes$I_1Q_cum + simulation_outcomes$I_2Q_cum + simulation_outcomes$I_3Q_cum)[start_t:end_t])
    s_t = data.frame(day=c(1:dim(simulation_outcomes)[1]), s_t = (simulation_outcomes$S_1+simulation_outcomes$S_2+simulation_outcomes$S_3+simulation_outcomes$S_1Q+simulation_outcomes$S_2Q+simulation_outcomes$S_3Q))
    avg_s_t = mean(s_t[start_t:end_t,2])/param_vec$n
    # Edit this to compute Re and td from exponential curve (done)
    Re = round(as.numeric(calc_Re_td_from_exp(param_vec,cum_case, avg_s_t)[1]), digits=3)
    p = round(as.numeric(calc_Re_td_from_exp(param_vec,cum_case, avg_s_t)[2]), digits=3)
    
    return (c(Re, p))
  })

  # Run Simulations Reactive Module
  runSimulations <- reactive({
    req(input$interventionInterval)

    param_vec <- param_vec_reactive()
    param_vec_int <- param_vec_int_reactive()

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

        ### run model without intervention
        simulation_outcomes = run_param_vec(params = param_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, days_out3 = input$sim_time, model_type = run_basic, det_table = det_table) 
        ### run intervention halfway
        simulation_outcomes_int = run_param_vec(params = param_vec, params2 = param_vec_int, days_out1 = input$interventionInterval[1],
                                 days_out2 = input$sim_time, days_out3 = input$interventionInterval[2], model_type = run_int, det_table = det_table_int)

    list(simulation_outcomes, simulation_outcomes_int)
  })

  # Reformat the data for plotting
  formatSimsForPlotting <- reactive({
    sims <- runSimulations()
    format_simulation_outcomes_for_plotting_int(sims[[1]], sims[[2]]) %>% 
      as.data.frame()
  })

  # Reformat for specifically plotting cumulative cases
  formatForCumulativeCasesPlotting <- reactive({
    df <- formatSimsForPlotting()
    compute_cumulative_cases_intervention(df) %>% 
      as.data.frame()
  })

  # Reformat for specifically plotting daily case rates
  formatForDailyCasesPlotting <- reactive({
    df <- formatSimsForPlotting()
    compute_daily_cases_intervention(df) %>% 
      as.data.frame()
  })
  
  ## download adjusted base parameters
  output$download <- downloadHandler(
    filename = function() {
      paste("parameters_base_",Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(param_vec_reactive(), file, row.names = FALSE)
    }
  )

  ## download adjusted intervention parameters
  output$download_int <- downloadHandler(
    filename = function() {
      paste("parameters_int_",Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(param_vec_int_reactive(), file, row.names = FALSE)
    }
  )
    
    ## output for Fits tab
    output$fit <- renderPlot({ plot_fit_to_observed_data_int(formatForCumulativeCasesPlotting(), observed_data$cases) }, res=120)  
    
    ## output for Comp flows tab
    output$comp_flow<- renderPlot({ 
      plot_flows_by_compartment_strata_int(formatSimsForPlotting(), input$compartmentFlowSelectedComps) 
    }, 
    height=1200, 
    res=120) 
    
    output$cumulative_infections_by_age <- renderPlot({ plot_cumulative_cases_by_age_int(formatForCumulativeCasesPlotting()) }, res=120)
    output$cumulative_diagnosed_by_age <- renderPlot({ plot_diagnosed_cumulative_cases_by_age_int(formatForCumulativeCasesPlotting()) }, res=120)
    
    ## output for Death & New case ratio tab
    output$deaths_by_age <- renderPlot({ plot_deaths_by_age_int(formatSimsForPlotting()) }, res=120)
    output$effective_reproductive_number <- renderPlot({ plot_ratio_of_new_to_existing_cases(formatSimsForPlotting()) }, res=120)
    
    ## output for Advanced care & Symptoms ratio tab
    output$cases_needing_advanced_care <- renderPlot({ 
      if (input$adv_care_and_symptoms_cumulative == 'Cumulative') {
        plot_cases_needing_advanced_care_int(formatForCumulativeCasesPlotting(), cumulative=TRUE) 
      } else if (input$adv_care_and_symptoms_cumulative == 'Daily Rates') {
        plot_cases_needing_advanced_care_int(formatForDailyCasesPlotting(), cumulative=FALSE) 
      }
    }, res=120)
    output$cumulative_cases_by_symptoms <- renderPlot({ 
      plot_cases_by_symptom_status_int(formatSimsForPlotting(), cumulative = (input$adv_care_and_symptoms_cumulative == 'Cumulative'))
    }, res=120)

    # if the user preses the Reset All Parameters actionButton, use the
    # shinyjs::reset function to reset all parameters to their default values. 
    # 
    # right now this includes the interventions, is that the behavior we want?
    observeEvent(input$reset_inputs, { sapply(c(param_names_base, param_names_int), 
        shinyjs::reset)
    })

    # Calculate old population fraction from young and medium
    observeEvent(c(input$young, input$medium), {
      updateSliderInput(session, 'old', value = 1 - (input$young + input$medium))
    })
    
    # Make the age distribution of population in intervention same as base scenario
    observeEvent(c(input$young), {
      updateSliderInput(session, 'young_int', value = input$young)
    })
    observeEvent(c(input$medium), {
      updateSliderInput(session, 'medium_int', value = input$medium)
    })
    observeEvent(c(input$old), {
      updateSliderInput(session, 'old_int', value = input$old)
    })
    
    # Make the td, R0, p in intervention same as base scenario in UI
    observeEvent(c(input$td), {
      updateNumericInput(session, 'td_int', value = input$td)
    })
    
    # Make the alphas, obs, n in intervention same as base scenario in UI
    observeEvent(c(input$alpha1), {
      updateSliderInput(session, 'alpha1_int', value = input$alpha1)
    })
    
    observeEvent(c(input$alpha2), {
      updateSliderInput(session, 'alpha2_int', value = input$alpha2)
    })
    
    observeEvent(c(input$alpha3), {
      updateSliderInput(session, 'alpha3_int', value = input$alpha3)
    })
    
    observeEvent(c(input$obs), {
      updateSliderInput(session, 'obs_int', value = input$obs)
    })
    
    observeEvent(c(input$n), {
      updateSliderInput(session, 'n_int', value = input$n)
    })

    # Render the doubling time interval with the maximum as the 
    # simulation time. 
    # 
    # This needs to be done reactively a renderUI so that the 
    # slider takes the user input into account. 
    output$doublingTimeInterval <- renderUI({
      sliderInput(
        inputId = 'doublingTimeInterval', 
        label = 'Interval for Estimating Doubling Time of Infections',
        min = 1, 
        max = input$sim_time,
        step = 1,
        value = c(1,5))
    })
    output$doublingTimeIntervalInt <- renderUI({
      disabled(sliderInput(
        inputId = 'doublingTimeIntervalInt', 
        label = 'Interval for Estimating Doubling Time of Infections',
        min = 1, 
        max = input$sim_time,
        step = 1,
        value = c(1,5)))
    })


    # When the user changes the doubling time interval, calculate the doubling
    # time based on the growth rate during the time period specified.

    output$doublingTime <- renderUI({ 

      re_str <- paste0("The effective reproductive rate between day ", input$doublingTimeInterval[1], " and day ", 
        input$doublingTimeInterval[2], ": ", 

      disabled(numericInput(
          inputId='ReOutput',
          label = 'Re',
          value = Re_td_value_exp()[1])) )
        
      td_str <- paste0("The doubling time between day ", input$doublingTimeInterval[1], ", and day ", 
          input$doublingTimeInterval[2], ": ", 

      disabled(numericInput(
          inputId='doublingTimeOutput',
          label = 'Doubling Time',
          value = Re_td_value_exp()[2])) )
      
      HTML(paste(re_str, td_str, sep = '<br/>'))
    })


    # When the user changes the doubling time interval, calculate the doubling
    # time based on the growth rate (for the intervention) during the specified
    # time period.

    output$doublingTimeInt <- renderUI({ 

      re_str <- paste0("The effective reproductive rate between day ", input$doublingTimeInterval[1], " and day ", 
        input$doublingTimeInterval[2], ": ", 

      disabled(numericInput(
          inputId='ReOutput_Int',
          label = 'Re',
          value = Re_td_value_exp_int()[1])) )
        
      td_str <- paste0("The doubling time between day ", input$doublingTimeInterval[1], ", and day ", 
          input$doublingTimeInterval[2], ": ", 

      disabled(numericInput(
          inputId='doublingTimeOutputInt',
          label = 'Doubling Time',
          value = Re_td_value_exp_int()[2])) )
      
      HTML(paste(re_str, td_str, sep = '<br/>'))
    })
    
    
    # show the corresponding p and R0 when entering td
    # Change this to update R0 and td based on p (done)
    observeEvent(input$p, {
      R0_and_td <- R0_td_value()

      R0 = R0_and_td[1]
      td = R0_and_td[2]

      updateNumericInput(session, 'R0', value = R0)
      updateNumericInput(session, 'R0_int', value = R0)

      updateNumericInput(session, 'td', value = td)
      updateNumericInput(session, 'td_int', value = td)
    })
    
    # show the corresponding p and R0 when entering td
    # Change this to update R0 and td based on p (done)
    observeEvent(input$doublingTimeInterval, {
      updateSliderInput(session, 'doublingTimeIntervalInt', 
                        value=c(input$doublingTimeInterval[1],input$doublingTimeInterval[2]))
    })
    
    # Module for updating the contact matrices entries
    callModule(contact_matrix_server_module, id = NULL)


    # Documentation page
    output$documentation_page <- renderUI({

      tempDocumentation <- file.path(tempdir(), "documentation.md") 

      shiny::withMathJax(
        HTML(
          markdown::markdownToHTML(
            knitr::knit(
              input = system.file(
                'documentation/app/app_documentation.Rmd', 
                package='covid.epi'), 
              output = tempDocumentation,
              quiet=TRUE),
            fragment.only = TRUE
            ))
      )
    })


    # Go through each param name and apply changes from the base case 
    # param vector to the intervention param vector.
    lapply(param_names_base, function(param_name) {
      observeEvent(input[[param_name]], {
        param_name_int <- paste0(param_name, "_int")

        if (param_name_int %in% names(input)) { 
          updateNumericInput(inputId = param_name_int,
            value = input[[param_name]],
            session = session)
        }
        }, ignoreInit = TRUE)
    })

}
