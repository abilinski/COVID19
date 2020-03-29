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
#'   - Adding Calibration to User-Uploaded Case Series
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
#' @seealso generate_ui runApp 
#' 
#' @export
server <- function(input, output, session) {

  # Get the default parameter vector for the model
  default_param_vec = load_parameters() 

  # TODO: I (Christian) think we should make sure the parameters.csv that gets 
  # loaded in *only* has parameters that we actually take as inputs. 
  # 
  # Previously we were doing things like this: 
  # 
  # param_names_base = # colnames(df)[2:ncol(df)]
  #   c(colnames(df)[1], colnames(df)[11:34])[!c(colnames(df)[1], 
  #   colnames(df)[11:34]) %in% c("epsilon","e_ratio")]
  # 
  # param_names_int = c(colnames(df)[1], 
  #   unlist(lapply(param_names_base[-1],function(x) paste(x,"int", sep="_"))))
  # param_names_int <- paste0(param_names_base, "_int")
  # 
  # I just worry this will get too confusing to update over time, so I updated 
  # the definition of param_names_base below to be explicit about what we're 
  # parameters are used.
  # 

  # this just makes the list v11, v12, v13, v21, ... 
  social_distancing_params <- 
    apply(expand.grid(1:3, 1:3), 1, 
      function(x) { paste0("v", paste0(x,collapse="")) })

  param_names_base <- c(social_distancing_params, "s", "e", "p", "kappa",
    "alpha1", "alpha2", "alpha3", "delta","gamma","m1", "m2", "m3","c",
    "obs","k_report","k_inf", "k_susp", "young", "medium", "old", "n",
    "rdetecti", "rdetecta","td")

  param_names_int <- paste0(param_names_base, "_int")

  param_vec_reactive <- reactive({
    ### update the params using inputs
    user_inputs<-c(unlist(reactiveValuesToList(input)))
    param_vec <- load_parameters()
    param_vec[param_names_base]<-as.numeric(user_inputs[param_names_base])
    param_vec$Scenario<-'Base'
    return(param_vec)
  })

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


  # Model Plots 
  # 
  # Use the user input to run the model for the base case and intervention.
  # Make plots of the outcomes.
  # Returns a list of plots
  #
  model_plots <- reactive({ 
        ### update the params using inputs
        user_inputs<-c(unlist(reactiveValuesToList(input)))
        param_vec <- param_vec_reactive()
        param_vec_int <- param_vec_int_reactive()

        det_table <- data.frame(
          time = 1:(input$sim_time),
          rdetecti = rep(input$rdetecti, input$sim_time),
          rdetecta = rep(input$rdetecta, input$sim_time))

        det_table_int <- data.frame(
          time = 1:(input$sim_time),
          rdetecti = c(rep(input$rdetecti, input$int_time), 
            rep(input$rdetecti_int, (input$sim_time - input$int_time))),
          rdetecta = c(rep(input$rdetecta, input$int_time), 
            rep(input$rdetecta_int, (input$sim_time - input$int_time))))
        
        ### give warning if population doesn't add up to 1
        validate(
          need(input$young<=1, 'total population = 1!!'),
          need(input$medium<=1, 'total population = 1!!'),
          need(input$young+input$medium<=1, 'total population = 1!!')
        )
        ### run model without intervention
        test = run_param_vec(params = param_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic, det_table = det_table) 
        ### run intervention halfway
        test_int = run_param_vec(params = param_vec, params2 = param_vec_int, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int, det_table = det_table_int)
        ### make plots
        g = make_plots_int(test, params = param_vec, test_int, params_int = param_vec_int)

        return(g)
  })

  
    ## download adjusted base parameters
    output$download <- downloadHandler(
        filename = function() {
            paste("parameters_base_",Sys.Date(),".csv", sep = "")
        },
        content = function(file) {
            old_vec['R0'] = calc_R0_from_td(td=param_vec_int['td'],vec=param_vec_int)
            old_vec['p']= calc_p_from_R0(R0_input=param_vec_reactive()['R0'],vec=param_vec_reactive()) 
            old_vec$Scenario<-'Base'
            write.csv(old_vec, file, row.names = FALSE)
        }
    )
    
    ## download adjusted intervention parameters
    output$download_int <- downloadHandler(
        filename = function() {
            paste("parameters_int_",Sys.Date(),".csv", sep = "")
        },
        content = function(file) {
          param_vec_int <- param_vec_int_reactive()
            param_vec_int['R0'] = calc_R0_from_td(td=param_vec_int['td'],vec=param_vec_int)
            param_vec_int['p']= calc_p_from_R0(R0_input=param_vec_int['R0'],vec=param_vec_int) 
            param_vec_int$Scenario<-'Intervention'
            write.csv(param_vec_int, file, row.names = FALSE)
        }
    )
    
    ## output for Fits tab
    output$fit <- renderPlot({ model_plots()[[8]] })
    
    ## output for Comp flows tab
    output$comp_flow<- renderPlot({ model_plots()[[7]] })
    
    output$cumulative_infections_by_age <- renderPlot({ model_plots()[[2]] })
    output$cumulative_diagnosed_by_age <- renderPlot({ model_plots()[[4]] })
    
    ## output for Death & New case ratio tab
    output$deaths_by_age <- renderPlot({ model_plots()[[5]] })
    output$effective_reproductive_number <- renderPlot({ model_plots()[[3]] })
    
    ## output for Advanced care & Symptoms ratio tab
    output$cases_needing_advanced_care <- renderPlot({ model_plots()[[9]] })
    output$cumulative_cases_by_symptoms <- renderPlot({ model_plots()[[6]] })

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
    
    # Calculate old population fraction from young and medium
    observeEvent(c(input$young_int, input$medium_int), {
      updateSliderInput(session, 'old_int', value = 1 - (input$young_int + input$medium_int))
    })

    callModule(contact_matrix_server_module, id = NULL)

    lapply(param_names_base, function(param_name) {
      observeEvent(input[[param_name]], {
        param_name_int <- paste0(param_name, "_int")

        if (param_name_int %in% names(input)) { 
          updateNumericInput(inputId = param_name_int,
            value = input[[param_name]],
            session = session)
        }
        })
    })

}
