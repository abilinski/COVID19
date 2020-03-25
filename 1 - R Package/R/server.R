# Define server logic required to run the model and display the results
server <- function(input, output, session) {

  df <- load_parameters_table()

  old_vec = df[1,]
  param_vec = df[1,]
  param_names_base = c(colnames(df)[1], colnames(df)[11:34])[!c(colnames(df)[1], colnames(df)[11:34]) %in% c("epsilon","e_ratio")]
  param_names_int = c(colnames(df)[1], unlist(lapply(param_names_base[-1],function(x) paste(x,"int", sep="_"))))
  
    ## for debugging 
    output$renderprint<-renderPrint({
        temp<-c(unlist(reactiveValuesToList(input)))

        # input_names<-names(temp)[!names(temp) %in% c("sim_time","int_time")]
        old_vec[param_names_base]<-as.numeric(temp[param_names_base])
        old_vec['R0'] = calc_R0_from_td(td=old_vec['td'],vec=old_vec)
        old_vec['p']= calc_p_from_R0(R0_input=old_vec['R0'],vec=old_vec) 
        param_vec[param_names_base]<-as.numeric(temp[param_names_int])
        param_vec['R0'] = calc_R0_from_td(td=param_vec['td'],vec=param_vec)
        param_vec['p']= calc_p_from_R0(R0_input=param_vec['R0'],vec=param_vec) 
        old_vec$Scenario<-'Base'
        param_vec$Scenario<-'Intervention'
        # test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
        #                      days_out2 = NULL, model_type = run_basic)
        # test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
        #                          days_out2 = input$sim_time, model_type = run_int)
        print(old_vec)
        print(param_vec)
        # print(tail(test))
        # print(tail(test_int))
    })
    

    ## download adjusted base parameters
    output$download <- downloadHandler(
        filename = function() {
            paste("parameters_base_",Sys.Date(),".csv", sep = "")
        },
        content = function(file) {
            temp<-c(unlist(reactiveValuesToList(input)))
            old_vec[param_names_base]<-as.numeric(temp[param_names_base])
            old_vec['p']= calc_p_from_R0(R0_input=old_vec['R0'],vec=old_vec) 
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
            temp<-c(unlist(reactiveValuesToList(input)))
            param_vec[param_names_base]<-as.numeric(temp[param_names_int])
            param_vec['R0'] = calc_R0_from_td(td=param_vec['td'],vec=param_vec)
            param_vec['p']= calc_p_from_R0(R0_input=param_vec['R0'],vec=param_vec) 
            param_vec$Scenario<-'Intervention'
            write.csv(param_vec, file, row.names = FALSE)
        }
    )
    
    ## output for Fits tab
    output$fit <- renderPlotly({
        ### update the params using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]<-as.numeric(temp[param_names_base])
        old_vec['R0'] = calc_R0_from_td(td=old_vec['td'],vec=old_vec)
        old_vec['p']= calc_p_from_R0(R0_input=old_vec['R0'],vec=old_vec) 
        param_vec[param_names_base]<-as.numeric(temp[param_names_int])
        param_vec['R0'] = calc_R0_from_td(td=param_vec['td'],vec=param_vec)
        param_vec['p']= calc_p_from_R0(R0_input=param_vec['R0'],vec=param_vec) 
        old_vec$Scenario<-'Base'
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int = param_vec)
        g[[8]]
        
    })
    
    ## output for Comp flows tab
    output$comp_flow<- renderPlotly({
        ### update the params using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]<-as.numeric(temp[param_names_base])
        old_vec['R0'] = calc_R0_from_td(td=old_vec['td'],vec=old_vec)
        old_vec['p']= calc_p_from_R0(R0_input=old_vec['R0'],vec=old_vec) 
        param_vec[param_names_base]<-as.numeric(temp[param_names_int])
        param_vec['R0'] = calc_R0_from_td(td=param_vec['td'],vec=param_vec)
        param_vec['p']= calc_p_from_R0(R0_input=param_vec['R0'],vec=param_vec) 
        old_vec$Scenario<-'Base'
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        g[[7]]
          
    })
    
    ## output for Cumulative case tab
    output$cum_case<- renderPlotly({
        ### update the params using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]<-as.numeric(temp[param_names_base])
        old_vec['R0'] = calc_R0_from_td(td=old_vec['td'],vec=old_vec)
        old_vec['p']= calc_p_from_R0(R0_input=old_vec['R0'],vec=old_vec) 
        param_vec[param_names_base]<-as.numeric(temp[param_names_int])
        param_vec['R0'] = calc_R0_from_td(td=param_vec['td'],vec=param_vec)
        param_vec['p']= calc_p_from_R0(R0_input=param_vec['R0'],vec=param_vec) 
        old_vec$Scenario<-'Base'
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        # multiplot(g[[2]], g[[4]], cols = 2)
        subplot(g[[2]], g[[4]], nrows=1)
    })
    
    ## output for Death & New case ratio tab
    output$death_new_case<- renderPlotly({
        ### update the param_vec using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]<-as.numeric(temp[param_names_base])
        old_vec['R0'] = calc_R0_from_td(td=old_vec['td'],vec=old_vec)
        old_vec['p']= calc_p_from_R0(R0_input=old_vec['R0'],vec=old_vec) 
        param_vec[param_names_base]<-as.numeric(temp[param_names_int])
        param_vec['R0'] = calc_R0_from_td(td=param_vec['td'],vec=param_vec)
        param_vec['p']= calc_p_from_R0(R0_input=param_vec['R0'],vec=param_vec) 
        old_vec$Scenario<-'Base'
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        # multiplot(g[[5]],g[[3]],cols = 2)
        subplot(g[[5]], g[[3]], nrows=1)
        
    })
    
    ## output for Advanced care & Symptoms ratio tab
    output$care_symptoms<- renderPlotly({
        ### update the param_vec using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]<-as.numeric(temp[param_names_base])
        old_vec['R0'] = calc_R0_from_td(td=old_vec['td'],vec=old_vec)
        old_vec['p']= calc_p_from_R0(R0_input=old_vec['R0'],vec=old_vec) 
        param_vec[param_names_base]<-as.numeric(temp[param_names_int])
        param_vec['R0'] = calc_R0_from_td(td=param_vec['td'],vec=param_vec)
        param_vec['p']= calc_p_from_R0(R0_input=param_vec['R0'],vec=param_vec) 
        old_vec$Scenario<-'Base'
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        # multiplot(g[[9]],g[[6]],cols = 2)
        subplot(g[[9]], g[[6]], nrows=1)
        
    })

    # if the user preses the Reset All Parameters actionButton, use the
    # shinyjs::reset function to reset all parameters to their default values. 
    # 
    # right now this includes the interventions, is that the behavior we want?
    observeEvent(input$reset_inputs, { 
      sapply(c('days_out1', 'days_out2', 'R0', 'delta', 'gamma', 'n', 's', 'e',
               'p', 'kappa', 'alpha1', 'alpha2', 'alpha3', 'c', 'm1', 'm2',
               'm3', 'young', 'medium', 'old', 'k_report', 'k_inf', 'k_susp'),
             shinyjs::reset)
    })
}
