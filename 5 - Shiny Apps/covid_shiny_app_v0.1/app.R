#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("source/model_3strat_18_mar_2020.R")
# upload paramter names (should hardcode the names in the future)
# fix contact matrix & obs, drop epsilon and e_ratio, and update other prameters per user's input
df = read.csv("source/parameters_18_mar_2020.csv", as.is = T)
old_vec = df[1,]
param_vec = df[2,]
param_names_base = c(colnames(df)[1], colnames(df)[11:34])[!c(colnames(df)[1], colnames(df)[11:34]) %in% c("epsilon","e_ratio", "obs")]
param_names_int = c(colnames(df)[1], unlist(lapply(param_names_base,function(x) paste(x,"int", sep="_"))))

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("covid_epi_model"),
    tabsetPanel(
        tabPanel("Fits", plotOutput("fit"), p("*Currently only fit to data for 15 days")),
        tabPanel("Comp flows", plotOutput("comp_flow")),
        tabPanel("Cumulative cases", plotOutput("cum_case")), 
        tabPanel("Deaths & New case ratio", plotOutput("death_new_case")),
        tabPanel("Advanced care & Symptoms", plotOutput("care_symptoms"))
        #tabPanel("Intervention", plotOutput("intervention"))
        
    ),
    #verbatimTextOutput("renderprint"),
    
    hr(),
    column(6,
           h3("Base case parameters"),
           wellPanel(
        fluidRow(#class = "text-center",
            column(4,
                   numericInput("sim_time", label="simulation time (days)", value=30),
                   #numericInput("int_time", label="intervention starts at", value=15),
                   numericInput("R0", label="R0", value=2.2),
                   numericInput("delta", label=HTML("&delta;:"), value=0.2),
                   numericInput("gamma", label=HTML("&gamma;:"), value=0.2),
                   numericInput("n", label="n", value=1938000)
                   #maybe put matrix here
            ),
            column(4,
                   sliderInput("s", label = "s", min = 0, 
                               max = 1, value = 0),
                   sliderInput("e", label = "e", min = 0, 
                               max = 1, value = 0),
                   sliderInput("p", label = "p", min = 0.01, 
                               max = 0.1, value = 0.05),
                   sliderInput("kappa", label = HTML("&kappa;:"), min = 0, 
                               max = 1, value = 0.375),
                   sliderInput("m1", label = "m1", min = 0, 
                               max = 1, value = 0),
                   sliderInput("m2", label = "m2", min = 0, 
                               max = 1, value = 0.005),
                   sliderInput("m3", label = "m3", min = 0, 
                               max = 1, value = 0.1),
                   sliderInput("k_report", label = "k_report", min = 0, 
                               max = 1, value = 1)
            ),
            column(4,
                   sliderInput("alpha1", label = HTML("&alpha;1"), min = 0, 
                               max = 1, value = 0.75),
                   sliderInput("alpha2", label = HTML("&alpha;2"), min = 0, 
                               max = 1, value = 0.3),
                   sliderInput("alpha3", label = HTML("&alpha;1"), min = 0, 
                               max = 1, value = 0.3),
                   sliderInput("c", label = "c", min = 0, 
                               max = 1, value = 0.13),
                   sliderInput("young", label = "youth", min = 0, 
                               max = 1, value = 0.24),
                   sliderInput("medium", label = "adults", min = 0, 
                               max = 1, value = 0.6),
                   sliderInput("old", label = "older adults", min = 0, 
                               max = 1, value = 0.15),
                   sliderInput("k_inf", label = "k_inf", min = 0, 
                               max = 1, value = 1),
                   sliderInput("k_susp", label = "k_susp", min = 0, 
                               max = 1, value = 1)
                   #p("*young+medium+old <= 1"),
                ),
            downloadButton("download", "Download parameters"),
                  
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
                          numericInput("delta_int", label=HTML("&delta;:"), value=0.2),
                          numericInput("gamma_int", label=HTML("&gamma;:"), value=0.2),
                          numericInput("n_int", label="n", value=1938000)
                          #maybe put matrix here
                   ),
                   column(4,
                          sliderInput("s_int", label = "s", min = 0, 
                                      max = 1, value = 0),
                          sliderInput("e_int", label = "e", min = 0, 
                                      max = 1, value = 0),
                          sliderInput("p_int", label = "p", min = 0.01, 
                                      max = 0.1, value = 0.05),
                          sliderInput("kappa_int", label = HTML("&kappa;:"), min = 0, 
                                      max = 1, value = 0.375),
                          sliderInput("m1_int", label = "m1", min = 0, 
                                      max = 1, value = 0),
                          sliderInput("m2_int", label = "m2", min = 0, 
                                      max = 1, value = 0.005),
                          sliderInput("m3_int", label = "m3", min = 0, 
                                      max = 1, value = 0.1),
                          sliderInput("k_report_int", label = "k_report", min = 0, 
                                      max = 1, value = 1)
                   ),
                   column(4,
                          sliderInput("alpha1_int", label = HTML("&alpha;1"), min = 0, 
                                      max = 1, value = 0.75),
                          sliderInput("alpha2_int", label = HTML("&alpha;2"), min = 0, 
                                      max = 1, value = 0.3),
                          sliderInput("alpha3_int", label = HTML("&alpha;1"), min = 0, 
                                      max = 1, value = 0.3),
                          sliderInput("c_int", label = "c", min = 0, 
                                      max = 1, value = 0.13),
                          sliderInput("young_int", label = "youth_int", min = 0, 
                                      max = 1, value = 0.24),
                          sliderInput("medium_int", label = "adults_int", min = 0, 
                                      max = 1, value = 0.6),
                          sliderInput("old_int", label = "older adults_int", min = 0, 
                                      max = 1, value = 0.15),
                          sliderInput("k_inf_int", label = "k_inf", min = 0, 
                                      max = 1, value = 1),
                          sliderInput("k_susp_int", label = "k_susp", min = 0, 
                                      max = 1, value = 1)
                          #p("*young+medium+old <= 1"),
                   ),
                   downloadButton("download_int", "Download parameters")
                   
               )
           )
    )
)

# Define server logic required to run the model and display the results
server <- function(input, output) {
   
    ## for debugging 
    output$renderprint<-renderPrint({
        temp<-c(unlist(reactiveValuesToList(input)))
        # input_names<-names(temp)[!names(temp) %in% c("sim_time","int_time")]
        old_vec[param_names_base]=as.numeric(temp[param_names_base])
        param_vec[param_names_int]=as.numeric(temp[param_names_int])
        old_vec$Scenario<-'Base'
        param_vec$Scenario<-'Intervention'
        print(old_vec)
        print(param_vec)
    })
    
    ## download adjusted parameters
    output$download <- downloadHandler(
        filename = function() {
            paste("parameters_shiny_",Sys.Date(),".csv", sep = "")
        },
        content = function(file) {
            temp<-c(unlist(reactiveValuesToList(input)))
            input_names<-names(temp)[!names(temp) %in% c("intervention","days_out1","days_out2")]
            param_vec[input_names]=as.numeric(temp[input_names])
            write.csv(param_vec, file, row.names = FALSE)
        }
    )
    
    ## output for Fits tab
    output$fit <- renderPlot({
        ### update the params using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]=as.numeric(temp[param_names_base])
        param_vec[param_names_int]=as.numeric(temp[param_names_int])
        old_vec$Scenario<-'Base' # can leave it outside in reactive()
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int = param_vec)
        multiplot(g[[8]],
                      cols = 1)
        
    })
    
    ## output for Comp flows tab
    output$comp_flow<- renderPlot({
        ### update the params using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]=as.numeric(temp[param_names_base])
        param_vec[param_names_int]=as.numeric(temp[param_names_int])
        old_vec$Scenario<-'Base' # can leave it outside in reactive()
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        multiplot(g[[7]],
                  cols = 1)
    
    })
    
    ## output for Cumulative case tab
    output$cum_case<- renderPlot({
        ### update the params using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]=as.numeric(temp[param_names_base])
        param_vec[param_names_int]=as.numeric(temp[param_names_int])
        old_vec$Scenario<-'Base' # can leave it outside in reactive()
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        multiplot(g[[2]], g[[4]],
                  cols = 2)
    
    })
    
    ## output for Death & New case ratio tab
    output$death_new_case<- renderPlot({
        ### update the param_vec using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]=as.numeric(temp[param_names_base])
        param_vec[param_names_int]=as.numeric(temp[param_names_int])
        old_vec$Scenario<-'Base' # can leave it outside in reactive()
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        multiplot(g[[5]],g[[3]],
                  cols = 2)
        
    })
    
    ## output for Advanced care & Symptoms ratio tab
    output$care_symptoms<- renderPlot({
        ### update the param_vec using inputs
        temp<-c(unlist(reactiveValuesToList(input)))
        old_vec[param_names_base]=as.numeric(temp[param_names_base])
        param_vec[param_names_int]=as.numeric(temp[param_names_int])
        old_vec$Scenario<-'Base' # can leave it outside in reactive()
        param_vec$Scenario<-'Intervention'
        
        ### run model without intervention
        test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$sim_time,
                             days_out2 = NULL, model_type = run_basic) 
        ### run intervention halfway
        test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$int_time,
                                 days_out2 = input$sim_time, model_type = run_int)
        ### make plots
        g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
        multiplot(g[[9]],g[[6]],
                      cols = 2)
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
