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
# upload paramters
# fix contact matrix & obs, drop epsilon and e_ratio, and update other prameters per user's input
df = read.csv("source/parameters_18_mar_2020.csv", as.is = T)
#df = read.csv("source/parameters_shiny.csv", as.is = T)
param_vec=df[1,]

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
    
    downloadButton("download", "Download parameters"),
    hr(),
    
    fluidRow(#class = "text-center",
        column(2,
               radioButtons("intervention", label = "Intervention",
                            choices = list("yes" = 1, "no" = 2), selected = 2),
               conditionalPanel(
                   condition = "input.intervention == 1",
                   numericInput("days_out1", label="intervention starts at", value=15),
                   numericInput("days_out2", label="simulation time (days)", value=30),
               ),
               conditionalPanel(
                   condition = "input.intervention == 2",
                   numericInput("days_out1", label="simulation time (days)", value=30)
               ),
               numericInput("R0", label="R0", value=2.2),
               numericInput("delta", label=HTML("&delta;:"), value=0.2),
               numericInput("gamma", label=HTML("&gamma;:"), value=0.2),
               numericInput("n", label="n", value=1938000)
        ),
        column(2,
               sliderInput("s", label = "s", min = 0, 
                           max = 1, value = 0),
               sliderInput("e", label = "e", min = 0, 
                           max = 1, value = 0),
               sliderInput("p", label = "p", min = 0.01, 
                           max = 0.1, value = 0.05),
               sliderInput("kappa", label = HTML("&kappa;:"), min = 0, 
                           max = 1, value = 0.375)
        ),
        column(2,
               sliderInput("alpha1", label = HTML("&alpha;1"), min = 0, 
                           max = 1, value = 0.75),
               sliderInput("alpha2", label = HTML("&alpha;2"), min = 0, 
                           max = 1, value = 0.3),
               sliderInput("alpha3", label = HTML("&alpha;1"), min = 0, 
                           max = 1, value = 0.3),
               sliderInput("c", label = "c", min = 0, 
                           max = 1, value = 0.13)
        ),
        column(2,
               sliderInput("m1", label = "m1", min = 0, 
                           max = 1, value = 0),
               sliderInput("m2", label = "m2", min = 0, 
                           max = 1, value = 0.005),
               sliderInput("m3", label = "m3", min = 0, 
                           max = 1, value = 0.1)
        ),
        column(2,
               sliderInput("youth", label = "young", min = 0, 
                           max = 1, value = 0.24),
               sliderInput("adults", label = "medium ", min = 0, 
                           max = 1, value = 0.6),
               sliderInput("older adults", label = "old", min = 0, 
                           max = 1, value = 0.15),
               #p("*young+medium+old <= 1"),
        ),
        column(2,
               sliderInput("k_report", label = "k_report", min = 0, 
                           max = 1, value = 1),
               sliderInput("k_inf", label = "k_inf", min = 0, 
                           max = 1, value = 1),
               sliderInput("k_susp", label = "k_susp", min = 0, 
                           max = 1, value = 1)
        )
              
    )
)

# Define server logic required to run the model and display the results
server <- function(input, output) {
   
    ## for debugging 
    output$renderprint<-renderPrint({
        temp<-c(unlist(reactiveValuesToList(input)))
        input_names<-names(temp)[!names(temp) %in% c("intervention","days_out1","days_out2")]
        old_vec<-param_vec
        param_vec[input_names]=as.numeric(temp[input_names])
        print(old_vec)
        print(param_vec)
    })
    
    ## download adjusted paramters
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
        ### update the param_vec using inputs
        temp<-unlist(reactiveValuesToList(input))
        input_names<-names(temp)[!names(temp) %in% c("intervention","days_out1","days_out2")]
        old_vec<-param_vec
        param_vec[input_names]<-as.numeric(temp[input_names])
        
        ### run model without intervention
        if (input$intervention==2) {
            test = run_param_vec(params = param_vec, params2 = NULL, days_out1 = input$days_out1,
                                 days_out2 = NULL, model_type = run_basic)
        ### make plots
        f = make_plots(test, params = param_vec)
        multiplot(f[[8]],
                  cols = 1)
        } else {
            ### run model without intervention
            test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$days_out2,
                                 days_out2 = NULL, model_type = run_basic) #days_out2 here is sim time
            ### run intervention halfway
            test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$days_out1,
                                     days_out2 = input$days_out2, model_type = run_int)
            ### make plots
            g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
            multiplot(g[[8]],
                      cols = 1)
            
        }
        
    })
    
    ## output for Comp flows tab
    output$comp_flow<- renderPlot({
        ### update the param_vec using inputs
        temp<-unlist(reactiveValuesToList(input))
        input_names<-names(temp)[!names(temp) %in% c("intervention","days_out1","days_out2")]
        old_vec<-param_vec
        param_vec[input_names]<-as.numeric(temp[input_names])
        
        ### run model without intervention
        if (input$intervention==2) {
            test = run_param_vec(params = param_vec, params2 = NULL, days_out1 = input$days_out1,
                                 days_out2 = NULL, model_type = run_basic)
            ### make plots
            f = make_plots(test, params = param_vec)
            multiplot(f[[7]],
                      cols = 1)
        } else {
            ### run model without intervention
            test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$days_out2,
                                 days_out2 = NULL, model_type = run_basic) #days_out2 here is sim time
            ### run intervention halfway
            test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$days_out1,
                                     days_out2 = input$days_out2, model_type = run_int)
            ### make plots
            g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
            multiplot(g[[7]],
                      cols = 1)
        }
        
    })
    
    ## output for Cumulative case tab
    output$cum_case<- renderPlot({
        ### update the param_vec using inputs
        temp<-unlist(reactiveValuesToList(input))
        input_names<-names(temp)[!names(temp) %in% c("intervention","days_out1","days_out2")]
        old_vec<-param_vec
        param_vec[input_names]<-as.numeric(temp[input_names])
        
        ### run model without intervention
        if (input$intervention==2) {
            test = run_param_vec(params = param_vec, params2 = NULL, days_out1 = input$days_out1,
                                 days_out2 = NULL, model_type = run_basic)
            ### make plots
            f = make_plots(test, params = param_vec)
            multiplot(f[[2]], f[[4]],
                      cols = 2)
        } else {
            ### run model without intervention
            test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$days_out2,
                                 days_out2 = NULL, model_type = run_basic) #days_out2 here is sim time
            ### run intervention halfway
            test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$days_out1,
                                     days_out2 = input$days_out2, model_type = run_int)
            ### make plots
            g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
            multiplot(g[[2]], g[[4]],
                      cols = 2)
        }
        
    })
    
    ## output for Death & New case ratio tab
    output$death_new_case<- renderPlot({
        ### update the param_vec using inputs
        temp<-unlist(reactiveValuesToList(input))
        input_names<-names(temp)[!names(temp) %in% c("intervention","days_out1","days_out2")]
        old_vec<-param_vec
        param_vec[input_names]<-as.numeric(temp[input_names])
        
        ### run model without intervention
        if (input$intervention==2) {
            test = run_param_vec(params = param_vec, params2 = NULL, days_out1 = input$days_out1,
                                 days_out2 = NULL, model_type = run_basic)
            ### make plots
            f = make_plots(test, params = param_vec)
            multiplot(f[[5]],f[[3]],
                      cols = 2)
        } else {
            ### run model without intervention
            test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$days_out2,
                                 days_out2 = NULL, model_type = run_basic) #days_out2 here is sim time
            ### run intervention halfway
            test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$days_out1,
                                     days_out2 = input$days_out2, model_type = run_int)
            ### make plots
            g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
            multiplot(g[[5]],g[[3]],
                      cols = 2)
        }
        
    })
    
    ## output for Advanced care & Symptoms ratio tab
    output$care_symptoms<- renderPlot({
        ### update the param_vec using inputs
        temp<-unlist(reactiveValuesToList(input))
        input_names<-names(temp)[!names(temp) %in% c("intervention","days_out1","days_out2")]
        old_vec<-param_vec
        param_vec[input_names]<-as.numeric(temp[input_names])
        
        ### run model without intervention
        if (input$intervention==2) {
            test = run_param_vec(params = param_vec, params2 = NULL, days_out1 = input$days_out1,
                                 days_out2 = NULL, model_type = run_basic)
            ### make plots
            f = make_plots(test, params = param_vec)
            multiplot(f[[9]],f[[6]],
                      cols = 2)
        } else {
            ### run model without intervention
            test = run_param_vec(params = old_vec, params2 = NULL, days_out1 = input$days_out2,
                                 days_out2 = NULL, model_type = run_basic) #days_out2 here is sim time
            ### run intervention halfway
            test_int = run_param_vec(params = old_vec, params2 = param_vec, days_out1 = input$days_out1,
                                     days_out2 = input$days_out2, model_type = run_int)
            ### make plots
            g = make_plots_int(test, params = old_vec, test_int, params_int=param_vec)
            multiplot(g[[9]],g[[6]],
                      cols = 2)
        }
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
