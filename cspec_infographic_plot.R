##### covid infographics code  #####
##### load in libraries 
library(reshape2)
library(directlabels)
library(waffle)
library(extrafont)
library(scales)
library(dplyr)
library(ggplot2)
#set working directory to where package is cloned
setwd("~/COVID19/1 - R Package/")
devtools::load_all()

#####set a cspec color palette
# (based on website and logo)
cspec_pal<-c("#1c133c", "#0089b8", "#be1622", "#ffffff")
#####define ggplot themes 
#best for non-line plots
#recommend using cspec_pal[3] for the fill of a bar/col plot
cspec_theme <- function() {
  theme(
    plot.background = element_rect(fill = cspec_pal[4], colour = cspec_pal[4]),
    panel.background = element_rect(fill = cspec_pal[4]),
    axis.text = element_text(colour = cspec_pal[1], family = "Impact"),
    plot.title = element_text(colour = cspec_pal[3], face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = cspec_pal[3], face = "bold", size = 13, family = "Impact"),
    panel.grid.major.x = element_line(colour = cspec_pal[1]),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Impact", colour = cspec_pal[2]),
    strip.background = element_rect(fill = cspec_pal[1]),
    axis.ticks = element_line(colour = cspec_pal[1])
  )
}
### best for line plots 
### recommended that cspec_pal[1] and cspec_pal[3] be used for data lines (where limited data presented)
### example: use cspec_pal[3] for basecase and cspec_pal[1] for intervention. 
cspec_theme2 <- function() {
  theme(
    legend.title = element_text(family = "Impact", colour = cspec_pal[2], size = 10),
    legend.background = element_rect(fill = cspec_pal[4]),
    legend.key = element_rect(fill = cspec_pal[4], colour = cspec_pal[4]),
    legend.text = element_text(family = "Impact", colour = cspec_pal[2], size = 10),
    plot.background = element_rect(fill = cspec_pal[4], colour = cspec_pal[4]),
    panel.background = element_rect(fill = cspec_pal[4]),
    axis.text = element_text(colour = cspec_pal[2], family = "Impact"),
    plot.title = element_text(colour = cspec_pal[3], face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = cspec_pal[3], face = "bold", size = 13, family = "Impact"),
    panel.grid.major.y = element_line(colour = cspec_pal[2]),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Impact", colour = cspec_pal[2]),
    strip.background = element_rect(fill = cspec_pal[2]),
    axis.ticks = element_line(colour = cspec_pal[4])
  )
}

###waffle plot
cspec_theme_waf <- function() {
  theme(
    legend.title = element_text(family = "Impact", colour = cspec_pal[2], size = 10), 
    legend.background = element_rect(fill = cspec_pal[4]),
    legend.key = element_rect(fill = cspec_pal[4], colour = cspec_pal[4]),
    legend.text = element_text(family = "Impact", colour = cspec_pal[2], size = 10),
    plot.background = element_rect(fill = cspec_pal[4], colour = cspec_pal[4]),
    axis.text = element_blank(),
    plot.title = element_text(colour = cspec_pal[3], face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Impact", colour = cspec_pal[2]),
    strip.background = element_rect(fill = cspec_pal[1]),
    axis.ticks = element_blank()
  )
}

#####add a function that is used later to format the data 
#make out function
make_out<-function(test, params){
  out = test %>%
    gather(var, value, -time) %>% { suppressWarnings(separate(., var, into = c("comp", "strat", "cum"), sep = "_")) } %>%
    mutate(cum = ifelse(is.na(cum), F, T),
           
           # reformat compartments
           comp2 = ifelse(comp %in% c("UA","DA","A"), "Asymptomatic", "Symptomatic"),
           comp2 = ifelse(comp=="E", "Exposed", comp2),
           comp2 = ifelse(comp=="R", "Recovered", comp2),
           comp2 = ifelse(comp=="S", "Susceptible", comp2),
           comp2 = factor(comp2, levels = c("Susceptible", "Exposed", "Asymptomatic",
                                            "Symptomatic", "Recovered")),
           
           # compartments with U/D
           comp3 = ifelse(comp %in% c("DI","DA"), "Detected", "Undetected"),
           comp3 = ifelse(comp %in% c("I","A"), "Infected", comp3),
           comp3 = ifelse(comp=="E", "Exposed", comp3),
           comp3 = ifelse(comp=="R", "Recovered", comp3),
           comp3 = ifelse(comp=="S", "Susceptible", comp3),
           comp3 = factor(comp3, levels = c("Susceptible", "Exposed", 
                                            "Detected", "Undetected", "Infected",
                                            "Recovered")),
           
           # reformat strata
           strat2 = ifelse(strat=="1", "<20", "<20 (SD)"),
           strat2 = ifelse(strat=="2", "21-65", strat2),
           strat2 = ifelse(strat=="2Q", "21-65 (SD)", strat2),
           strat2 = ifelse(strat=="3", ">65", strat2),
           strat2 = ifelse(strat=="3Q", ">65 (SD)", strat2),
           strat2 = factor(strat2, levels = c("<20", "<20 (SD)",
                                              "21-65", "21-65 (SD)",
                                              ">65", ">65 (SD)")),
           
           # get only age
           strat3 = factor(sub(" \\(SD\\)", "", strat2), levels = c("<20", "21-65", ">65"))
    )
  return(out)
}

#####make a plot showing that intervention timing is important 
#this plot displays the results of comparing an intervention 
#starting on day 1 and on day n. The intervention is defined by 
#50% of the population being in social distancing. Case counts are 
#compared 28 days after the later intervention. For example, the bar 
#at x=20 is the comparison of case counts at day 49 
#(intervention day 21-intervention day 1). 
timing_plot<-function(){

  df <- load_parameters_table()
  timing_ratio_list<-list()

  simulation_length_vec<-28:78
  timing_ratio_vec<-rep(0,length(simulation_length_vec))
  cases_vec<-effect_ratio_vec<-rep(0,length(simulation_length_vec))
  
  for(i in 1:length(simulation_length_vec)){
    print(paste("running simulation #", i, "of 100"))
    simulation_length<-simulation_length_vec[i]
    #set the detection rates equal 
    #rates of detection 
    bc_rdetecti<-sd_rdetecti<-.1
    bc_rdetecta<-sd_rdetecta<-.01
    
    ###run the basecase in case it's useful 
    bc_params<-df[1,]
    bc_det_table <- data.frame(
      time = 1:simulation_length,
      rdetecti = rep(bc_rdetecti, simulation_length),
      rdetecta = rep(bc_rdetecta, simulation_length))
    
    
    ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    ##### #####  explore the effects of intervention timing  ##### #####
    ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

    #basecase is start intervention on day 1
    intv_timing_early<-1
    #compare to starting it on day n 
    intv_timing_late<-i
    #pull the social distancing scenario  
    sd_params<-df[8,]
    
    # sd_params$R0<-R0_vec[j]
    sde_det_table <- data.frame(
      time = 1:(simulation_length),
      rdetecti = c(rep(bc_rdetecti, intv_timing_early), rep(sd_rdetecti, (simulation_length - intv_timing_early))),
      rdetecta = c(rep(bc_rdetecta, intv_timing_early), rep(sd_rdetecta, (simulation_length - intv_timing_early))))
    
    sdl_det_table <- data.frame(
      time = 1:(simulation_length),
      rdetecti = c(rep(bc_rdetecti, intv_timing_late), rep(sd_rdetecti, (simulation_length - intv_timing_late))),
      rdetecta = c(rep(bc_rdetecta, intv_timing_late), rep(sd_rdetecta, (simulation_length - intv_timing_late))))
    
    ### run intervention 
    sde_test = run_param_vec(params = bc_params, params2 = sd_params, days_out1 = intv_timing_early,
                             days_out2 = simulation_length, days_out3 = simulation_length, model_type = run_int, det_table = sde_det_table)
    
    sdl_test = run_param_vec(params = bc_params, params2 = sd_params, days_out1 = intv_timing_late,
                             days_out2 = simulation_length, days_out3 = simulation_length, model_type = run_int, det_table = sdl_det_table)

    ###format the data 
    sde_out<-make_out(sde_test, sde_params)
    sdl_out<-make_out(sdl_test, sdl_params)
    ###filter down to total infected (detected and total)

    sde_out_cases = sde_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>% 
      summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) 
    sdl_out_cases = sdl_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>% 
      summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) 

    #whats the difference of cases based on timing
    cases_vec[i]<-as.numeric(sdl_out_cases[simulation_length,3]-sde_out_cases[simulation_length,3])
  } 
##make a bar plot
  #make a dataframe 
  cases_df<-data.frame(Delay=1:50, CaseRatio=cases_vec[-1] )

  t3<- ggplot(cases_df, aes(x=CaseRatio, y=Delay))+geom_col(fill=cspec_pal[2])+coord_flip()+
    labs(y="Intervention Delay (in Days)", x="Additional Cases vs. Day 1 Intervention",
         title="Every day we delay an intervention, cases counts increase!")+cspec_theme()
  return(t3)
}

#####make a plot showing the difference between social distancing percentages
#this plot shows the difference between 25% and 50% of the population in social
#distancing after 4 weeks. 

effective_plot<-function(){

  df <- load_parameters_table()
  
  simulation_length<-30
  #set the detection rates equal 
  #rates of detection 
  bc_rdetecti<-sd_rdetecti<-.1
  bc_rdetecta<-sd_rdetecta<-.01
  #set the basecase parameters
  bc_params<-df[1,]
  bc_det_table <- data.frame(
    time = 1:simulation_length,
    rdetecti = rep(bc_rdetecti, simulation_length),
    rdetecta = rep(bc_rdetecta, simulation_length))
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ###Explore the effects of Effectiveness ##### ##### ##### ##### ##### 
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## we will use the "early==day 1" social distancing
  intv_timing_early<-1
  #set intervention parameters to 50% social distancing parameters 
  sd_params<-df[8,]
  # baseline social distancing is 50% (results are in sde_out)
  # will explore social distancing at 25%
  sdhalf_params<-df[8,]
  sdhalf_params$s<-.25
  sde_det_table <- data.frame(
    time = 1:(simulation_length),
    rdetecti = c(rep(bc_rdetecti, intv_timing_early), rep(sd_rdetecti, (simulation_length - intv_timing_early))),
    rdetecta = c(rep(bc_rdetecta, intv_timing_early), rep(sd_rdetecta, (simulation_length - intv_timing_early))))
  
  ### run intervention
  sde_test = run_param_vec(params = bc_params, params2 = sd_params, days_out1 = intv_timing_early,
                           days_out2 = simulation_length, days_out3 = simulation_length, model_type = run_int, det_table = sde_det_table)
  
  sdhalf_test = run_param_vec(params = bc_params, params2 = sdhalf_params, days_out1 = intv_timing_early,
                              days_out2 = simulation_length, model_type = run_int, det_table = sde_det_table)

  ###format the data 
  #format as "out"
  sdhalf_out<-make_out(sdhalf_test, sdhalf_params)
  sde_out<-make_out(sde_test, sde_params)
  ###filter down to total infected (detected and total)
  sde_out_cases = sde_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>% 
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) 
  
  sdhalf_out_cases<-sdhalf_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>%
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time)
  
  #make a single dataframe for ggplot
  sd_effective_cases<-data.frame("time"=sde_out_cases[,1],
                                 "50%"=sde_out_cases[,3],
                                 "25%"=sdhalf_out_cases[,3])
  colnames(sd_effective_cases)<-c("time", "50%", "25%")
  #melt it down
  sd_effective_melt<-melt(sd_effective_cases, id.vars = "time")
  #calculate the ratio of cases with effectiveness
  effective_ratio<-round(as.numeric(sdhalf_out_cases[simulation_length,3])/as.numeric(sde_out_cases[simulation_length,3]),1)

e1 = ggplot(sd_effective_melt, aes(x = time, y = value, group=variable, color=variable)) +
    scale_color_manual(values=c(cspec_pal[1], cspec_pal[3]))+
    geom_line(size=1.5) + theme(legend.position = c(0.1,.32))+
    # geom_dl(aes(label=round(value,0)), method=list(dl.trans(x = x + .2), "last.points")) +
    labs(color="Population in Social Distancing", x = "Time (days)", y = "", 
         title = paste0("Implementing public health measures at half effectiveness results in ", effective_ratio,  " times more cases in 4 weeks.")) +cspec_theme2()

  return(e1)
}

#####make a plot that shows that ending measures early will result in more cases
#This plot shows the case ratio for a simulation that has social distancing of 
#50% of the population. We will start two interventions on day 3 of the simulation
#one will run for 4 weeks and another for 3 weeks. Case counts are evaluated on day 
#100 of the simulation. 
persist_plot<-function(){

  df <- load_parameters_table()
  
  simulation_length<-100 #4 week intervention starting on day 3; allow for ~10 weeks of follow up (mid-june)
  #set the detection rates equal 
  #rates of detection 
  bc_rdetecti<-sd_rdetecti<-.1
  bc_rdetecta<-sd_rdetecta<-.01
  #set the basecase parameters
  bc_params<-df[1,]
  bc_det_table <- data.frame(
    time = 1:simulation_length,
    rdetecti = rep(bc_rdetecti, simulation_length),
    rdetecta = rep(bc_rdetecta, simulation_length))
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ###Explore the effects of Persistence ##### ##### ##### ##### ##### 
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  intv_timing_early<-3
  #assign the parameters for social distancing at 50% of population
  sd_params<-df[8,]

  sde_det_table <- data.frame(
    time = 1:(simulation_length),
    rdetecti = c(rep(bc_rdetecti, intv_timing_early), rep(sd_rdetecti, (simulation_length - intv_timing_early))),
    rdetecta = c(rep(bc_rdetecta, intv_timing_early), rep(sd_rdetecta, (simulation_length - intv_timing_early))))
  
  ### run intervention
  sde_test = run_param_vec(params = bc_params, params2 = sd_params, days_out1 = intv_timing_early,
                           days_out2 = simulation_length, days_out3 = 30, model_type = run_int, det_table = sde_det_table)
  
  sdend_test = run_param_vec(params = bc_params, params2 = sd_params, days_out1 = intv_timing_early,
                             days_out2 = simulation_length, days_out3 = 23, model_type = run_int, det_table = sde_det_table)
  ###format the data 
  #format as "out"
  sdend_out<-make_out(sdend_test, sde_params)
  sde_out<-make_out(sde_test, sde_params)
  ###filter down to total infected (detected and total)
  sde_out_cases = sde_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>% 
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) 
  
  sdend_out_cases<-sdend_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>%
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time)
  
  #make a single dataframe for ggplot
  
  sd_end_cases<-data.frame("time"=sde_out_cases[30:100,1],
                           "earlyend"=sdend_out_cases[30:100,3],
                           "full"=sde_out_cases[30:100,3]
  )
  colnames(sd_end_cases)<-c("time", "3weeks", "4weeks")
  #melt it down
  sd_end_melt<-melt(sd_end_cases, id.vars = "time")
  
  #waffle plot
  waffle_size<-52
  inf_full<-20
  case_ratio<-round(sd_end_cases[nrow(sd_end_cases),2]/sd_end_cases[nrow(sd_end_cases),3],1)
  inf_end<-inf_full*case_ratio
  
  per_waffle<-waffle(
    c(`4 week intervention`=inf_full, `3 week intervention`=inf_end), use_glyph = "user", colors = c(cspec_pal[1],cspec_pal[3]),
    legend_pos = "right", rows=5,title = paste0("Ending public health measures one week earlier will result in ", case_ratio , " times more cases after 10 weeks." ))+cspec_theme_waf()
  
  return(per_waffle)
}
##### make a plot that shows that even when tranmission is slowed, confirmed cases will still rise
#simulation is 30 days long; on day 10 transmission is reduced by 
#either a factor of 4, 3, or completely zeroed out. We create a line plot
#to show the case detection lag.

patience_plot<-function(){
#load in the model parameters
df <- load_parameters_table()
#factor to reduce social distancing by
prob<-c(0,.25,.33)
#x is necessary for accumulating case counts later 
  x<-1:4
  for(i in 1:length(prob)){
    print(paste("running simulation #", i, "of 3"))   
    #run for a total of 30 days
    simulation_length<-30 
    #set the detection rates equal 
    #rates of detection 
    bc_rdetecti<-sd_rdetecti<-.1
    bc_rdetecta<-sd_rdetecta<-.01
#set the base case parameters    
    bc_params<-df[1,]
    bc_det_table <- data.frame(
      time = 1:simulation_length,
      rdetecti = rep(bc_rdetecti, simulation_length),
      rdetecta = rep(bc_rdetecta, simulation_length))
    
    ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    ##### #####  explore the effects of patience   ##### #####
    ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    ###set the timing of the intervention 
    intv_timing_early<-10
    #set the patience scenario parameters to the basecase
    sdp_params<-df[1,]
    #update the transmission probability 
    sdp_params$p<-sdp_params$p*prob[i]
    
    sdp_det_table <- data.frame(
      time = 1:(simulation_length),
      rdetecti = c(rep(bc_rdetecti, intv_timing_early), rep(sd_rdetecti, (simulation_length - intv_timing_early))),
      rdetecta = c(rep(bc_rdetecta, intv_timing_early), rep(sd_rdetecta, (simulation_length - intv_timing_early))))
    ### run model without intervention
    bc_test = run_param_vec(params = bc_params, params2 = NULL, days_out1 = simulation_length,
                            days_out2 = NULL, model_type = run_basic, det_table = bc_det_table)
    ### run intervention 
    sdp_test = run_param_vec(params = bc_params, params2 = sdp_params, days_out1 = intv_timing_early,
                             days_out2 = simulation_length,days_out3 = simulation_length, model_type = run_int, det_table = sdp_det_table)
    ###format the data 
    bc_out<-make_out(bc_test, bc_params)
    sdp_out<-make_out(sdp_test, sdp_params)
    
    ###filter down to total infected (detected and total)
    bc_out_cases = bc_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>%
      summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time)
    sdp_out_cases = sdp_out %>% filter(cum == T & comp!="D") %>% group_by(time, comp3) %>% 
      summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time)
    
    sdp_out_cases2 = sdp_out %>% filter(cum == F & comp!="D") %>% group_by(time, comp3) %>% 
      summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) 
    #combine into a single dataframe
    sd_out_cases<-data.frame("time"=sdp_out_cases[,1],
                             "bc"=bc_out_cases[,3],
                             "non"=sdp_out_cases[,3])
    colnames(sd_out_cases)<-c("time", "Basecase", "NoTransmission")
    
    sd_patience_cases2<-data.frame("time"=sdp_out_cases2[,1],
                                   "DoubleTimeMultiplier"=as.factor(i),
                                   "detected"=as.numeric(as.character(unlist(sdp_out_cases2[,4]))),
                                   "infected"=as.numeric(as.character(unlist(sdp_out_cases2[,4])))+as.numeric(as.character(unlist(sdp_out_cases2[,5])))
    )
    #combine all the different runs together
        x<-rbind(x,sd_patience_cases2)
  }
  #remove the dummy row 
  x<-x[-1,]
  #melt it down
  sd_patience_melt2<-melt(x, id.vars = c("time", "DoubleTimeMultiplier"))
  
  yint<-as.numeric(sd_patience_melt2%>%filter(time==intv_timing_early & variable=="detected" & DoubleTimeMultiplier==1)%>%select(value))
  m<-sd_patience_melt2%>%filter(variable=="detected") %>% group_by(DoubleTimeMultiplier)%>%summarize(maxCases=max(value), maxTime=time[which(value==max(value))])
  
  pat3= ggplot(sd_patience_melt2%>%filter(variable=="detected"), aes(x = time, y = value, group=DoubleTimeMultiplier, color=DoubleTimeMultiplier) )+
    geom_line(size=1.5) + theme(legend.position = c(.75,.1475)) +  
    labs(x="Days ", y="New Confirmed Cases", color="",
         title="Even though measures are working, confirmed cases will still continue to rise before they fall.", fill="")+cspec_theme2()+
    annotate(geom="text", label="start reduction measures",color=cspec_pal[1], fontface=2, x=intv_timing_early-5,y=yint+3, size=5.5)+
    geom_segment(x = intv_timing_early-5, y = yint, xend = intv_timing_early-.2, yend = yint,
                 arrow = arrow(length = unit(.4, "cm")), size=1.2, color=cspec_pal[1])+
    geom_point(data=m, aes(x=maxTime, y=maxCases, fill="Maximum Confirmed Cases"), size=3.5, show.legend = TRUE)+
    scale_color_manual(labels=c("Zero disease transmission", 
                                "25% of current disease transmission rate", 
                                "33% of current disease transmission rate",
                                "Maximum Cases Reached"),values=c("grey35", cspec_pal[1],cspec_pal[3], cspec_pal[2]))+theme(    legend.margin = margin(0, 0, 0, 0))

  return(pat3)
}

#####combine all plots and text into the infographic 
#create the plots
time<-timing_plot()
effect<-effective_plot()
persist<-persist_plot()
patience<-patience_plot()

# Generate Infographic in PNG Format
png("~/Desktop/cspec_infographic2.png", width = 12, height = 22, units = "in", res = 500)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(nrow=9, ncol=4, 
                                           heights =c(5,1, 5,1, 5,1,5,1.2,5))))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.rect(gp = gpar(fill = cspec_pal[4], col = cspec_pal[4]))
grid.text("COVID-19", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Impact", col = cspec_pal[3], cex = 12))
grid.text("action plan", y = unit(0.92, "npc"), gp = gpar(fontfamily = "Impact", col = cspec_pal[1], cex = 6.4))
grid.text("by COVID-19 Statistics, Policy Modeling, and Epidemiology Collective", vjust = 0, y = unit(0.89, "npc"), gp = gpar(fontfamily = "Impact", col = cspec_pal[2], cex = 1))
grid.text("http://covid-spec.org/", vjust = 0, y = unit(0.881, "npc"), gp = gpar(fontfamily = "Impact", col = cspec_pal[2], cex = 1))

####line break 
grid.text("it's on you to...", y = unit(0.86, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = cspec_pal[3], cex = 5))

#first plot
print(time, vp = vplayout(3, 1:4))
#second plot
print(effect, vp = vplayout(5, 1:4))
#third plot
print(persist, vp = vplayout(7, 1:3))
#fourth plot 
print(patience, vp = vplayout(9, 1:4))
#text block one 
grid.rect(gp = gpar(fill = cspec_pal[1], col = cspec_pal[1]), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.035, "npc"))
grid.text("...BE IMMEDIATE", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = cspec_pal[3], cex = 4))
#text block two 
grid.rect(gp = gpar(fill = cspec_pal[1], col = cspec_pal[1]), x = unit(0.5, "npc"), y = unit(.605, "npc"), width = unit(1, "npc"), height = unit(0.035, "npc"))
grid.text("...BE DEFINITIVE", y = unit(.605, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = cspec_pal[3], cex = 4))
#text block three 
grid.rect(gp = gpar(fill = cspec_pal[1], col = cspec_pal[1]), x = unit(0.5, "npc"), y = unit(.40, "npc"), width = unit(1, "npc"), height = unit(0.035, "npc"))
grid.text("...BE PERSISTENT", y = unit(.40, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = cspec_pal[3], cex = 4))
#text block four
grid.rect(gp = gpar(fill = cspec_pal[1], col = cspec_pal[1]), x = unit(0.5, "npc"), y = unit(0.195, "npc"), width = unit(1, "npc"), height = unit(0.034, "npc"))
grid.text("...BE PATIENT", y = unit(0.195, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = cspec_pal[3], cex = 4))

dev.off()

