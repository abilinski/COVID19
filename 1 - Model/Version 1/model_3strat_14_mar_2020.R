#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
# By: Alyssa Bilinski                                                                      #
# Created: March 13, 2020                                                                  #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#

#### DO NOT USE TIDY VERSE 
#### IF WE DO IT CANNOT GO ON CLUSTER

############## STRATIFIED TB MODEL
model_strat <- function (t, x, params) {
  # attach initial conditions into workspace
  attach(x)
  
  # attach parameters into workspace
  attach(params)
  
  # set up for equations
    
    # age
    age = c(1:3)
    
    # socially isolated
    q = c("", "Q")
    
    # make compartments
    comps = data.frame(expand.grid(age, q)) 
    group = paste(comps$Var1, comps$Var2, sep = "")
    
  ###### Equations
  make_eqn_strat = function(group){
   S = paste("dS", group, "dt = -S", group,
             "*p*(v1", group, "*I1 + vA1", group, "*A1 + v2", group, 
             "*I2 + vA2", group, "*A2 + v3", group, "*I3 + vA3", group, "*A3 + v1Q", group, 
             "*I1Q + vA1Q", group, "*A1Q + v2Q", group, "*I2Q + vA2Q", group, "*A2Q + v3Q", group, 
             "*I3Q + vA3Q", group, "*A3Q)", 
             sep = "")

   E = paste("dE", group, "dt = -delta*E", group, "+ S", group, 
             "*p*(v1", group, "*I1 + vA1", group, "*A1 + v2", group, 
             "*I2 + vA2", group, "*A2 + v3", group, "*I3 + vA3", group, "*A3 + v1Q", group, 
             "*I1Q + vA1Q", group, "*A1Q + v2Q", group, "*I2Q + vA2Q", group, "*A2Q + v3Q", group, 
             "*I3Q + vA3Q", group, "*A3Q)",
             sep = "")
   
   A = paste("dA", group, "dt = delta*E", group, "-(1-alpha", group, ")*epsilon*A", group, "-gamma*A", group, sep = "") 
   
  
   I = paste("dI", group, "dt = (1-alpha", group, ")*epsilon*A", group, "- (q", group, "*(m", group,
   "*omega + (1-m", group, ")*gamma) - (1-q", group, 
   ")*(mH", group, "*omega + (1-mH", group, ")*gamma))*I", group, sep = "")
   
   R = paste("dR", group, "dt = gamma*A", group, "+ (q", group, "*(1-m", group, ")*gamma + (1-q", 
             group, ")*(1-mH", group, ")*gamma)*I", group, sep = "")
   
   return(c(S, E, I, A, R))

  }
  
  # results
  output <- c(dS1dt, dE1dt, dI1dt, dR1dt,
              dS2dt, dE2dt, dI2dt, dR2dt,
              dS3dt, dE3dt, dI3dt, dR3dt,
              dS1Qdt, dE1Qdt, dI1Qdt, dR1Qdt,
              dS2Qdt, dE2Qdt, dI2Qdt, dR2Qdt,
              dS3Qdt, dE3Qdt, dI3Qdt, dR3Qdt)
  
  # list it!
  list(output)
}

############## RUN ODE
run_model <- function(func, xstart, times, params, method = "lsodes") {
  return(as.data.frame(ode(func = func, y = xstart, times = times, parms = params, method = method)))
}
