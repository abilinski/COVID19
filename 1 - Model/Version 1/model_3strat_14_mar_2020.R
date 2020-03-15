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
    paste("dS", group, "dt = -S1*p1*(v11*I1*((1-kappa) + kappa*alpha1)+
                    v21*I2*((1-kappa) + kappa*alpha2)+
                    v31*I3*((1-kappa) + kappa*alpha3)+
                    v1Q1*I1Q*((1-kappa) + kappa*alpha1Q)+
                    v2Q1*I2Q*((1-kappa) + kappa*alpha2Q)+
                    v3Q1*I3Q*((1-kappa) + kappa*alpha3Q))
  dE1dt = S1*p1*(v11*I1*((1-kappa) + kappa*alpha1)+
                    v21*I2*((1-kappa) + kappa*alpha2)+
                    v31*I3*((1-kappa) + kappa*alpha3)+
                    v1Q1*I1Q*((1-kappa) + kappa*alpha1Q)+
                    v2Q1*I2Q*((1-kappa) + kappa*alpha2Q)+
                    v3Q1*I3Q*((1-kappa) + kappa*alpha3Q)) - delta*E1
  dI1dt = delta*E1 - (q1*(m1*lambda+(1-m1)*gamma) - (1-q1)*(mH1*lambda + (1-mH1)*gamma))*I1
  dR1dt = (q1*(1-m1)*gamma + (1-q1)*(1-mH1)*gamma)*I1", sep = "")
  }
  
  ### MEDIUM
  dS2dt = -S1*p1*(v12*I1*((1-kappa) + kappa*alpha1)+
                    v22*I2*((1-kappa) + kappa*alpha2)+
                    v32*I3*((1-kappa) + kappa*alpha3)+
                    v1Q2*I1Q*((1-kappa) + kappa*alpha1Q)+
                    v2Q2*I2Q*((1-kappa) + kappa*alpha2Q)+
                    v3Q2*I3Q*((1-kappa) + kappa*alpha3Q))
  dE2dt = S1*p1*(v12*I1*((1-kappa) + kappa*alpha1)+
                   v22*I2*((1-kappa) + kappa*alpha2)+
                   v32*I3*((1-kappa) + kappa*alpha3)+
                   v1Q2*I1Q*((1-kappa) + kappa*alpha1Q)+
                   v2Q2*I2Q*((1-kappa) + kappa*alpha2Q)+
                   v3Q2*I3Q*((1-kappa) + kappa*alpha3Q)) - delta*E2
  dI2dt = delta*E2 - (q2*(m2*lambda+(1-m2)*gamma) - (1-q2)*(mH2*lambda + (1-mH2)*gamma))*I2
  dR2dt = (q2*(1-m2)*gamma + (1-q2)*(1-mH2)*gamma)*I2
  
  ### OLD
  dS3dt = -S1*p1*(v13*I1*((1-kappa) + kappa*alpha1)+
                    v23*I2*((1-kappa) + kappa*alpha2)+
                    v33*I3*((1-kappa) + kappa*alpha3)+
                    v1Q3*I1Q*((1-kappa) + kappa*alpha1Q)+
                    v2Q3*I2Q*((1-kappa) + kappa*alpha2Q)+
                    v3Q3*I3Q*((1-kappa) + kappa*alpha3Q))
  dE3dt = S1*p1*(v13*I1*((1-kappa) + kappa*alpha1)+
                   v23*I2*((1-kappa) + kappa*alpha2)+
                   v33*I3*((1-kappa) + kappa*alpha3)+
                   v1Q3*I1Q*((1-kappa) + kappa*alpha1Q)+
                   v2Q3*I2Q*((1-kappa) + kappa*alpha2Q)+
                   v3Q3*I3Q*((1-kappa) + kappa*alpha3Q)) - delta*E3
  dI3dt = delta*E3 - (q3*(m3*lambda+(1-m3)*gamma) - (1-q3)*(mH3*lambda + (1-mH3)*gamma))*I3
  dR3dt = (q3*(1-m3)*gamma + (1-q3)*(1-mH3)*gamma)*I3
  
  ### YOUNG - Q
  dS1Qdt = -S1*p1*(v12*I1*((1-kappa) + kappa*alpha1)+
                    v21Q*I2*((1-kappa) + kappa*alpha2)+
                    v31Q*I3*((1-kappa) + kappa*alpha3)+
                    v1Q1Q*I1Q*((1-kappa) + kappa*alpha1Q)+
                    v2Q1Q*I2Q*((1-kappa) + kappa*alpha2Q)+
                    v3Q1Q*I3Q*((1-kappa) + kappa*alpha3Q))
  dE1Qdt = S1*p1*(v11Q*I1*((1-kappa) + kappa*alpha1)+
                   v21Q*I2*((1-kappa) + kappa*alpha2)+
                   v31Q*I3*((1-kappa) + kappa*alpha3)+
                   v1Q1Q*I1Q*((1-kappa) + kappa*alpha1Q)+
                   v2Q1Q*I2Q*((1-kappa) + kappa*alpha2Q)+
                   v3Q1Q*I3Q*((1-kappa) + kappa*alpha3Q)) - delta*E1Q
  dI1Qdt = delta*E1Q - (q1Q*(m1Q*lambda+(1-m1Q)*gamma) - (1-q1Q)*(mH1Q*lambda + (1-mH1Q)*gamma))*I1Q
  dR1Qdt = (q1Q*(1-m1Q)*gamma + (1-q1Q)*(1-mH1Q)*gamma)*I1Q
    
  ### MEDIUM - Q
  dS2Qdt = -S1*p1*(v12Q*I1*((1-kappa) + kappa*alpha1)+
                    v22Q*I2*((1-kappa) + kappa*alpha2)+
                    v32Q*I3*((1-kappa) + kappa*alpha3)+
                    v1Q2Q*I1Q*((1-kappa) + kappa*alpha1Q)+
                    v2Q2Q*I2Q*((1-kappa) + kappa*alpha2Q)+
                    v3Q2Q*I3Q*((1-kappa) + kappa*alpha3Q))
  dE2Qdt = S1*p1*(v12Q*I1*((1-kappa) + kappa*alpha1)+
                   v22Q*I2*((1-kappa) + kappa*alpha2)+
                   v32Q*I3*((1-kappa) + kappa*alpha3)+
                   v1Q2Q*I1Q*((1-kappa) + kappa*alpha1Q)+
                   v2Q2Q*I2Q*((1-kappa) + kappa*alpha2Q)+
                   v3Q2Q*I3Q*((1-kappa) + kappa*alpha3Q)) - delta*E2Q
  dI2QQdt = delta*E2Q - (q2Q*(m2Q*lambda+(1-m2Q)*gamma) - (1-q2Q)*(mH2Q*lambda + (1-mH2Q)*gamma))*I2Q
  dR2QQdt = (q2Q*(1-m2Q)*gamma + (1-q2Q)*(1-mH2Q)*gamma)*I2Q
  
  ### OLD - Q
  dS3Qdt = -S1*p1*(v13Q*I1*((1-kappa) + kappa*alpha1)+
                    v23Q*I2*((1-kappa) + kappa*alpha2)+
                    v33Q*I3*((1-kappa) + kappa*alpha3)+
                    v1Q3Q*I1Q*((1-kappa) + kappa*alpha1Q)+
                    v2Q3Q*I2Q*((1-kappa) + kappa*alpha2Q)+
                    v3Q3Q*I3Q*((1-kappa) + kappa*alpha3Q))
  dE3Qdt = S1*p1*(v13Q*I1*((1-kappa) + kappa*alpha1)+
                   v23Q*I2*((1-kappa) + kappa*alpha2)+
                   v33Q*I3*((1-kappa) + kappa*alpha3)+
                   v1Q3Q*I1Q*((1-kappa) + kappa*alpha1Q)+
                   v2Q3Q*I2Q*((1-kappa) + kappa*alpha2Q)+
                   v3Q3Q*I3Q*((1-kappa) + kappa*alpha3Q)) - delta*E3Q
  dI3Qdt = delta*E3Q - (q3Q*(m3Q*lambda+(1-m3Q)*gamma) - (1-q3Q)*(mH3Q*lambda + (1-mH3Q)*gamma))*I3Q
  dR3Qdt = (q3Q*(1-m3Q)*gamma + (1-q3Q)*(1-mH3Q)*gamma)*I3Q
  
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
