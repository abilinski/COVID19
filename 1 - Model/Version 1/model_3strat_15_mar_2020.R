#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
# By: Alyssa Bilinski                                                                      #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#

############## STRATIFIED TB MODEL
model_strat <- function (t, x, params) {
 
  # attach initial conditions into workspace
  attach(x)
  
  # attach parameters into workspace
  attach(params)
  
  ###### Equations
  ### YOUNG
  dS1dt = -S1*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 + v31*I3 + vA31*A3 +
                   v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)
  dE1dt = -delta*E1+ S1*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 +  v31*I3 + vA31*A3 + 
                             v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)
  dI1dt = (1-alpha1)*epsilon*A1- (q1*(m1*omega + (1-m1)*gamma) - (1-q1)*(mH1*omega + (1-mH1)*gamma))*I1
  dA1dt = delta*E1-(1-alpha1)*epsilon*A1-gamma*A1
  dR1dt = gamma*A1+ (q1*(1-m1)*gamma - (1-q1)*(1-mH1)*gamma)*I1
  
  ### MEDIUM
  dS2dt = -S2*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 + v31*I3 + vA31*A3 + v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)          
  dE2dt = -delta*E2+ S2*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 +  v31*I3 + vA31*A3 +v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)
  dI2dt = (1-alpha2)*epsilon*A2- (q2*(m2*omega + (1-m2)*gamma) - (1-q2)*(mH2*omega + (1-mH2)*gamma))*I2                                                
  dA2dt = delta*E2-(1-alpha2)*epsilon*A2-gamma*A2                                                                                                      
  dR2dt = gamma*A2+ (q2*(1-m2)*gamma - (1-q2)*(1-mH2)*gamma)*I2
  
  ### OLD
  dS3dt = -S3*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 + v31*I3 + vA31*A3 + v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)          
  dE3dt = -delta*E3+ S3*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 +  v31*I3 + vA31*A3 +v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)
  dI3dt = (1-alpha3)*epsilon*A3- (q3*(m3*omega + (1-m3)*gamma) - (1-q3)*(mH3*omega + (1-mH3)*gamma))*I3                                                
  dA3dt = delta*E3-(1-alpha3)*epsilon*A3-gamma*A3                                                                                                      
  dR3dt = gamma*A3+ (q3*(1-m3)*gamma - (1-q3)*(1-mH3)*gamma)*I3     
  
  ### YOUNG - SOCIALLY DISTANCED
  dS1Qdt = -S1Q*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 + v31*I3 + vA31*A3 + v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)           
  dE1Qdt = -delta*E1Q+ S1Q*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 +  v31*I3 + vA31*A3 +v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)
  dI1Qdt = (1-alpha1Q)*epsilon*A1Q- (q1Q*(m1Q*omega + (1-m1Q)*gamma) - (1-q1Q)*(mH1Q*omega + (1-mH1Q)*gamma))*I1Q                                         
  dA1Qdt = delta*E1Q-(1-alpha1Q)*epsilon*A1Q-gamma*A1Q                                                                                                    
  dR1Qdt = gamma*A1Q+ (q1Q*(1-m1Q)*gamma - (1-q1Q)*(1-mH1Q)*gamma)*I1Q 
  
  ### MEDIUM - SOCIALLY DISTANCED
  dS2Qdt = -S2Q*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 + v31*I3 + vA31*A3 + v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)           
  dE2Qdt = -delta*E2Q+ S2Q*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 +  v31*I3 + vA31*A3 +v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)
  dI2Qdt = (1-alpha2Q)*epsilon*A2Q- (q2Q*(m2Q*omega + (1-m2Q)*gamma) - (1-q2Q)*(mH2Q*omega + (1-mH2Q)*gamma))*I2Q                                         
  dA2Qdt = delta*E2Q-(1-alpha2Q)*epsilon*A2Q-gamma*A2Q                                                                                                    
  dR2Qdt = gamma*A2Q+ (q2Q*(1-m2Q)*gamma - (1-q2Q)*(1-mH2Q)*gamma)*I2Q
  
  ### OLD - SOCIALLY DISTANCED
  dS3Qdt = -S3Q*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 + v31*I3 + vA31*A3 + v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)           
  E3Qdt = -delta*E3Q+ S3Q*p*(v11*I1 + vA11*A1 + v21*I2 + vA21*A2 +  v31*I3 + vA31*A3 +v1Q1*I1Q + vA1Q1*A1Q + v2Q1*I2Q + vA2Q1*A2Q + v3Q1*I3Q + vA3Q1*A3Q)
  dI3Qdt = (1-alpha3Q)*epsilon*A3Q- (q3Q*(m3Q*omega + (1-m3Q)*gamma) - (1-q3Q)*(mH3Q*omega + (1-mH3Q)*gamma))*I3Q                                         
  dA3Qdt = delta*E3Q-(1-alpha3Q)*epsilon*A3Q-gamma*A3Q                                                                                                    
  dR3Qdt = gamma*A3Q+ (q3Q*(1-m3Q)*gamma - (1-q3Q)*(1-mH3Q)*gamma)*I3Q
  
  # results
  output <- c(dS1dt, dE1dt, dI1dt, dA1dt, dR1dt,
              dS2dt, dE2dt, dI2dt, dA2dt, dR2dt,
              dS3dt, dE3dt, dI3dt, dA3dt, dR3dt,
              dS1Qdt, dE1Qdt, dI1Qdt, dA1Qdt, dR1Qdt,
              dS2Qdt, dE2Qdt, dI2Qdt, dA2Qdt, dR2Qdt,
              dS3Qdt, dE3Qdt, dI3Qdt, dA2Qdt, dR3Qdt)
  
  # list it!
  list(output)
}

############## RUN ODE
run_model <- function(func, xstart, times, params, method = "lsodes") {
  return(as.data.frame(ode(func = func, y = xstart, times = times, parms = params, method = method)))
}
