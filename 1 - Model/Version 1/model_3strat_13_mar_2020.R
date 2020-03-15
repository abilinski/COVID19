#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
# By: Alyssa Bilinski                                                                      #
# Created: March 13, 2020                                                                  #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#

############## STRATIFIED TB MODEL
model_strat <- function (t, x, params) {
  # state variables
  S1 <- x[1]
  E1 <- x[2]
  I1 <- x[4]
  A1 <- 
  R1 <- x[5]
  
  S2 <- x[6]
  E2 <- x[7]
  I2 <- x[9]
  A2 <- 
  R2 <- x[10]
  
  S3 <- x[11]
  E3 <- x[12]
  I3 <- x[13]
  A3 <- 
  R3 <- x[14]
  
  S1Q <- x[15]
  E1Q <- x[16]
  I1Q <- x[17]
  A1Q <- 
  R1Q <- x[18]
  
  S2Q <- x[19]
  E2Q <- x[20]
  I2Q <- x[21]
  A2Q <- 
  R2Q <- x[22]
  
  S3Q <- x[23]
  E3Q <- x[24]
  I3Q <- x[25]
  A3Q <- 
  R3Q <- x[26]
  
  
  # parameters
  
  # contact rates
  # vij = from i INFECTED to j SUSCEPTIBLE
  v11 = params$value[params$var=="v11"]
  v12 = params$value[params$var=="v12"]
  v13 = params$value[params$var=="v13"]
  v11Q = params$value[params$var=="v11Q"]
  v12Q = params$value[params$var=="v12Q"]
  v13Q = params$value[params$var=="v13Q"]
  
  v21 = params$value[params$var=="v21"]
  v22 = params$value[params$var=="v22"]
  v23 = params$value[params$var=="v23"]
  v21Q = params$value[params$var=="v21Q"]
  v22Q = params$value[params$var=="v22Q"]
  v23Q = params$value[params$var=="v23Q"]
  
  v31 = params$value[params$var=="v31"]
  v32 = params$value[params$var=="v32"]
  v33 = params$value[params$var=="v33"]
  v31Q = params$value[params$var=="v31Q"]
  v32Q = params$value[params$var=="v32Q"]
  v33Q = params$value[params$var=="v33Q"]
  
  v1Q1 = params$value[params$var=="v1Q1"]
  v1Q2 = params$value[params$var=="v1Q2"]
  v1Q3 = params$value[params$var=="v1Q3"]
  v1Q1Q = params$value[params$var=="v1Q1Q"]
  v1Q2Q = params$value[params$var=="v1Q2Q"]
  v1Q3Q = params$value[params$var=="v1Q3Q"]
  
  v2Q1 = params$value[params$var=="v2Q1"]
  v2Q2 = params$value[params$var=="v2Q2"]
  v2Q3 = params$value[params$var=="v2Q3"]
  v2Q1Q = params$value[params$var=="v2Q1Q"]
  v2Q2Q = params$value[params$var=="v2Q2Q"]
  v2Q3Q = params$value[params$var=="v2Q3Q"]
  
  v3Q1 = params$value[params$var=="v3Q1"]
  v3Q2 = params$value[params$var=="v3Q2"]
  v3Q3 = params$value[params$var=="v3Q3"]
  v3Q1Q = params$value[params$var=="v3Q1Q"]
  v3Q2Q = params$value[params$var=="v3Q2Q"]
  v3Q3Q = params$value[params$var=="v3Q3Q"]

  # probability transmission in susceptible stratum i given 
  # contacted with infected
  p1 = params$value[params$var=="p1"]
  p2 = params$value[params$var=="p2"]
  p3 = params$value[params$var=="p3"]
  p1Q = params$value[params$var=="p1Q"]
  p2Q = params$value[params$var=="p2Q"]
  p3Q = params$value[params$var=="p3Q"]
  
  # proportion of asymptomatic cases in group i
  alpha1 = params$value[params$var=="alpha1"]
  alpha2 = params$value[params$var=="alpha2"]
  alpha3 = params$value[params$var=="alpha3"]
  alpha1Q = params$value[params$var=="alpha1Q"]
  alpha2Q = params$value[params$var=="alpha2Q"]
  alpha3Q = params$value[params$var=="alpha3Q"]
  
  # proportion of group that can receive w/health care
  q1 = params$value[params$var=="q1"]
  q2 = params$value[params$var=="q2"]
  q3 = params$value[params$var=="q3"]
  q1Q = params$value[params$var=="q1Q"]
  q2Q = params$value[params$var=="q2Q"]
  q3Q = params$value[params$var=="q3Q"]
  
  # age specific mortality probability w/healthcare
  m1 = params$value[params$var=="m1"]
  m2 = params$value[params$var=="m2"]
  m3 = params$value[params$var=="m3"]
  m1Q = params$value[params$var=="m1Q"]
  m2Q = params$value[params$var=="m2Q"]
  m3Q = params$value[params$var=="m3Q"]
  
  # age specific mortality probability above healthcare capacity constraint
  mH1 = params$value[params$var=="m1"]
  mH2 = params$value[params$var=="m2"]
  mH3 = params$value[params$var=="m3"]
  mH1Q = params$value[params$var=="m1Q"]
  mH2Q = params$value[params$var=="m2Q"]
  mH3Q = params$value[params$var=="m3Q"]
  
  # other parameters
  kappa = params$value[params$var=="kappa"]     # reduction in transmission probability when asymptomatic
  delta = params$value[params$var=="delta"]     # 1/incubation period
  gamma = params$value[params$var=="gamma"]     # 1/duration of infection to recovery
  lambda = params$value[params$var=="lambda"]     # 1/duration of infection to death
  
  ###### Equations
  ### YOUNG
  dS1dt = -S1*p1*(v11*I1*((1-kappa) + kappa*alpha1)+
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
  dR1dt = (q1*(1-m1)*gamma + (1-q1)*(1-mH1)*gamma)*I1
  
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
