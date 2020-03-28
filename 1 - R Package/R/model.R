#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#


############## STRATIFIED MODEL-------------
#' Stratified Model
#' 
#' @export
model_strat <- function (t, x, parms, parms_int, time_int, det_input_method='input',det_table=NULL) {
  #if using parameters in params to set up vary detection rate, det_input_method="calc"
  #if using input table directly to set up detection rate for each time step, det_input_method="input"

  # decide if intervention starts
  ifelse(t>=time_int, parms<-parms_int, parms)
  
  # initial conditions
  S1 = x[1]; E1 = x[2]; UI1 = x[3]; DI1 = x[4]; UA1 = x[5]; DA1 = x[6]; R1 = x[7]
  S2 = x[8]; E2 = x[9]; UI2 = x[10]; DI2 = x[11]; UA2 = x[12]; DA2 = x[13]; R2 = x[14]
  S3 = x[15]; E3 = x[16]; UI3 = x[17]; DI3 = x[18]; UA3 = x[19]; DA3 = x[20]; R3 = x[21]
  S1Q = x[22]; E1Q = x[23]; UI1Q = x[24]; DI1Q = x[25]; UA1Q = x[26]; DA1Q = x[27]; R1Q = x[28]
  S2Q = x[29]; E2Q = x[30]; UI2Q = x[31]; DI2Q = x[32]; UA2Q = x[33]; DA2Q = x[34]; R2Q = x[35]
  S3Q = x[36]; E3Q = x[37]; UI3Q = x[38]; DI3Q = x[39]; UA3Q = x[40]; DA3Q = x[41]; R3Q = x[42]
  
  # pull in params
  # pulled from: paste(names(params), " = parms$", names(params), sep = "", collapse = ";")
  # in case params change
  # could be cleaner
  v11 = parms$v11;v12 = parms$v12;v13 = parms$v13;v21 = parms$v21;v22 = parms$v22
  v23 = parms$v23;v31 = parms$v31;v32 = parms$v32;v33 = parms$v33;s = parms$s
  e = parms$e;p = parms$p;R0 = parms$R0;kappa = parms$kappa;alpha1 = parms$alpha1
  alpha2 = parms$alpha2;alpha3 = parms$alpha3;epsilon = parms$epsilon;delta = parms$delta
  gamma = parms$gamma;m1 = parms$m1;m2 = parms$m2;m3 = parms$m3;c = parms$c
  obs = parms$obs;e_ratio = parms$e_ratio;k_report = parms$k_report;k_inf = parms$k_inf
  k_susp = parms$k_susp;omega = parms$omega;alpha1Q = parms$alpha1Q;alpha2Q = parms$alpha2Q
  alpha3Q = parms$alpha3Q;m1Q = parms$m1Q;m2Q = parms$m2Q;m3Q = parms$m3Q;v11Q = parms$v11Q
  v1Q1 = parms$v1Q1;v1Q1Q = parms$v1Q1Q;v12Q = parms$v12Q;v1Q2 = parms$v1Q2;v1Q2Q = parms$v1Q2Q
  v13Q = parms$v13Q;v1Q3 = parms$v1Q3;v1Q3Q = parms$v1Q3Q;v21Q = parms$v21Q;v2Q1 = parms$v2Q1
  v2Q1Q = parms$v2Q1Q;v22Q = parms$v22Q;v2Q2 = parms$v2Q2;v2Q2Q = parms$v2Q2Q;v23Q = parms$v23Q
  v2Q3 = parms$v2Q3;v2Q3Q = parms$v2Q3Q;v31Q = parms$v31Q;v3Q1 = parms$v3Q1;v3Q1Q = parms$v3Q1Q
  v32Q = parms$v32Q;v3Q2 = parms$v3Q2;v3Q2Q = parms$v3Q2Q;v33Q = parms$v33Q;v3Q3 = parms$v3Q3
  v3Q3Q = parms$v3Q3Q;vA11 = parms$vA11;vA12 = parms$vA12;vA13 = parms$vA13;vA11Q = parms$vA11Q
  vA12Q = parms$vA12Q;vA13Q = parms$vA13Q;vA21 = parms$vA21;vA22 = parms$vA22;vA23 = parms$vA23
  vA21Q = parms$vA21Q;vA22Q = parms$vA22Q;vA23Q = parms$vA23Q;vA31 = parms$vA31;vA32 = parms$vA32
  vA33 = parms$vA33;vA31Q = parms$vA31Q;vA32Q = parms$vA32Q;vA33Q = parms$vA33Q;vA1Q1 = parms$vA1Q1
  vA1Q2 = parms$vA1Q2;vA1Q3 = parms$vA1Q3;vA1Q1Q = parms$vA1Q1Q;vA1Q2Q = parms$vA1Q2Q
  vA1Q3Q = parms$vA1Q3Q;vA2Q1 = parms$vA2Q1;vA2Q2 = parms$vA2Q2;vA2Q3 = parms$vA2Q3
  vA2Q1Q = parms$vA2Q1Q;vA2Q2Q = parms$vA2Q2Q;vA2Q3Q = parms$vA2Q3Q;vA3Q1 = parms$vA3Q1
  vA3Q2 = parms$vA3Q2;vA3Q3 = parms$vA3Q3;vA3Q1Q = parms$vA3Q1Q;vA3Q2Q = parms$vA3Q2Q
  vA3Q3Q = parms$vA3Q3Q;N1 = parms$N1;N2 = parms$N2;N3 = parms$N3;N1Q = parms$N1Q;N2Q = parms$N2Q
  N3Q = parms$N3Q
  
  ###time varying detection rate
  if (det_input_method == "calc") {
    #amended to detection rate directly: initial detection rate for I (det_ini), increasing p by time step (det_inc), a multiplier <1 to represent less detection rate for A (k_det_a); for calibration, use det_ini=0.1, det_inc=0, k_det_a=0 now.
    #for symptomatic
    #assuming a cap detection rate of 0.2 (100% detect probability * 1/5 days to detection, not including competing risk of recovery for this estimation now)
    rdetecti <- min(parms$det_ini * (1 + parms$det_inc*(t-1)), 0.2)
    #for asymptomatic
    #assuming detection rate will continue to grow in asymptomatic after symptomatic reached 0.2
    rdetecta <- min(parms$k_det_a * parms$det_ini * (1 + parms$det_inc*(t-1)), 0.2)
  } else { # input directly from a table
    rdetecti <- det_table[t,'rdetecti']
    rdetecta <- det_table[t,'rdetecta']
  }
  
  #multiplier representing the change in contacts when detected
  k_det_c <- parms$k_det_c
  
  ###### Equations
  ### YOUNG
  dS1dt = -S1*k_susp*p*(k_inf*v11*(UI1+k_det_c*DI1)/N1 + vA11*(UA1+k_det_c*DA1)/N1 + v21*(UI2+k_det_c*DI2)/N2 + vA21*(UA2+k_det_c*DA2)/N2 + v31*(UI3+k_det_c*DI3)/N3 + vA31*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q1*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q1*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q1*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q1*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q1*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q1*(UA3Q+k_det_c*DA3Q)/N3Q)
  dE1dt = -delta*E1+ S1*k_susp*p*(k_inf*v11*(UI1+k_det_c*DI1)/N1 + vA11*(UA1+k_det_c*DA1)/N1 + v21*(UI2+k_det_c*DI2)/N2 + vA21*(UA2+k_det_c*DA2)/N2 + v31*(UI3+k_det_c*DI3)/N3 + vA31*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q1*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q1*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q1*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q1*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q1*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q1*(UA3Q+k_det_c*DA3Q)/N3Q)
  dUI1dt = (1-alpha1)*delta*E1 - (k_report*rdetecti + m1*omega + (1-m1)*gamma)*UI1
  dDI1dt = k_report*rdetecti*UI1 - (m1*omega + (1-m1)*gamma)*DI1
  dUA1dt = alpha1*delta*E1 - (k_report*rdetecta + gamma)*UA1
  dDA1dt = k_report*rdetecta*UA1 - gamma*DA1
  dR1dt = (1-m1)*gamma*(UI1+DI1) + gamma*(UA1+DA1)
  It1 = (1-alpha1)*delta*E1
  DIt1 = k_report*rdetecti*UI1
  At1 = alpha1*delta*E1
  DAt1 = k_report*rdetecta*UA1
  Dt1 = m1*omega*(UI1+DI1)
  
  ### MEDIUM
  dS2dt = -S2*p*(k_inf*v12*(UI1+k_det_c*DI1)/N1 + vA12*(UA1+k_det_c*DA1)/N1 + v22*(UI2+k_det_c*DI2)/N2 + vA22*(UA2+k_det_c*DA2)/N2 + v32*(UI3+k_det_c*DI3)/N3 + vA32*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q2*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q2*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q2*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q2*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q2*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q2*(UA3Q+k_det_c*DA3Q)/N3Q)
  dE2dt = -delta*E2+ S2*p*(k_inf*v12*(UI1+k_det_c*DI1)/N1 + vA12*(UA1+k_det_c*DA1)/N1 + v22*(UI2+k_det_c*DI2)/N2 + vA22*(UA2+k_det_c*DA2)/N2 + v32*(UI3+k_det_c*DI3)/N3 + vA32*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q2*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q2*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q2*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q2*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q2*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q2*(UA3Q+k_det_c*DA3Q)/N3Q)
  dUI2dt = (1-alpha2)*delta*E2 - (rdetecti + m2*omega + (1-m2)*gamma)*UI2
  dDI2dt = rdetecti*UI2 - (m2*omega + (1-m2)*gamma)*DI2
  dUA2dt = alpha2*delta*E2 - (rdetecta + gamma)*UA2
  dDA2dt = rdetecta*UA2 - gamma*DA2
  dR2dt = (1-m2)*gamma*(UI2+DI2) + gamma*(UA2+DA2)
  It2 = (1-alpha2)*delta*E2
  DIt2 = rdetecti*UI2
  At2 = alpha2*delta*E2
  DAt2 = rdetecta*UA2
  Dt2 = m2*omega*(UI2+DI2)
  
  ### OLD
  dS3dt = -S3*p*(k_inf*v13*(UI1+k_det_c*DI1)/N1 + vA13*(UA1+k_det_c*DA1)/N1 + v23*(UI2+k_det_c*DI2)/N2 + vA23*(UA2+k_det_c*DA2)/N2 + v33*(UI3+k_det_c*DI3)/N3 + vA33*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q3*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q3*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q3*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q3*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q3*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q3*(UA3Q+k_det_c*DA3Q)/N3Q)
  dE3dt = -delta*E3+ S3*p*(k_inf*v13*(UI1+k_det_c*DI1)/N1 + vA13*(UA1+k_det_c*DA1)/N1 + v23*(UI2+k_det_c*DI2)/N2 + vA23*(UA2+k_det_c*DA2)/N2 + v33*(UI3+k_det_c*DI3)/N3 + vA33*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q3*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q3*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q3*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q3*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q3*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q3*(UA3Q+k_det_c*DA3Q)/N3Q)
  dUI3dt = (1-alpha3)*delta*E3 - (rdetecti + m3*omega + (1-m3)*gamma)*UI3
  dDI3dt = rdetecti*UI3 - (m3*omega + (1-m3)*gamma)*DI3
  dUA3dt = alpha3*delta*E3 - (rdetecta + gamma)*UA3
  dDA3dt = rdetecta*UA3 - gamma*DA3
  dR3dt = (1-m3)*gamma*(UI3+DI3) + gamma*(UA3+DA3)
  It3 = (1-alpha3)*delta*E3
  DIt3 = rdetecti*UI3
  At3 = alpha3*delta*E3
  DAt3 = rdetecta*UA3
  Dt3 = m3*omega*(UI3+DI3)
  
  ### YOUNG - SOCIALLY DISTANCED
  dS1Qdt = -S1Q*k_susp*p*(k_inf*v11Q*(UI1+k_det_c*DI1)/N1 + vA11Q*(UA1+k_det_c*DA1)/N1 + v21Q*(UI2+k_det_c*DI2)/N2 + vA21Q*(UA2+k_det_c*DA2)/N2 + v31Q*(UI3+k_det_c*DI3)/N3 + vA31Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q1Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q1Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q1Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q1Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q1Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q1Q*(UA3Q+k_det_c*DA3Q)/N3Q)
  dE1Qdt = -delta*E1Q + S1Q*k_susp*p*(k_inf*v11Q*(UI1+k_det_c*DI1)/N1 + vA11Q*(UA1+k_det_c*DA1)/N1 + v21Q*(UI2+k_det_c*DI2)/N2 + vA21Q*(UA2+k_det_c*DA2)/N2 + v31Q*(UI3+k_det_c*DI3)/N3 + vA31Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q1Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q1Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q1Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q1Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q1Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q1Q*(UA3Q+k_det_c*DA3Q)/N3Q)
  dUI1Qdt = (1-alpha1Q)*delta*E1Q - (k_report*rdetecti + m1Q*omega + (1-m1Q)*gamma)*UI1Q
  dDI1Qdt = k_report*rdetecti*UI1Q - (m1Q*omega + (1-m1Q)*gamma)*DI1Q
  dUA1Qdt = alpha1Q*delta*E1Q - (k_report*rdetecta + gamma)*UA1Q
  dDA1Qdt = k_report*rdetecta*UA1Q - gamma*DA1Q
  dR1Qdt = (1-m1Q)*gamma*(UI1Q+DI1Q) + gamma*(UA1Q+DA1Q)
  It1Q = (1-alpha1Q)*delta*E1Q
  DIt1Q = k_report*rdetecti*UI1Q
  At1Q = alpha1Q*delta*E1Q
  DAt1Q = k_report*rdetecta*UA1Q
  Dt1Q = m1Q*omega*(UI1Q+DI1Q)
  
  ### MEDIUM - SOCIALLY DISTANCED
  dS2Qdt = -S2Q*p*(k_inf*v12Q*(UI1+k_det_c*DI1)/N1 + vA12Q*(UA1+k_det_c*DA1)/N1 + v22Q*(UI2+k_det_c*DI2)/N2 + vA22Q*(UA2+k_det_c*DA2)/N2 + v32Q*(UI3+k_det_c*DI3)/N3 + vA32Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q2Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q2Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q2Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q2Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q2Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q2Q*(UA3Q+k_det_c*DA3Q)/N3Q)
  dE2Qdt = -delta*E2Q + S2Q*p*(k_inf*v12Q*(UI1+k_det_c*DI1)/N1 + vA12Q*(UA1+k_det_c*DA1)/N1 + v22Q*(UI2+k_det_c*DI2)/N2 + vA22Q*(UA2+k_det_c*DA2)/N2 + v32Q*(UI3+k_det_c*DI3)/N3 + vA32Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q2Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q2Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q2Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q2Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q2Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q2Q*(UA3Q+k_det_c*DA3Q)/N3Q)
  dUI2Qdt = (1-alpha2Q)*delta*E2Q - (rdetecti + m2Q*omega + (1-m2Q)*gamma)*UI2Q
  dDI2Qdt = rdetecti*UI2Q - (m2Q*omega + (1-m2Q)*gamma)*DI2Q
  dUA2Qdt = alpha2Q*delta*E2Q - (rdetecta + gamma)*UA2Q
  dDA2Qdt = rdetecta*UA2Q - gamma*DA2Q
  dR2Qdt = (1-m2Q)*gamma*(UI2Q+DI2Q) + gamma*(UA2Q+DA2Q)
  It2Q = (1-alpha2Q)*delta*E2Q
  DIt2Q = rdetecti*UI2Q
  At2Q = alpha2Q*delta*E2Q
  DAt2Q = rdetecta*UA2Q
  Dt2Q = m2Q*omega*(UI2Q+DI2Q)
  
  ### OLD - SOCIALLY DISTANCED
  dS3Qdt = -S3Q*p*(k_inf*v13Q*(UI1+k_det_c*DI1)/N1 + vA13Q*(UA1+k_det_c*DA1)/N1 + v23Q*(UI2+k_det_c*DI2)/N2 + vA23Q*(UA2+k_det_c*DA2)/N2 + v33Q*(UI3+k_det_c*DI3)/N3 + vA33Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q3Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q3Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q3Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q3Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q3Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q3Q*(UA3Q+k_det_c*DA3Q)/N3Q)
  dE3Qdt = -delta*E3Q + S3Q*p*(k_inf*v13Q*(UI1+k_det_c*DI1)/N1 + vA13Q*(UA1+k_det_c*DA1)/N1 + v23Q*(UI2+k_det_c*DI2)/N2 + vA23Q*(UA2+k_det_c*DA2)/N2 + v33Q*(UI3+k_det_c*DI3)/N3 + vA33Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q3Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q3Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q3Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q3Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q3Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q3Q*(UA3Q+k_det_c*DA3Q)/N3Q)
  dUI3Qdt = (1-alpha3Q)*delta*E3Q - (rdetecti + m3Q*omega + (1-m3Q)*gamma)*UI3Q
  dDI3Qdt = rdetecti*UI3Q - (m3Q*omega + (1-m3Q)*gamma)*DI3Q
  dUA3Qdt = alpha3Q*delta*E3Q - (rdetecta + gamma)*UA3Q
  dDA3Qdt = rdetecta*UA3Q - gamma*DA3Q
  dR3Qdt = (1-m3Q)*gamma*(UI3Q+DI3Q) + gamma*(UA3Q+DA3Q)
  It3Q = (1-alpha3Q)*delta*E3Q
  DIt3Q = rdetecti*UI3Q
  At3Q = alpha3Q*delta*E3Q
  DAt3Q = rdetecta*UA3Q
  Dt3Q = m3Q*omega*(UI3Q+DI3Q)
  
  # results
  output <- c(dS1dt, dE1dt, dUI1dt, dDI1dt, dUA1dt, dDA1dt, dR1dt, 
              dS2dt, dE2dt, dUI2dt, dDI2dt, dUA2dt, dDA2dt, dR2dt, 
              dS3dt, dE3dt, dUI3dt, dDI3dt, dUA3dt, dDA3dt, dR3dt, 
              dS1Qdt, dE1Qdt, dUI1Qdt, dDI1Qdt, dUA1Qdt, dDA1Qdt, dR1Qdt, 
              dS2Qdt, dE2Qdt, dUI2Qdt, dDI2Qdt, dUA2Qdt, dDA2Qdt, dR2Qdt, 
              dS3Qdt, dE3Qdt, dUI3Qdt, dDI3Qdt, dUA3Qdt, dDA3Qdt, dR3Qdt, 
              It1, It2, It3, It1Q, It2Q, It3Q,
              DIt1, DIt2, DIt3, DIt1Q, DIt2Q, DIt3Q,
              At1, At2, At3, At1Q, At2Q, At3Q,
              DAt1, DAt2, DAt3, DAt1Q, DAt2Q, DAt3Q,
              Dt1, Dt2, Dt3, Dt1Q, Dt2Q, Dt3Q)
  
  # list it!
  list(output)
}

############## RUN ODE-----------------
#'  Run the Model
#' 
#' @export
run_model <- function(func, xstart, times, params, det_table, method = "lsodes", events=NULL, parms_int, time_int) {
  return(as.data.frame(ode(func = func, y = xstart, times = times, parms = params, det_table=det_table, method = method, atol=1e-8, events=events, parms_int=parms_int, time_int=time_int)))
}

############## POST-PROCESSING-------------------
#' Make Plots
#' 
#' @export
make_plots = function(test, params){

  # formatting
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
  
  
  # make graphs of output over time
  out_age = out %>% group_by(comp, cum) %>% summarize(sum(value))

  # Flows by compartment (SEAIR)
  a = ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2) %>% summarize(value = sum(value)), 
         aes(x = time, y = value, group = comp2, col = comp2)) + geom_line() + theme_minimal() + 
    scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
  
  # Cases by age
  out_cases = out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3, comp3) %>% 
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time) %>% 
    mutate(Total = sum(Infected),
           Total_obs = sum(Detected),
           Hospital = .1*Total,
           Ventilator = .05*Total)
  
  b = ggplot(out_cases, aes(x = time, y = Infected, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") + scale_linetype(guide = F) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                           title = "Cumulative cases by age")
  
  b2 = ggplot(out_cases %>% gather(var, value, Hospital, Ventilator), 
              aes(x = time, y = value, group = var, col = var)) + geom_line() + 
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cases needing advanced care")
  
  #  Ratio of Daily New Cases to Existing Cases
  out_Re = out %>% subset(select=-comp3) %>% filter(comp %in% c("UI","DI","I", "UA", "DA", "A")) %>% 
    mutate(comp=replace(comp,comp %in% c("UI","DI","UA","DA"), "I")) %>% 
    group_by(time,cum) %>% summarize(val2=sum(value)) %>% spread(cum, val2) %>% 
    rename(existing_inf = "FALSE", cum_inf = "TRUE") %>% as.data.frame() %>% 
    mutate(new_inf=ifelse(time==1, cum_inf, cum_inf-lag(cum_inf)), ratio = new_inf/existing_inf)

  c = ggplot(out_Re, aes(x = time, y = ratio)) + geom_line() + 
           theme_minimal() + scale_color_discrete(name = "") + 
           labs(x = "Time (days)", y = "", title = "Ratio of new to existing cases")
         
  
  # Observed cases by age
  d = ggplot(out_cases, aes(x = time, y = Detected,
                        group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total_obs), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Observed cumulative cases by age")
  
  
  # Deaths by age
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3) %>% 
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2))
  e = ggplot(out_death, aes(x = time, y = val2, group = strat3, col = strat3)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative deaths by age")
  
  
  # Cases by symptoms
  out_symp = out %>% filter(cum == T & comp!="D" & !is.na(strat3)) %>% group_by(time, comp2) %>% 
    summarize(val2 = sum(value)) %>% group_by(time) %>% mutate(Total = sum(val2))
  f = ggplot(out_symp, aes(x = time, y = val2, group = comp2, col = comp2)) + geom_line() +
    geom_line(aes(y = Total), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative cases by symptoms")
  
  # Flows by compartment (devided by strata and quarentine)
  g = ggplot(out %>% filter(cum ==F), aes(x = time, y = value, group = comp, col = comp2)) + geom_line() + 
    facet_wrap(.~strat, ncol = 2) + theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
  
  # Check fit
  ts = read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T)[6:20,] %>% # These rows are for March 1st - 15th# Set a reasonable range of p
    mutate(time = 1:15, Total_obs = cum_cases)
  
  out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed"))
  h = ggplot(out_fit, aes(x = time, y = Total_obs, group = id, col=id)) + geom_line() +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Calibration") + 
    scale_linetype(name = "")
  
  
  return(list(a,b,c,d,e,f,g,h,b2))
}

#' Make Plots Comparing Base Case and Intervention
#' 
#' @export
make_plots_int = function(test, params, test_int, params_int){
  # formatting
  out_base = test %>%
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
  
  out_int = test_int %>%
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
  
  out = bind_rows(out_base %>% mutate(int = "Base"), out_int %>% mutate(int = "Intervention"))
  
  
  # make graphs of output over time
  out_age = out %>% group_by(comp, cum) %>% summarize(sum(value)) %>% ungroup()
  
  # Flows by compartment (SEAIR)
  a = ggplot(out %>% filter(cum ==F) %>% group_by(time, comp2, int) %>% summarize(value = sum(value)), 
             aes(x = time, y = value, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) + theme_minimal() + 
    scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment") + scale_linetype(name ="")
  
  # Cases by age
  out_cases = out %>% filter(cum == T & comp!="D") %>% group_by(time, strat3, comp3, int) %>% 
    summarize(val2 = sum(value)) %>% spread(comp3, val2) %>% group_by(time, int) %>% 
    mutate(Total = sum(Infected),
           Total_obs = sum(Detected),
           Hospital = .17*.13*Total, ########this multipliers are different from the one used in no interventions
           Ventilator = .05*Total) %>% ungroup()
  
  b = ggplot(out_cases, aes(x = time, y = Infected, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cumulative cases by age")
  b2 = ggplot(out_cases %>% gather(var, value, Hospital, Ventilator), 
              aes(x = time, y = value, group = interaction(var, int), col = var)) + geom_line(aes(lty = int)) + 
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Cases needing advanced care")
  
  # Effective R
  out_Re = out %>% subset(select=-comp3) %>% filter(comp %in% c("UI","DI","I")) %>% mutate(comp=replace(comp,comp!="I", "I")) %>% 
    group_by(time,cum,int) %>% summarize(val2=sum(value))%>% spread(cum, val2) %>% group_by(time,int) %>%
    summarize(existing_inf = sum(`FALSE`), new_inf = sum(`TRUE`), ratio = new_inf/existing_inf) %>% ungroup()
  c = ggplot(out_Re, aes(x = time, y = ratio)) + geom_line(aes(lty = int)) + 
    theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Ratio of new to existing cases")
  
  
  # Observed cases by age
  d = ggplot(out_cases, aes(x = time, y = Detected,
                            group = interaction(strat3,int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total_obs, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Observed cumulative cases by age")
  
  
  # Deaths by age
  out_death = out %>% filter(cum == T & comp=="D") %>% group_by(time, strat3, int) %>% 
    summarize(val2 = sum(value)) %>% group_by(time,int) %>% mutate(Total = sum(val2)) %>% ungroup()
  e = ggplot(out_death, aes(x = time, y = val2, group = interaction(strat3, int), col = strat3)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative deaths by age")
  
  
  # Cases by symptoms
  out_symp = out %>% filter(cum == T & comp!="D" & !is.na(strat3)) %>% group_by(time, comp2, int) %>% 
    summarize(val2 = sum(value)) %>% group_by(time, int) %>% mutate(Total = sum(val2)) %>% ungroup()
  f = ggplot(out_symp, aes(x = time, y = val2, group = interaction(comp2, int), col = comp2)) + geom_line(aes(lty = int)) +
    geom_line(aes(y = Total, group = int, lty=int), col = "black") +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", title = "Cumulative cases by symptoms")
  
  # Flows by compartment (devided by strata and quarentine)
  g = ggplot(out %>% filter(cum ==F), aes(x = time, y = value, group = interaction(comp, int), col = comp2)) + geom_line(aes(lty = int)) + 
    facet_wrap(.~strat, ncol = 2) + theme_minimal() + scale_color_discrete(name = "") + 
    labs(x = "Time (days)", y = "", title = "Flows by compartment")
  
  # Check fit (won't include intervention, since we are only fitting 15 days data for now)
  ts = read.csv(system.file("time_series/time_series_SCC.csv", package="covid.epi"), as.is = T)[6:20,] %>% # These rows are for March 1st - 15th# Set a reasonable range of p
    mutate(time = 1:15, Total_obs = cum_cases, int = "Base")
  #out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% group_by(int) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed")) %>% ungroup()
  out_fit = bind_rows(out_cases %>% filter(time <= 15) %>% mutate(id = "Estimated"), ts %>% mutate(id = "Observed")) #this was copied from yuhan's update, but this update was not markered as different from prvious commit, so may have been changed long ago
  h = ggplot(out_fit, aes(x = time, y = Total_obs, group = interaction(int,id), col=id)) + geom_line(aes(lty = int)) +
    theme_minimal() + scale_color_discrete(name = "") + labs(x = "Time (days)", y = "", 
                                                             title = "Calibration") + 
    scale_linetype(name = "")
  
  return(list(a,b,c,d,e,f,g,h,b2))
}


#' Read In Detection Rates
load_detection_rates <- function() {
  det_table <- read.csv(system.file("detection_input.csv", package="covid.epi"), as.is = T)
  return(det_table)
}

############### RUN PARAMETER VECTOR----------------------


#' Process Parameters
#' 
#' @export
process_params = function(params, p.adj = NA, obs.adj = NA){

  # we take the approach of defining R0 and per-contact transmission
  # probability p based on the doubling time parameter td
  params['R0'] = calc_R0_from_td(td=params['td'],vec=params)
  params['p']= calc_p_from_R0(R0_input=params['R0'],vec=params) 

  # adjust if calibrating
  params$p = ifelse(is.na(p.adj), params$p, p.adj)
  params$obs = ifelse(is.na(obs.adj), params$obs, obs.adj)
  
  #### ADJUSTMENTS BASED ON MODEL SIMPLIFICATIONS 
  # same time to infection whether recovery or death
  params$omega = params$gamma
  
  # asymptomatic and mortality prob same whether socially distanced (Q) or no
  params$alpha1Q = params$alpha1
  params$alpha2Q = params$alpha2
  params$alpha3Q = params$alpha3
  params$m1Q = params$m1
  params$m2Q = params$m2
  params$m3Q = params$m3
  
  # contact rates for socially distanced
  # contact rates between socially distanced and non-socially distanced are social distance rates
  params$v1Q1Q = params$v1Q1 = params$v11Q = params$e*params$v11
  params$v1Q2Q = params$v1Q2 = params$v12Q = params$e*params$v12
  params$v1Q3Q = params$v1Q3 = params$v13Q = params$e*params$v13
  
  params$v2Q1Q = params$v2Q1 = params$v21Q = params$e*params$v21
  params$v2Q2Q = params$v2Q2 = params$v22Q = params$e*params$v22
  params$v2Q3Q = params$v2Q3 = params$v23Q = params$e*params$v23
  
  params$v3Q1Q = params$v3Q1 = params$v31Q = params$e*params$v31
  params$v3Q2Q = params$v3Q2 = params$v32Q = params$e*params$v32
  params$v3Q3Q = params$v3Q3 = params$v33Q = params$e*params$v33
  
  # contact rates are same whether asymptomatic or no
  # (this might be adjusted with some interventions)
  # but they have lower infection prob (kappa)
  params$vA11 = params$v11*params$kappa
  params$vA12 = params$v12*params$kappa
  params$vA13 = params$v13*params$kappa
  params$vA11Q = params$v11Q*params$kappa
  params$vA12Q = params$v12Q*params$kappa
  params$vA13Q = params$v13Q*params$kappa
  
  params$vA21 = params$v21*params$kappa
  params$vA22 = params$v22*params$kappa
  params$vA23 = params$v23*params$kappa
  params$vA21Q = params$v21Q*params$kappa
  params$vA22Q = params$v22Q*params$kappa
  params$vA23Q = params$v23Q*params$kappa
  
  params$vA31 = params$v31*params$kappa
  params$vA32 = params$v32*params$kappa
  params$vA33 = params$v33*params$kappa
  params$vA31Q = params$v31Q*params$kappa
  params$vA32Q = params$v32Q*params$kappa
  params$vA33Q = params$v33Q*params$kappa
  
  params$vA1Q1 = params$v1Q1*params$kappa
  params$vA1Q2 = params$v1Q2*params$kappa
  params$vA1Q3 = params$v1Q3*params$kappa
  params$vA1Q1Q = params$v1Q1Q*params$kappa
  params$vA1Q2Q = params$v1Q2Q*params$kappa
  params$vA1Q3Q = params$v1Q3Q*params$kappa
  
  params$vA2Q1 = params$v2Q1*params$kappa
  params$vA2Q2 = params$v2Q2*params$kappa
  params$vA2Q3 = params$v2Q3*params$kappa
  params$vA2Q1Q = params$v2Q1Q*params$kappa
  params$vA2Q2Q = params$v2Q2Q*params$kappa
  params$vA2Q3Q = params$v2Q3Q*params$kappa
  
  params$vA3Q1 = params$v3Q1*params$kappa
  params$vA3Q2 = params$v3Q2*params$kappa
  params$vA3Q3 = params$v3Q3*params$kappa
  params$vA3Q1Q = params$v3Q1Q*params$kappa
  params$vA3Q2Q = params$v3Q2Q*params$kappa
  params$vA3Q3Q = params$v3Q3Q*params$kappa
  
  params$N1 = if_else(params$n*(1-params$s)*params$young == 0, 1, params$n*(1-params$s)*params$young)
  params$N2 = if_else(params$n*(1-params$s)*params$medium == 0, 1, params$n*(1-params$s)*params$medium)
  params$N3 = if_else(params$n*(1-params$s)*params$old == 0, 1, params$n*(1-params$s)*params$old)
  params$N1Q = if_else(params$n*(params$s)*params$young == 0, 1, params$n*(params$s)*params$young)
  params$N2Q = if_else(params$n*(params$s)*params$medium == 0, 1, params$n*(params$s)*params$medium)
  params$N3Q = if_else(params$n*(params$s)*params$old == 0, 1, params$n*(params$s)*params$old)

  return(params)
}

############### RUN PARAMETER VECTOR
#' Run Model from Parameter Vector
#' 
#' @export
run_param_vec = function(params, params2 = NULL, p.adj = NA, obs.adj = NA,
                         days_out1 = 30, days_out2 = NULL, model_type = run_basic,
                         det_table){
  
  # process parameters
  params = process_params(params, p.adj = p.adj, obs.adj = obs.adj)
  if(!is.null(params2)) params2 = process_params(params2, p.adj = p.adj, obs.adj = obs.adj)

  ############## SET INITIAL CONDITIONS--------------
  
  # assuming you don't have specific reporting rates
  # you can instead pull from obs_adults and obs_kids by 
  # adding to parameter vector
  #*** this code is being grumpy (need more edits down the line) so I held off for now
  #obs_adults = obs*(medium + old)
  #obs_kids = obs*young
  #start = obs_adults/(c*(medium+old))
  #start_kids = obs_kids/(c*k_report*young)
  start = start_kids = params$obs
  
  x = data.frame(
    # initial conditions
    S_1 = params$n*(1-params$s)*params$young - start_kids*params$young*(1-params$s),
    E_1 = start_kids*(1-params$s)*params$young,
    UI_1 = start_kids*(1-params$s)*params$young*(1-params$alpha1),
    DI_1 = 0,
    UA_1 = start_kids*(1-params$s)*params$young*(params$alpha1),
    DA_1 = 0,
    R_1 = 0,

    S_2 = params$n*(1-params$s)*params$medium - start*params$medium*(1-params$s),
    E_2 = start*(1-params$s)*params$medium,
    UI_2 = start*(1-params$s)*params$medium*(1-params$alpha2),
    DI_2 = 0,
    UA_2 = start*(1-params$s)*params$medium*(params$alpha2),
    DA_2 = 0,
    R_2 = 0,

    S_3 = params$n*(1-params$s)*params$old - start*params$old*(1-params$s),
    E_3 = start*(1-params$s)*params$old,
    UI_3 = start*(1-params$s)*params$old*(1-params$alpha3),
    DI_3 = 0,
    UA_3 = start*(1-params$s)*params$old*(params$alpha3),
    DA_3 = 0,
    R_3 = 0,

    S_1Q = params$n*(params$s)*params$young - start_kids*params$young*(params$s),
    E_1Q = start_kids*(params$s)*params$young,
    UI_1Q = start_kids*(params$s)*params$young*(1-params$alpha1),
    DI_1Q = 0,
    UA_1Q = start_kids*(params$s)*params$young*(params$alpha1),
    DA_1Q = 0,
    R_1Q = 0,

    S_2Q = params$n*(params$s)*params$medium - start*params$medium*(params$s),
    E_2Q = start*(params$s)*params$medium, 
    UI_2Q = start*(params$s)*params$medium*(1-params$alpha2),
    DI_2Q = 0,
    UA_2Q = start*(params$s)*params$medium*(params$alpha2),
    DA_2Q = 0,
    R_2Q = 0,

    S_3Q = params$n*(params$s)*params$old - start*params$old*(params$s),
    E_3Q = start*(params$s)*params$old,
    UI_3Q = start*(params$s)*params$old*(1-params$alpha3),
    DI_3Q = 0,
    UA_3Q = start*(params$s)*params$old*(params$alpha3),
    DA_3Q = 0,
    R_3Q = 0)%>%
    mutate(I_1_cum = UI_1,
           I_2_cum = UI_2,
           I_3_cum = UI_3,
           I_1Q_cum = UI_1Q,
           I_2Q_cum = UI_2Q,
           I_3Q_cum = UI_3Q,
           DI_1_cum = 0,
           DI_2_cum = 0,
           DI_3_cum = 0,
           DI_1Q_cum = 0,
           DI_2Q_cum = 0,
           DI_3Q_cum = 0,
           A_1_cum = UA_1,
           A_2_cum = UA_2,
           A_3_cum = UA_3,
           A_1Q_cum = UA_1Q,
           A_2Q_cum = UA_2Q,
           A_3Q_cum = UA_3Q,
           DA_1_cum = 0,
           DA_2_cum = 0,
           DA_3_cum = 0,
           DA_1Q_cum = 0,
           DA_2Q_cum = 0,
           DA_3Q_cum = 0,
           D_1_cum = 0,
           D_2_cum = 0,
           D_3_cum = 0,
           D_1Q_cum = 0,
           D_2Q_cum = 0,
           D_3Q_cum = 0
    )
    
  
  ############## RUN MODEL----------------------
  # run the model
  test = model_type(model = model_strat, xstart = x, params = params, params2 = params2,
                    days_out1 = days_out1, days_out2 = days_out2, det_table=det_table)
  return(test)

}

##### BASIC MODEL-------------
#' Run Basic Model
#' 
#' @export
run_basic = function(model = model_strat, xstart, params = params, params2 = NULL, days_out1, days_out2 = NULL, det_table=det_table){
  
  # run model
  test = run_model(model, xstart = as.numeric(xstart), times = c(1:days_out1), 
                   params = params, det_table=det_table, method = "lsoda", parms_int = params , time_int = 0)
  names(test)[2:ncol(test)] = names(xstart)
  
  return(test)
  
}

##### WITH INTERVENTION
#' Run Model with Intervention
#' 
#' @export
run_int = function(model = model_strat, xstart, params = params, 
  params2 = NULL, days_out1, days_out2, det_table=det_table){

  compartment_names <- names(xstart)
  not_socially_distanced <- which( (! grepl("Q", compartment_names) & (! grepl("_cum", compartment_names))) )
  socially_distanced <- which( grepl("Q", compartment_names) & (! grepl("_cum", compartment_names)) )


  params2$p<-params$p
  eventfun <- function(t, y, parms, parms_int = parms_int, time_int = time_int, det_table = det_table){
    y_new<-y

    if (parms_int$p != parms$p) { 
      y_new[socially_distanced]<-(1-parms_int$s)*(y[socially_distanced]+y[not_socially_distanced])
      y_new[not_socially_distanced]<-parms_int$s*(y[socially_distanced]+y[not_socially_distanced])
    }
    return(y_new)
  }
  
  # run intervention model
  test = run_model(model_strat, xstart = as.numeric(xstart), 
    times = c(1:days_out2), params, det_table=det_table,  method = "lsoda",
    events=list(func = eventfun, time =days_out1), parms_int=params2,
    time_int=days_out1)

  names(test)[2:ncol(test)] = names(xstart)

  return(test)
  
}

#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

