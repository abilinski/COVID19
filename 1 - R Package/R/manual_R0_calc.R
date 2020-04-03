#********************************** R0 MANUAL CALCULATION *********************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#


#### CALCULATE R0
#' Calculate R0 from Parameters
#' 
#' @export
R_0 <- function (params){

  n = params$n
  p = params$p
  young = params$young
  medium = params$medium
  old = params$old
  e = params$e
  s = params$s
  
  #DFE <-c(young , medium, old , c(0,0,0))*n;
  # including people in Qs at DFE
  DFE <-c((1-s)*young , (1-s)*medium, (1-s)*old , s*young, s*medium, s*old)*n;
  
  S10 = DFE[1];
  S20 = DFE[2];
  S30 = DFE[3];
  S1Q0 = DFE[4];
  S2Q0 = DFE[5];
  S3Q0 = DFE[6];
  
  
  #### ADJUSTMENTS BASED ON MODEL SIMPLIFICATIONS
  # same time to infection whether recovery or death
  gamma = params$gamma
  delta = params$delta
  kappa = params$kappa
  omega = gamma
  
  # asymptomatic and mortality prob same whether socially distanced (Q) or no
  alpha1 = params$alpha1
  alpha2 = params$alpha2
  alpha3 = params$alpha3
  m1 = params$m1
  m2 = params$m2
  m3 = params$m3
  alpha1Q = alpha1
  alpha2Q = alpha2
  alpha3Q = alpha3
  m1Q = m1
  m2Q = m2
  m3Q = m3
  
  v11=params$v11;
  v12=params$v12;
  v13=params$v13;
  v21=params$v21;
  v22=params$v22;
  v23=params$v23;
  v31=params$v31;
  v32=params$v32;
  v33=params$v33;
  # contact rates for socially distanced
  # contact rates between socially distanced and non-socially distanced are social distance rates
  v1Q1Q = v1Q1 = v11Q = e*v11
  v1Q2Q = v1Q2 = v12Q = e*v12
  v1Q3Q = v1Q3 = v13Q = e*v13
  
  v2Q1Q = v2Q1 = v21Q = e*v21
  v2Q2Q = v2Q2 = v22Q = e*v22
  v2Q3Q = v2Q3 = v23Q = e*v23
  
  v3Q1Q = v3Q1 = v31Q = e*v31
  v3Q2Q = v3Q2 = v32Q = e*v32
  v3Q3Q = v3Q3 = v33Q = e*v33
  
  # contact rates are same whether asymptomatic or no
  # (this might be adjusted with some interventions)
  vA11 = v11*kappa
  vA12 = v12*kappa
  vA13 = v13*kappa
  vA11Q = v11Q*kappa
  vA12Q = v12Q*kappa
  vA13Q = v13Q*kappa
  vA21 = v21*kappa
  vA22 = v22*kappa
  vA23 = v23*kappa
  vA21Q = v21Q*kappa
  vA22Q = v22Q*kappa
  vA23Q = v23Q*kappa
  vA31 = v31*kappa
  vA32 = v32*kappa
  vA33 = v33*kappa
  vA31Q = v31Q*kappa
  vA32Q = v32Q*kappa
  vA33Q = v33Q*kappa
  vA1Q1 = v1Q1*kappa
  vA1Q2 = v1Q2*kappa
  vA1Q3 = v1Q3*kappa
  vA1Q1Q = v1Q1Q*kappa
  vA1Q2Q = v1Q2Q*kappa
  vA1Q3Q = v1Q3Q*kappa
  vA2Q1 = v2Q1*kappa
  vA2Q2 = v2Q2*kappa
  vA2Q3 = v2Q3*kappa
  vA2Q1Q = v2Q1Q*kappa
  vA2Q2Q = v2Q2Q*kappa
  vA2Q3Q = v2Q3Q*kappa
  vA3Q1 = v3Q1*kappa
  vA3Q2 = v3Q2*kappa
  vA3Q3 = v3Q3*kappa
  vA3Q1Q = v3Q1Q*kappa
  vA3Q2Q = v3Q2Q*kappa
  vA3Q3Q = v3Q3Q*kappa
  
  ##Populations
  N1 = ifelse(n*(1-params$s)*young == 0, 1, n*(1-params$s)*young)
  N2 = ifelse(n*(1-params$s)*medium == 0, 1,n*(1-params$s)*medium)
  N3 = ifelse(n*(1-params$s)*old == 0, 1, n*(1-params$s)*old)
  N1Q = ifelse(n*(params$s)*young == 0, 1, n*(params$s)*young)
  N2Q = ifelse(n*(params$s)*medium == 0, 1, n*(params$s)*medium)
  N3Q = ifelse(n*(params$s)*old == 0, 1, n*(params$s)*old)
  
  ## Calculate next generation matrix
  
  F <- matrix(0L, nrow = 30, ncol = 30)  # input matrix to infectious states (F \geq 0)
  V <- matrix(0L, nrow = 30, ncol = 30)  # transitions within and out of infectious states (non-singular)
  
  # order : E_1 UI_1 DI_1 UA_1 DA_1 E_2 UI_2 DI_2 UA_2 DA_2 E_3 UI_3 DI_3 UA_3 DA_3 E_1Q UI_1Q DI_1Q UA_1Q DA_1Q E_2Q UI_2Q DI_2Q UA_2Q DA_2Q E_3Q UI_3Q DI_3Q UA_3Q DA_3Q 
  
  # F[i,j] = d_{x_j}input_i
  # only the E states have input
  k_det_c = params$k_det_c
  
  tempvector<- rep(0L, 30);
  tempvector[2] <- params$k_inf*v11/N1 ;
  tempvector[3] <- params$k_inf*v11*k_det_c/N1;
  tempvector[4] <- vA11/N1;
  tempvector[5] <- vA11*k_det_c/N1;
  tempvector[7] <- v21/N2;
  tempvector[8] <- v21*k_det_c/N2;
  tempvector[9] <- vA21/N2;
  tempvector[10] <- vA21*k_det_c/N2;
  tempvector[12] <- v31/N3;
  tempvector[13] <- v31*k_det_c/N3;
  tempvector[14] <- vA31/N3;
  tempvector[15] <- vA31*k_det_c/N3;
  tempvector[17] <- params$k_inf*v1Q1/N1Q ;
  tempvector[18] <- params$k_inf*v1Q1*k_det_c/N1Q ;
  tempvector[19] <- vA1Q1/N1Q;
  tempvector[20] <- vA1Q1*k_det_c/N1Q;
  tempvector[22] <- v2Q1/N2Q;
  tempvector[23] <- v2Q1*k_det_c/N2Q;
  tempvector[24] <- vA2Q1/N2Q;
  tempvector[26] <- vA2Q1*k_det_c/N2Q;
  tempvector[27] <- v3Q1/N3Q;
  tempvector[28] <- v3Q1*k_det_c/N3Q;
  tempvector[29] <- vA3Q1/N3Q;
  tempvector[30] <- vA3Q1*k_det_c/N3Q;
  
  F[1,] <- S10*params$k_susp*p* tempvector; 
  
  tempvector<- rep(0L, 30);
  tempvector[2] <- params$k_inf*v12/N1 ;
  tempvector[3] <- params$k_inf*v12*k_det_c/N1;
  tempvector[4] <- vA12/N1;
  tempvector[5] <- vA12*k_det_c/N1;
  tempvector[7] <- v22/N2;
  tempvector[8] <- v22*k_det_c/N2;
  tempvector[9] <- vA22/N2;
  tempvector[10] <- vA22*k_det_c/N2;
  tempvector[12] <- v32/N3;
  tempvector[13] <- v32*k_det_c/N3;
  tempvector[14] <- vA32/N3;
  tempvector[15] <- vA32*k_det_c/N3;
  tempvector[17] <- params$k_inf*v1Q2/N1Q ;
  tempvector[18] <- params$k_inf*v1Q2*k_det_c/N1Q ;
  tempvector[19] <- vA1Q2/N1Q;
  tempvector[20] <- vA1Q2*k_det_c/N1Q;
  tempvector[22] <- v2Q2/N2Q;
  tempvector[23] <- v2Q2*k_det_c/N2Q;
  tempvector[24] <- vA2Q2/N2Q;
  tempvector[26] <- vA2Q2*k_det_c/N2Q;
  tempvector[27] <- v3Q2/N3Q;
  tempvector[28] <- v3Q2*k_det_c/N3Q;
  tempvector[29] <- vA3Q2/N3Q;
  tempvector[30] <- vA3Q2*k_det_c/N3Q;
  
  F[6,] <- S20*p*tempvector; 
  
  tempvector<- rep(0L, 30);
  tempvector[2] <- params$k_inf*v13/N1 ;
  tempvector[3] <- params$k_inf*v13*k_det_c/N1;
  tempvector[4] <- vA13/N1;
  tempvector[5] <- vA13*k_det_c/N1;
  tempvector[7] <- v23/N2;
  tempvector[8] <- v23*k_det_c/N2;
  tempvector[9] <- vA23/N2;
  tempvector[10] <- vA23*k_det_c/N2;
  tempvector[12] <- v33/N3;
  tempvector[13] <- v33*k_det_c/N3;
  tempvector[14] <- vA33/N3;
  tempvector[15] <- vA33*k_det_c/N3;
  tempvector[17] <- params$k_inf*v1Q3/N1Q ;
  tempvector[18] <- params$k_inf*v1Q3*k_det_c/N1Q ;
  tempvector[19] <- vA1Q3/N1Q;
  tempvector[20] <- vA1Q3*k_det_c/N1Q;
  tempvector[22] <- v2Q3/N2Q;
  tempvector[23] <- v2Q3*k_det_c/N2Q;
  tempvector[24] <- vA2Q3/N2Q;
  tempvector[26] <- vA2Q3*k_det_c/N2Q;
  tempvector[27] <- v3Q3/N3Q;
  tempvector[28] <- v3Q3*k_det_c/N3Q;
  tempvector[29] <- vA3Q3/N3Q;
  tempvector[30] <- vA3Q3*k_det_c/N3Q;
  
  F[11,] <- S30*p*tempvector; 
  
  tempvector<- rep(0L, 30);
  tempvector[2] <- params$k_inf*v11Q/N1 ;
  tempvector[3] <- params$k_inf*v11Q*k_det_c/N1;
  tempvector[4] <- vA11Q/N1;
  tempvector[5] <- vA11Q*k_det_c/N1;
  tempvector[7] <- v21Q/N2;
  tempvector[8] <- v21Q*k_det_c/N2;
  tempvector[9] <- vA21Q/N2;
  tempvector[10] <- vA21Q*k_det_c/N2;
  tempvector[12] <- v31Q/N3;
  tempvector[13] <- v31Q*k_det_c/N3;
  tempvector[14] <- vA31Q/N3;
  tempvector[15] <- vA31Q*k_det_c/N3;
  tempvector[17] <- params$k_inf*v1Q1Q/N1Q ;
  tempvector[18] <- params$k_inf*v1Q1Q*k_det_c/N1Q ;
  tempvector[19] <- vA1Q1Q/N1Q;
  tempvector[20] <- vA1Q1Q*k_det_c/N1Q;
  tempvector[22] <- v2Q1Q/N2;
  tempvector[23] <- v2Q1Q*k_det_c/N2Q;
  tempvector[24] <- vA2Q1Q/N2Q;
  tempvector[26] <- vA2Q1Q*k_det_c/N2Q;
  tempvector[27] <- v3Q1Q/N3Q;
  tempvector[28] <- v3Q1Q*k_det_c/N3Q;
  tempvector[29] <- vA3Q1Q/N3Q;
  tempvector[30] <- vA3Q1Q*k_det_c/N3Q;
  
  F[16,] <- S1Q0*params$k_susp*p* tempvector; 
  
  tempvector<- rep(0L, 30);
  tempvector[2] <- params$k_inf*v12Q/N1 ;
  tempvector[3] <- params$k_inf*v12Q*k_det_c/N1;
  tempvector[4] <- vA12Q/N1;
  tempvector[5] <- vA12Q*k_det_c/N1;
  tempvector[7] <- v22Q/N2;
  tempvector[8] <- v22Q*k_det_c/N2;
  tempvector[9] <- vA22Q/N2;
  tempvector[10] <- vA22Q*k_det_c/N2;
  tempvector[12] <- v32Q/N3;
  tempvector[13] <- v32Q*k_det_c/N3;
  tempvector[14] <- vA32Q/N3;
  tempvector[15] <- vA32Q*k_det_c/N3;
  tempvector[17] <- params$k_inf*v1Q2Q/N1Q ;
  tempvector[18] <- params$k_inf*v1Q2Q*k_det_c/N1Q ;
  tempvector[19] <- vA1Q2Q/N1Q;
  tempvector[20] <- vA1Q2Q*k_det_c/N1Q;
  tempvector[22] <- v2Q2Q/N2Q;
  tempvector[23] <- v2Q2Q*k_det_c/N2Q;
  tempvector[24] <- vA2Q2Q/N2Q;
  tempvector[26] <- vA2Q2Q*k_det_c/N2Q;
  tempvector[27] <- v3Q2Q/N3Q;
  tempvector[28] <- v3Q2Q*k_det_c/N3Q;
  tempvector[29] <- vA3Q2Q/N3Q;
  tempvector[30] <- vA3Q2Q*k_det_c/N3Q;
  
  F[21,] <- S2Q0*p*tempvector; 
  
  tempvector<- rep(0L, 30);
  tempvector[2] <- params$k_inf*v13Q/N1 ;
  tempvector[3] <- params$k_inf*v13Q*k_det_c/N1;
  tempvector[4] <- vA13Q/N1;
  tempvector[5] <- vA13Q*k_det_c/N1;
  tempvector[7] <- v23Q/N2;
  tempvector[8] <- v23Q*k_det_c/N2;
  tempvector[9] <- vA23Q/N2;
  tempvector[10] <- vA23Q*k_det_c/N2;
  tempvector[12] <- v33Q/N3;
  tempvector[13] <- v33Q*k_det_c/N3;
  tempvector[14] <- vA33Q/N3;
  tempvector[15] <- vA33Q*k_det_c/N3;
  tempvector[17] <- params$k_inf*v1Q3Q/N1Q ;
  tempvector[18] <- params$k_inf*v1Q3Q*k_det_c/N1Q ;
  tempvector[19] <- vA1Q3Q/N1Q;
  tempvector[20] <- vA1Q3Q*k_det_c/N1Q;
  tempvector[22] <- v2Q3Q/N2Q;
  tempvector[23] <- v2Q3Q*k_det_c/N2Q;
  tempvector[24] <- vA2Q3Q/N2Q;
  tempvector[26] <- vA2Q3Q*k_det_c/N2Q;
  tempvector[27] <- v3Q3Q/N3Q;
  tempvector[28] <- v3Q3Q*k_det_c/N3Q;
  tempvector[29] <- vA3Q3Q/N3Q;
  tempvector[30] <- vA3Q3Q*k_det_c/N3Q;
  
  F[26,] <- S3Q0*p*tempvector; 
  
  # V matrix is internal flows and outflows
  # order : E_1 UI_1 DI_1 UA_1 DA_1 E_2 UI_2 DI_2 UA_2 DA_2 E_3 UI_3 DI_3 UA_3 DA_3 E_1Q UI_1Q DI_1Q UA_1Q DA_1Q E_2Q UI_2Q DI_2Q UA_2Q DA_2Q E_3Q UI_3Q DI_3Q UA_3Q DA_3Q 
  
  # V[i,j] = d_{x_j}flow_i
  k_report = params$k_report
  rdetecti = params$rdetecti
  rdetecta = params$rdetecta
  
  V[1,1] <- delta;
  V[2,1] <- -(1-alpha1)*delta;
  V[2,2] <- (k_report*rdetecti+m1*omega + (1-m1)*gamma);
  V[3,2] <- -(k_report*rdetecti);
  V[3,3] <- m1*omega+(1-m1)*gamma;
  V[4,1] <- -alpha1*delta;
  V[4,4] <- k_report*rdetecta+gamma;
  V[5,4] <- -(k_report*rdetecta);
  V[5,5] <- gamma;
  
  V[6,6] <- delta;
  V[7,6] <- -(1-alpha2)*delta;
  V[7,7] <- (k_report*rdetecti+m2*omega + (1-m2)*gamma);
  V[8,7] <- -(k_report*rdetecti);
  V[8,8] <- m2*omega+(1-m2)*gamma;
  V[9,6] <- -alpha2*delta;
  V[9,9] <- k_report*rdetecta+gamma;
  V[10,9] <- -(k_report*rdetecta);
  V[10,10] <- gamma;
  
  V[11,11] <- delta;
  V[12,11] <- -(1-alpha3)*delta;
  V[12,12] <- (k_report*rdetecti+m3*omega + (1-m3)*gamma);
  V[13,12] <- -(k_report*rdetecti);
  V[13,13] <- m3*omega+(1-m3)*gamma;
  V[14,11] <- -alpha3*delta;
  V[14,14] <- k_report*rdetecta+gamma;
  V[15,14] <- -(k_report*rdetecta);
  V[15,15] <- gamma;
  
  V[16,16] <- delta;
  V[17,16] <- -(1-alpha1Q)*delta;
  V[17,17] <- (k_report*rdetecti+m1Q*omega + (1-m1Q)*gamma);
  V[18,17] <- -(k_report*rdetecti);
  V[18,18] <- m1Q*omega+(1-m1Q)*gamma;
  V[19,16] <- -alpha1Q*delta;
  V[19,19] <- k_report*rdetecta+gamma;
  V[20,19] <- -(k_report*rdetecta);
  V[20,20] <- gamma;
  
  V[21,21] <- delta;
  V[22,21] <- -(1-alpha2Q)*delta;
  V[22,22] <- (k_report*rdetecti+m2Q*omega + (1-m2Q)*gamma);
  V[23,22] <- -(k_report*rdetecti);
  V[23,23] <- m2Q*omega+(1-m2Q)*gamma;
  V[24,21] <- -alpha2Q*delta;
  V[24,24] <- k_report*rdetecta+gamma;
  V[25,24] <- -(k_report*rdetecta);
  V[25,25] <- gamma;
  
  V[26,26] <- delta;
  V[27,26] <- -(1-alpha3Q)*delta;
  V[27,27] <- (k_report*rdetecti+m3Q*omega + (1-m3Q)*gamma);
  V[28,27] <- -(k_report*rdetecti);
  V[28,28] <- m3Q*omega+(1-m3Q)*gamma;
  V[29,26] <- -alpha3Q*delta;
  V[29,29] <- k_report*rdetecta+gamma;
  V[30,29] <- -(k_report*rdetecta);
  V[30,30] <- gamma;
  
  #print(F)
  #print(V) 
  return (eigen(F %*% solve(V))$values[1])
  
}
