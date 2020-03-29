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
  
  DFE <-c(young , medium, old , c(0,0,0))*n;
  
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
  
  F <- matrix(0L, nrow = 18, ncol = 18)  # input matrix to infectious states (F \geq 0)
  V <- matrix(0L, nrow = 18, ncol = 18)  # transitions within and out of infectious states (non-singular)
  
  # order : E_1 I_1 A_1 E_2 I_2 A_2 E_3 I_3 A_3 E_1Q I_1Q A_1Q E_2Q I_2Q A_2Q E_3Q I_3Q A_3Q 
  
  # F[i,j] = d_{x_j}input_i
  # only the E states have input
  
  tempvector<- rep(0L, 18);
  tempvector[2] <- params$k_inf*v11/N1 ;
  tempvector[3] <- vA11/N1 ;
  tempvector[5] <- v21/N2 ;
  tempvector[6] <- vA21/N2 ;
  tempvector[8] <- v31/N3 ;
  tempvector[9] <- vA31/N3 ;
  tempvector[11] <- params$k_inf*v1Q1/N1Q ;
  tempvector[12] <- vA1Q1/N1Q ;
  tempvector[14] <- v2Q1/N2Q ;
  tempvector[15] <- vA2Q1/N2Q ;
  tempvector[17] <- v3Q1/N3Q ;
  tempvector[18] <- vA3Q1/N3Q ;
  
  F[1,] <- S10*params$k_susp*p* tempvector; 
  
  tempvector<- rep(0L, 18);
  tempvector[2] <- params$k_inf*v12/N1 ;
  tempvector[3] <- vA12/N1 ;
  tempvector[5] <- v22/N2 ;
  tempvector[6] <- vA22/N2 ;
  tempvector[8] <- v32/N3 ;
  tempvector[9] <- vA32/N3 ;
  tempvector[11] <- params$k_inf*v1Q2/N1Q ;
  tempvector[12] <- vA1Q2/N1Q ;
  tempvector[14] <- v2Q2/N2Q ;
  tempvector[15] <- vA2Q2/N2Q ;
  tempvector[17] <- v3Q2/N3Q ;
  tempvector[18] <- vA3Q2/N3Q ;
  
  F[4,] <- S20*p*tempvector; 
  
  tempvector<- rep(0L, 18);
  tempvector[2] <- params$k_inf*v13/N1 ;
  tempvector[3] <- vA13/N1 ;
  tempvector[5] <- v23/N2 ;
  tempvector[6] <- vA23/N2 ;
  tempvector[8] <- v33/N3 ;
  tempvector[9] <- vA33/N3 ;
  tempvector[11] <- params$k_inf*v1Q3/N1Q ;
  tempvector[12] <- vA1Q3/N1Q ;
  tempvector[14] <- v2Q3/N2Q ;
  tempvector[15] <- vA2Q3/N2Q ;
  tempvector[17] <- v3Q3/N3Q ;
  tempvector[18] <- vA3Q3/N3Q ;
  
  F[7,] <- S30*p*tempvector; 
  
  tempvector<- rep(0L, 18);
  tempvector[2] <- params$k_inf*v11Q/N1 ;
  tempvector[3] <- vA11Q/N1 ;
  tempvector[5] <- v21Q/N2 ;
  tempvector[6] <- vA21Q/N2 ;
  tempvector[8] <- v31Q/N3 ;
  tempvector[9] <- vA31Q/N3 ;
  tempvector[11] <- params$k_inf*v1Q1Q/N1Q ;
  tempvector[12] <- vA1Q1Q/N1Q ;
  tempvector[14] <- v2Q1Q/N2Q ;
  tempvector[15] <- vA2Q1Q/N2Q ;
  tempvector[17] <- v3Q1Q/N3Q ;
  tempvector[18] <- vA3Q1Q/N3Q ;
  
  F[10,] <- S1Q0*params$k_susp*p* tempvector; 
  
  tempvector<- rep(0L, 18);
  tempvector[2] <- params$k_inf*v12Q/N1 ;
  tempvector[3] <- vA12Q/N1 ;
  tempvector[5] <- v22Q/N2 ;
  tempvector[6] <- vA22Q/N2 ;
  tempvector[8] <- v32Q/N3 ;
  tempvector[9] <- vA32Q/N3 ;
  tempvector[11] <- params$k_inf*v1Q2Q/N1Q ;
  tempvector[12] <- vA1Q2Q/N1Q ;
  tempvector[14] <- v2Q2Q/N2Q ;
  tempvector[15] <- vA2Q2Q/N2Q ;
  tempvector[17] <- v3Q2Q/N3Q ;
  tempvector[18] <- vA3Q2Q/N3Q ;
  
  F[13,] <- S2Q0*p*tempvector; 
  
  tempvector<- rep(0L, 18);
  tempvector[2] <- params$k_inf*v13Q/N1 ;
  tempvector[3] <- vA13Q/N1 ;
  tempvector[5] <- v23Q/N2 ;
  tempvector[6] <- vA23Q/N2 ;
  tempvector[8] <- v33Q/N3 ;
  tempvector[9] <- vA33Q/N3 ;
  tempvector[11] <- params$k_inf*v1Q3Q/N1Q ;
  tempvector[12] <- vA1Q3Q/N1Q ;
  tempvector[14] <- v2Q3Q/N2Q ;
  tempvector[15] <- vA2Q3Q/N2Q ;
  tempvector[17] <- v3Q3Q/N3Q ;
  tempvector[18] <- vA3Q3Q/N3Q ;
  
  
  F[16,] <- S3Q0*p*tempvector; 
  
  # V matrix is internal flows and outflows
  # order : E_1 I_1 A_1 E_2 I_2 A_2 E_3 I_3 A_3 E_1Q I_1Q A_1Q E_2Q I_2Q A_2Q E_3Q I_3Q A_3Q 
  
  # V[i,j] = d_{x_j}flow_i
  
  V[1,1] <- delta;
  V[2,1] <- -(1-alpha1)*delta
  V[2,2] <- (m1*omega + (1-m1)*gamma)
  V[3,1] <- -alpha1*delta;
  V[3,3] <- gamma;
  
  V[4,4] <-  V[1,1]
  V[5,4] <- -(1-alpha2)*delta
  V[5,5] <- (m2*omega + (1-m2)*gamma)
  V[6,4] <- -alpha2*delta;
  V[6,6] <-  V[3,3];
  
  V[7,7] <-  V[1,1]
  V[8,7] <- -(1-alpha3)*delta
  V[8,8] <- (m3*omega + (1-m3)*gamma)
  V[9,7] <- -alpha3*delta;
  V[9,9] <-  V[3,3];
  
  V[10,10] <- delta;
  V[11,10] <- -(1-alpha1Q)*delta
  V[11,11] <- (m1Q*omega + (1-m1Q)*gamma)
  V[12,10] <- -alpha1Q*delta;
  V[12,12] <- gamma;
  
  V[13,13] <-  V[1,1]
  V[14,13] <- -(1-alpha2Q)*delta
  V[14,14] <- (m2Q*omega + (1-m2Q)*gamma)
  V[15,13] <- -alpha2Q*delta;
  V[15,15] <-  V[3,3];
  
  V[16,16] <-  V[1,1]
  V[17,16] <- -(1-alpha3Q)*delta
  V[17,17] <- (m3Q*omega + (1-m3Q)*gamma)
  V[18,16] <- -alpha3Q*delta;
  V[18,18] <-  V[3,3];
  
  #print(F)
  #print(V) 
  return (eigen(F %*% solve(V))$values[1])
  
}
