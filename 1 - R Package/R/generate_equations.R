make_eqn_strat = function(group){
  if (group==1) { #only young group have *k_susp
    F = paste("S", group, "*k_susp*p*(k_inf*v1", group, "*(UI1+k_det_c*DI1)/N1 + vA1", group, "*(UA1+k_det_c*DA1)/N1 + v2", group, "*(UI2+k_det_c*DI2)/N2 + vA2", group, "*(UA2+k_det_c*DA2)/N2 + v3", group, "*(UI3+k_det_c*DI3)/N3 + vA3", group, "*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q", group, "*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q", group, "*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q", group, "*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q", group, "*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q", group, "*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q", group, "*(UA3Q+k_det_c*DA3Q)/N3Q)", sep = "")
  } else {
    F = paste("S", group, "*p*(k_inf*v1", group, "*(UI1+k_det_c*DI1)/N1 + vA1", group, "*(UA1+k_det_c*DA1)/N1 + v2", group, "*(UI2+k_det_c*DI2)/N2 + vA2", group, "*(UA2+k_det_c*DA2)/N2 + v3", group, "*(UI3+k_det_c*DI3)/N3 + vA3", group, "*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q", group, "*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q", group, "*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q", group, "*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q", group, "*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q", group, "*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q", group, "*(UA3Q+k_det_c*DA3Q)/N3Q)", sep = "")
  }
  
  
  S = paste("dS", group, "dt = -", F, sep = "")
  
  E = paste("dE", group, "dt = -delta*E", group, " +", F, sep = "")
  
  UI = paste("dUI", group, "dt = (1-alpha", group, ")*delta*E", group, " - (rdetecti*q + m", group, "*omega + (1-m", group, ")*gamma)*UI", group, sep = "")
  
  DI = paste("dDI", group, "dt = rdetecti*q*UI", group, " - (m", group, "*omega + (1-m", group, ")*gamma)*DI", group, sep = "")
  
  UA = paste("dUA", group, "dt = alpha", group, "*delta*E", group, " - (rdetecta*q + gamma)*UA", group, sep = "")
  
  DA = paste("dDA", group, "dt = rdetecta*q*UA", group, " - gamma*DA", group, sep = "")
  
  R = paste("dR", group, "dt = (1-m", group, ")*gamma*(UI", group,"+DI", group, ")"," + gamma*(UA", group, "+DA", group, ")", sep = "")
  
  IT = paste("It", group, " = (1-alpha", group, ")*delta*E", group, sep = "")
  
  AT = paste("At", group, " = alpha", group, "*delta*E", group, sep = "")
  
  DT = paste("Dt", group, " = m", group, "*omega*(UI", group, "+DI", group, ")", sep = "")
  
  return(c(S, E, UI, DI, UA, DA, R, IT, AT, DT))
}

make_eqn_strat(1)

make_eqn_strat_Q = function(group){
  if (group==1) { #only young group have *k_susp
    F = paste("S", group, "Q*k_susp*p*(k_inf*v1", group, "Q*(UI1+k_det_c*DI1)/N1 + vA1", group, "Q*(UA1+k_det_c*DA1)/N1 + v2", group, "Q*(UI2+k_det_c*DI2)/N2 + vA2", group, "Q*(UA2+k_det_c*DA2)/N2 + v3", group, "Q*(UI3+k_det_c*DI3)/N3 + vA3", group, "Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q", group, "Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q", group, "Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q", group, "Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q", group, "Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q", group, "Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q", group, "Q*(UA3Q+k_det_c*DA3Q)/N3Q)", sep = "")
  } else {
    F = paste("S", group, "Q*p*(k_inf*v1", group, "Q*(UI1+k_det_c*DI1)/N1 + vA1", group, "Q*(UA1+k_det_c*DA1)/N1 + v2", group, "Q*(UI2+k_det_c*DI2)/N2 + vA2", group, "Q*(UA2+k_det_c*DA2)/N2 + v3", group, "Q*(UI3+k_det_c*DI3)/N3 + vA3", group, "Q*(UA3+k_det_c*DA3)/N3 + k_inf*v1Q", group, "Q*(UI1Q+k_det_c*DI1Q)/N1Q + vA1Q", group, "Q*(UA1Q+k_det_c*DA1Q)/N1Q + v2Q", group, "Q*(UI2Q+k_det_c*DI2Q)/N2Q + vA2Q", group, "Q*(UA2Q+k_det_c*DA2Q)/N2Q + v3Q", group, "Q*(UI3Q+k_det_c*DI3Q)/N3Q + vA3Q", group, "Q*(UA3Q+k_det_c*DA3Q)/N3Q)", sep = "")
  }
  
  
  S = paste("dS", group, "Qdt = -", F, sep = "")
  
  E = paste("dE", group, "Qdt = -delta*E", group, "Q + ", F, sep = "")
  
  UI = paste("dUI", group, "Qdt = (1-alpha", group, "Q)*delta*E", group, "Q - (rdetecti*q + m", group, "Q*omega + (1-m", group, "Q)*gamma)*UI", group, "Q",sep = "")
  
  DI = paste("dDI", group, "Qdt = rdetecti*Q*UI", group, "Q - (m", group, "Q*omega + (1-m", group, "Q)*gamma)*DI", group, "Q", sep = "")
  
  UA = paste("dUA", group, "Qdt = alpha", group, "Q*delta*E", group, "Q - (rdetecta*q + gamma)*UA", group, "Q", sep = "")
  
  DA = paste("dDA", group, "Qdt = rdetecta*q*UA", group, "Q - gamma*DA", group, "Q", sep = "")
  
  R = paste("dR", group, "Qdt = (1-m", group, "Q)*gamma*(UI", group, "Q+DI", group, "Q)", " + gamma*(UA", group, "Q+DA", group, "Q)", sep = "")
  
  IT = paste("It", group, "Q = (1-alpha", group, "Q)*delta*E", group, "Q", sep = "")
  
  AT = paste("At", group, "Q = alpha", group, "Q*delta*E", group, "Q", sep = "")
  
  DT = paste("Dt", group, "Q = m", group, "Q*omega*(UI", group, "Q+DI", group, "Q)", sep = "")
  
  return(c(S, E, UI, DI, UA, DA, R, IT, AT, DT))
}

make_eqn_strat_Q(3)