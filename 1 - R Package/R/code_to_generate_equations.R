make_eqn_strat = function(group){
  S = paste("dS", group, "dt = -S", group,
            "*k_susp*p*(k_inf*v1", group, "*I1/N1 + vA1", group, "*A1/N1 + v2", group,
            "*I2/N2 + vA2", group, "*A2/N2 + v3", group, "*I3/N3 + vA3", group, "*A3/N3 + k_inf*v1Q", group,
            "*I1Q/N1Q + vA1Q", group, "*A1Q/N1Q + v2Q", group, "*I2Q/N2Q + vA2Q", group, "*A2Q/N2Q + v3Q", group,
            "*I3Q/N3Q + vA3Q", group, "*A3Q/N3Q)",
            sep = "")
  E = paste("dE", group, "dt = -delta*E", group, "+ S", group,
            "*k_susp*p*(k_inf*v1", group, "*I1/N1 + vA1", group, "*A1/N1 + v2", group,
            "*I2/N2 + vA2", group, "*A2/N2 + v3", group, "*I3/N3 + vA3", group, "*A3/N3 + k_inf*v1Q", group,
            "*I1Q/N1Q + vA1Q", group, "*A1Q/N1Q + v2Q", group, "*I2Q/N2Q + vA2Q", group, "*A2Q/N2Q + v3Q", group,
            "*I3Q/N3Q + vA3Q", group, "*A3Q/N3Q)",
            sep = "")
  A = paste("dA", group, "dt = alpha", group, "*delta*E", group, " - gamma*A", group, sep = "")
  I = paste("dI", group, "dt = (1-alpha", group, ")*delta*E", group, "- (m", group, "*omega + (1-m", group, ")*gamma)*I",group, sep = "")
  R = paste("dR", group, "dt = gamma*A", group, "+ (1-m", group, ")*gamma*I", group, sep = "")
  return(c(S, E, I, A, R))
}

