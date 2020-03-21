## This function takes the input R0 and the parameter vector as inputs and calculates the
## corresponding p
calc_p_from_R0<- function (R0_input,vec) {
  #convert input R0 to numeric
  R0_input= as.numeric(R0_input)
  R0 = c()
  p_cand = seq(.001, .11, by = .005)
  # loop through all the values of p and find the corresponding R0s
  for(i in 1:length(p_cand)){
    vec$p=p_cand[i]
    R0[i] = R_0(vec) #, n, p_cand[i], young, medium, old)
  }
  # return the value of p that corresponds with the calculated R that's closest to input R
  return(p_cand[which.min(abs(R0-R0_input))])
}
