#' This function takes the input R0 and the parameter vector as inputs and calculates the
#' corresponding p
#' 
#' @export
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

#' #' Calculate R0 from td (based on SIR) 
#' #' @export
#' calc_R0_from_td <-function(td,vec){
#'   #calculate time of infectiousness
#'   t_inf = 1/vec$gamma 
#'   R0 = 1+(t_inf/td)*log(2)
#'   return(R0)
#' }

#' Calculate R0 from td (based on SEIR)
#' @export
calc_R0_from_td <-function(td,vec){
  #calculate time of infectiousness
  gamma = vec$gamma
  delta = vec$delta
  R0 = ((delta+log(2)/td)/delta)*((gamma+log(2)/td)/gamma)
  return(R0)
}

