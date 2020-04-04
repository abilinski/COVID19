#' This function takes the input p and the parameter vector as inputs and calculates the
#' corresponding R0 and doubling time
#' 
#' Calculate R0 and td from p
#' @export
calc_R0_td_from_p <- function(params, p) {
  params$p <- p
  
  R0 <- R_0(params)
  
  # calculate lambda from R0
  # f <- function(lamda) (lamda+params$delta)*(lamda+params$gamma)/(params$delta*params$gamma) - R0
  # lamda <- uniroot(f, interval=c(0,70))[[1]]
  
  # direct compute lambda from SEIR growth rate
  lamda <- (-(params$delta+params$gamma)+sqrt(4*params$delta*R0*params$gamma+(params$delta-params$gamma)**2))/2
  
  td <- log(2)/lamda
  
  return(c(R0, td))
}



