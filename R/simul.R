# simultaneous fit

qvb <-
  function(What, r, q, tau, t0){
    What * tau^r * (1- (pmax(0,1-(1-q)*(t-t0)/tau))^(1/(1-q)) )^r
  }

qvbf <- function(What, r, q, tau, t0, c = 0.07){
  w <- qvb(age, What, r, q, tau, t0)
  w * r * (w/What)^(-1/r)*(1-(1-(1/tau)*(w/What)^(1/r))^q) *c

}


qvb_loglik <- function(What, r, q, tau, t0){
  -sum(dnorm(FL, qvb(What, r, q, tau, t0), log=TRUE))
}

qvbf_loglik <- function(What, r, q, tau, t0, c){
  -sum(dnorm(GW, qvbf(What, r, q, tau, t0, c), log=TRUE))
}
