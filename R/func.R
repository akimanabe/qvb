
# Define functions

age <- seq(1, 10, 0.1)

vb <-
  function(age, Linf, k, t0){
    t <- age
    Lt <- Linf * (1-exp(-k*(age-t0)))
    return(Lt)
  }

vb3 <-
  function(age, Winf, k, t0){
    t <- age
    Wt <- Winf * (1-exp(-k*(age-t0)))^3
    return(Wt)
  }

gompertz <-
  function(age, Linf, k, t0){
    Lt <- Linf * exp(-exp(-k*(t-t0)))
    return(Lt)
  }

richards <- 
  function(age, Linf, k, t0, r){
    Lt <- Linf * (1 + r * exp(-k*(t-t0)))^(-1/r)
    return(Lt)
  }

qvb <- 
  function(age, What, r, q, tau, t0){
    t <- age
    Wt <- What * tau^r * (1- (pmax(0,1-(1-q)*(t-t0)/tau))^(1/(1-q)) )^r
    return(Wt)
  }

qvbf <- function(age, What, r, q, tau, t0, c = 0.07){
  t <- age
  w <- qvb(age, What, r, q, tau, t0)
  f <- w * r * (w/What)^(-1/r)*(1-(1-(1/tau)*(w/What)^(1/r))^q) *c
  return(f)
}
  