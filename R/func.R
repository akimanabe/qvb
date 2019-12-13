
# Define functions

vb <-
  function(Linf, k, t0){
    Linf * (1-exp(-k*(age-t0)))
  }

vb3 <-
  function(Winf, k, t0){
    Winf * (1-exp(-k*(age-t0)))^3
    }

gompertz <-
  function(age, Linf, k, t0){
    Linf * exp(-exp(-k*(t-t0)))
  }

richards <-
  function(age, Linf, k, t0, r){
    Linf * (1 + r * exp(-k*(t-t0)))^(-1/r)
  }

qvb <-
  function(age, What, r, q, tau, t0){
    Wt <- What * tau^r * (1- (pmax(0,1-(1-q)*(t-t0)/tau))^(1/(1-q)) )^r
  }

qvbf <- function(What, r, q, tau, t0, c = 0.07){
  w <- qvb(What, r, q, tau, t0)
  w * r * (w/What)^(-1/r)*(1-(1-(1/tau)*(w/What)^(1/r))^q) *c
}
