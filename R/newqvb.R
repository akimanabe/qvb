# R script for fitting the generalized q-VBGF (Manabe et al. 2018. PLoS ONE 13(6):e0199346)

#' Generalized q - von Bertalanffy growth funciton
#'
#' @param Lhat Initial parameter for L_hat; Growth scale factors
#' @param r Initial parameter for r; Growth exponents
#' @param q Initial parameter for q; Growth indeterminacy timing parameter
#' @param tau Initial parameter for tau; Maturation timing parameter
#' @param t0 Initial parameter for t0; Theretical age at size zero
#'
#'
#' @return vector of size at given age
#' @export
#'
#' @examples
#' qvb(What=300, r=0.75, q=1.2, tau=2, t0= -0.01)
qvb <-
  function(age, Lhat, r, q, tau, t0){
    Lhat * tau^r * (1- (pmax(0,1-(1-q)*(age-t0)/tau))^(1/(1-q)) )^r
  }

fit.qvb <- function(length, age, Lhat, r, q, tau, t0, method="OLS"){

  if(method == "OLS"){

    p <- c(Lhat, r, q, tau, t0)

    rss <- function(p){
      sum(length - qvb(age,p[1], p[2], p[3], p[4], p[5]))^2
    }

    res <- optim(p, rss)
    return(res)

  }

  if(method == "MLE"){}



}

bar <- fit.qvb(foo$FL, foo$Age2, 350, 0.8, 1.2, 1, 0)


fit2 <- function(length, age, Lhat, r, q, tau, t0){

  LL <- function(Lhat, r, q, tau, t0){
    -sum(dnorm(length, qvb(age, Lhat, r, q, tau, t0), log = TRUE))
  }

  res.mle <- bbmle::mle2(LL, start=list(Lhat=Lhat, r=r, q=q, tau=tau, t0=t0))
  
  Lhat <- res.mle@coef[1]
  r    <- res.mle@coef[2]
  q    <- res.mle@coef[3]
  tau  <- res.mle@coef[4]
  t0   <- res.mle@coef[5]

  return(list(Lhat, r, q, tau, t0))
  
  }

# 
# qvbL <-
#   function(Lhat, r, q, tau, t0){
#     Lhat * tau^r * (1- (pmax(0,1-(1-q)*(age-t0)/tau))^(1/(1-q)) )^r
#   }
# 
# fit_qvb <- function(FL, age, What, r, q, tau, t0){
# 
#   qvb <-
#     function(What, r, q, tau, t0){
#       Wt <- What * tau^r * (1- (pmax(0,1-(1-q)*(age-t0)/tau))^(1/(1-q)) )^r
#       return(Wt)
#     }
# 
#   loglikeli <-
#     function(What, r, q, tau, t0){
# 
#       -sum(dnorm(FL, qvb(What, r, q, tau, t0), log = TRUE))
#     }
# 
#   for(i in 1:10){
# 
#     if(i==1){
# 
#       res <- mle2(loglikeli, start=list(What=What, r = r, q = q, tau = tau, t0= t0))
#       show(res)
# 
#     }else{
#       res <- mle2(loglikeli, start=list(What=res@coef[1],
#                                         r = res@coef[2],
#                                         q = res@coef[3],
#                                         tau = res@coef[4],
#                                         t0=res@coef[5]))
#       show(res)
#     }
#   }
# 
#   What <- res@coef[1]
#   r    <- res@coef[2]
#   q    <- res@coef[3]
#   tau  <- res@coef[4]
#   t0   <- res@coef[5]
# 
#   p <<- c(What, r, q, tau, t0)
#   names(p) <<- c("What", "r", "q", "tau", "t0")
#   return(p)
# 
# }
