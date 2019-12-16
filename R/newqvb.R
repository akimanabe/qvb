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

#' Fit the generalized q- von Bertalanffy growth funciton
#'
#' @param length length data
#' @param age age data
#' @param Lhat Initial parameter for L_hat; Growth scale factors
#' @param r Initial parameter for r; Growth exponents
#' @param q Initial parameter for q; Growth indeterminacy timing parameter
#' @param tau Initial parameter for tau; Maturation timing parameter
#' @param t0 Initial parameter for t0; Theretical age at size zero
#' @param method Fitting method; select either "OLS" or "MLE"
#' @param summary if TRUE, summary is returned along with the estimated parameters
#'
#' @return list
#' @export
#'
#' @examples
fit.qvb <- function(length, age, Lhat, r, q, tau, t0, method="OLS", summary=FALSE){

  if(method == "OLS"){

    p <- c(Lhat, r, q, tau, t0)

    rss <- function(p){
      sum(length - qvb(age,p[1], p[2], p[3], p[4], p[5]))^2
    }

    res <- optim(p, rss)

    if(summary==TRUE){
      return(res)
    }else{
        resp <- res$par
        names(resp)<- c("Lhat", "r", "q", "tau", "t0")
        return(resp)
      }

  }

  if(method == "MLE"){

    LL <- function(Lhat, r, q, tau, t0){
      -sum(dnorm(length, qvb(age, Lhat, r, q, tau, t0), log = TRUE))
    }

    res.mle <- bbmle::mle2(LL, start=list(Lhat=Lhat, r=r, q=q, tau=tau, t0=t0))

    if(summary==TRUE){
      return(bbmle::summary(res.mle))
    }else{
      return(res.mle@coef)
    }

  }

}

bar <- fit.qvb(dat$FL, dat$Age2, 350, 0.8, 1.2, 1, 0, method="MLE", summary=FALSE)


fit.mle <- function(length, age, Lhat, r, q, tau, t0){

  LL <- function(Lhat, r, q, tau, t0){
    -sum(dnorm(length, qvb(age, Lhat, r, q, tau, t0), log = TRUE))
  }

  res.mle <- bbmle::mle2(LL, start=list(Lhat=Lhat, r=r, q=q, tau=tau, t0=t0))

  #return(res.mle@coef)
  return(bbmle::summary(res.mle))

  }

res <- fit.mle(dat$FL, dat$Age2, 350, 0.8, 1.2, 1, 0)
