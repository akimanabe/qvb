fpath <- "C:/Rfiles/ALK2/masabadata.csv"

sabadata <-
  read.csv(fpath) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Yearclass = as.factor(Yearclass))

#' Fit q-von bertalanffy
#'
#' @param FL length data
#' @param age agedata
#' @param What param initial
#' @param r param initial
#' @param q param initial
#' @param tau param initial
#' @param t0 param initial
#'
#' @return
#' @export
#'
#' @examples

fit_qvb <- function(FL, age, What, r, q, tau, t0){
  
  qvb <-
    function(What, r, q, tau, t0){
      Wt <- What * tau^r * (1- (pmax(0,1-(1-q)*(age-t0)/tau))^(1/(1-q)) )^r
      return(Wt)
    }
  
  loglikeli <-
    function(What, r, q, tau, t0){
      
      -sum(dnorm(FL, qvb(What, r, q, tau, t0), log = TRUE))
    }
  
  for(i in 1:10){
    
    if(i==1){
      
      res <- mle2(loglikeli, start=list(What=What, r = r, q = q, tau = tau, t0= t0))
      show(res)
      
    }else{
      res <- mle2(loglikeli, start=list(What=res@coef[1],
                                        r = res@coef[2],
                                        q = res@coef[3],
                                        tau = res@coef[4],
                                        t0=res@coef[5]))
      show(res)
    }
  }
  
  What <- res@coef[1]
  r    <- res@coef[2]
  q    <- res@coef[3]
  tau  <- res@coef[4]
  t0   <- res@coef[5]
  
  p <<- c(What, r, q, tau, t0)
  names(p) <<- c("What", "r", "q", "tau", "t0")
  return(p)
  
}

#' Fit von Bertalanffy
#'
#' @param FL length data
#' @param age age data
#' @param Linf param intiale
#' @param K param intiale
#' @param t0 param intiale
#'
#' @return
#' @export
#'
#' @examples
fit_vb <-  function(FL,age, Linf, K, t0){
  
  t <- age
  
  vb_loglik <- function(Linf, K, t0){
    -sum(dnorm(FL, Linf * (1 - exp(-K * (t-t0))), log=TRUE))
  }
  
  ##
  for(i in 1:10){
    
    if(i==1){
      
      res <- bbmle::mle2(vb_loglik, start = list(Linf=Linf, K=K, t0=t0))
      show(res)
      
    }else{
      
      res <- bbmle::mle2(vb_loglik, start=list(Linf=res@coef[1],
                                               K = res@coef[2],
                                               t0 = res@coef[3]))
      show(res)
      
    }
  }
  ##
  Linf <- res@coef[1]
  K    <- res@coef[2]
  t0   <- res@coef[3]
  
  vbp <<- c(Linf, K, t0)
  names(vbp) <<- c("Linf", "K", "t0")
  return(vbp)
  
  
}