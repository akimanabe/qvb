#
# 2009:2012の４年
shared <-
function(data, What, r, t0, q1, tau1, q2, tau2, q3, tau3, q4, tau4){
  yearchoice <- 2009
  FL <- data %>% dplyr::filter(Yearclass == yearchoice) %>% dplyr::select(FL)
  age <- data %>% dplyr::filter(Yearclass == 2009) %>% dplyr::select(Age2)

  qvb09 <-
    function(What, r, q1, tau1, t0){
      What * tau1^r * (1- (pmax(0,1-(1-q1)*(age-t0)/tau1))^(1/(1-q1)) )^r
    }

  loglikeli09 <-
    function(What, r, q1, tau1, t0){
            -sum(dnorm(FL, qvb09(What, r, q1, tau1, t0), log = TRUE))
    }
  yearchoice <- 2010
  FL <- data %>% dplyr::filter(Yearclass == yearchoice) %>% dplyr::select(FL)
  age <- data %>% dplyr::filter(Yearclass == yearchoice) %>% dplyr::select(Age2)

  qvb10 <-
    function(What, r, q2, tau2, t0){
      What * tau2^r * (1- (pmax(0,1-(1-q2)*(age-t0)/tau2))^(1/(1-q2)) )^r
    }

  loglikeli10 <-
    function(What, r, q2, tau2, t0){
      -sum(dnorm(FL, qvb10(What, r, q2, tau2, t0), log = TRUE))
    }

  yearchoice <- 2011
  FL <- data %>% dplyr::filter(Yearclass == yearchoice) %>% dplyr::select(FL)
  age <- data %>% dplyr::filter(Yearclass == yearchoice) %>% dplyr::select(Age2)

  qvb11 <-
    function(What, r, q3, tau3, t0){
      What * tau3^r * (1- (pmax(0,1-(1-q3)*(age-t0)/tau3))^(1/(1-q3)) )^r
    }

  loglikeli11 <-
    function(What, r, q3, tau3, t0){
      -sum(dnorm(FL, qvb11(What, r, q3, tau3, t0), log = TRUE))
    }

  yearchoice <- 2012
  FL <- data %>% dplyr::filter(Yearclass == yearchoice) %>% dplyr::select(FL)
  age <- data %>% dplyr::filter(Yearclass == yearchoice) %>% dplyr::select(Age2)

  qvb12 <-
    function(What, r, q4, tau4, t0){
      What * tau4^r * (1- (pmax(0,1-(1-q4)*(age-t0)/tau4))^(1/(1-q4)) )^r
      }

  loglikeli12 <-
    function(What, r, q4, tau4, t0){
      -sum(dnorm(FL, qvb12(What, r, q4, tau4, t0), log = TRUE))
    }

  theloglik <- function(What, r, t0, q1, tau1, q2, tau2, q3, tau3, q4, tau4)
    loglikeli09(What, r, q1, tau1, t0)-
    loglikeli10(What, r, q2, tau2, t0)-
    loglikeli11(What, r, q3, tau3, t0)-
    loglikeli12(What, r, q4, tau4, t0)

  res <- bbmle::mle2(theloglik, start=list(What=What,
                                           r=r,
                                           t0=t0,
                                           q1=q1,
                                           tau1=tau1,
                                           q2=q2,
                                           tau2=tau2,
                                           q3=q3,
                                           tau3=tau3,
                                           q4=q4,
                                           tau4=tau4))
}




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
