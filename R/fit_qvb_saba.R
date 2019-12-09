# fit masaba

sabadata <-
  read.csv("masabadata.csv") %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Yearclass = as.factor(Yearclass))

ggplot2::ggplot(sabadata) +
  ggplot2::geom_point(aes(x=Age2, y=FL, colour = Yearclass), alpha = 0.5)

saba7 <-
  sabadata %>%
  dplyr::filter(Yearclass == 2007)

ggplot2::ggplot(saba7) +
  ggplot2::geom_point(aes(x=Age2, y=FL, colour = as.factor(Year)), alpha = 0.5)

#
#
# qvb <-
#   function(What, r, q, tau, t0){
#     Wt <- What * tau^r * (1- (pmax(0,1-(1-q)*(t-t0)/tau))^(1/(1-q)) )^r
#     return(Wt)
#   }
#
qvbf <- function(age, What, r, q, tau, t0, c = 0.07){
  t <- age
  w <- qvb(age, What, r, q, tau, t0)
  f <- w * r * (w/What)^(-1/r)*(1-(1-(1/tau)*(w/What)^(1/r))^q) *c
  return(f)
}
#
# t <- saba7$Age2
#
# loglikeli <-
#   function(What, r, q, tau, t0){
#
#     -sum(dnorm(saba7$FL, qvb(What, r, q, tau, t0), log = TRUE))
#   }
#
# res <- mle2(loglikeli, start=list(What=300, r = 1.2, q = 2, tau = 1, t0=0.1))

### fit qvb function -----

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

fit_qvb(saba7$FL, saba7$Age2,300,1.2,2,1,0.1)

### fit vb function ---

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

#estimate
fit_vb(saba7$FL, saba7$Age2, 400, 0.6, 0)


grapher <-
  ggplot2::ggplot(saba7) +
  ggplot2::geom_point(aes(x=Age2, y=FL, colour = as.factor(Year)), alpha = 0.5) +
  stat_function(
    fun=function(x)p[1] * p[4]^p[2] * (1- (pmax(0,1-(1-p[3])*(x-p[5])/p[4]))^(1/(1-p[3])) )^p[2],
    color="dodgerblue4", size=1.5) +

  stat_function(
    fun=function(x) vbp[1] * (1-exp(-vbp[2]*(x-vbp[3]))),
    color="red", size=1
  )

