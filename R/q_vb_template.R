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
#'               for Ordinary Least Square method and Maximum Likelihood Estimation, respectively
#' @param summary if TRUE, summary is returned along with the estimated parameters
#'
#' @return list
#' @export

# NOTE: length and age must be the same length

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

## example ------


### Data preparation -----

# Required packages

library(stats4)
library(bbmle)
library(ggplot2)

# Read data (Enter full path of the sample.csv file)
sample.data <- read.csv("C:/foo/bar/baz/sample.csv")


# Plot low data
# Set theme for ggplot
ggtheme <-
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = c(0.8, 0.2),
                 legend.title = element_text(size = 16),
                 legend.text  = element_text(size = 14),
                 axis.text = element_text(size = 12),
                 axis.title = element_text(size = 14))
g  <-
  ggplot2::ggplot(data=sample.data) +
  ggplot2::geom_point(aes(x = Age, y = Size), color = "#6baed6") +
  ggtheme

g

### Fitting by OLS-----

# Set initial parameters
Lhat <- 400
r    <- 2
q    <- 3
tau  <- 1
t0   <- -0.1


## Fit function using Ordinary Least Square method (OLS)
res.OLS <- fit.qvb(length = sample.data$Size,
                   age    = sample.data$Age,
                   Lhat   = Lhat,
                   r      = r,
                   q      = q,
                   tau    = tau,
                   t0     = t0,
                   method = "OLS")

# Repeat fit.qvb several times (10 times)
for(i in 1:10){
  res.OLS <- fit.qvb(length = sample.data$Size,
                     age    = sample.data$Age,
                     Lhat   = res.OLS[1],
                     r      = res.OLS[2],
                     q      = res.OLS[3],
                     tau    = res.OLS[4],
                     t0     = res.OLS[5],
                     method = "OLS")
  }

# Draw growth trajectory estimated with OLS
gg <- g +
  ggplot2::stat_function(
    fun = function(x)qvb(age = x,res.OLS[1], res.OLS[2], res.OLS[3], res.OLS[4], res.OLS[5]),
    # color = "#2c7bb6",
    aes(colour = "OLS"),
    size = 1.2) +
  ggtheme

# Draw graph
gg

### Fitting by MLE-----

# Set initial parameters
Lhat <- 200
r    <- 2
q    <- 3
tau  <- 1
t0   <- -0.1

## Fit function using Maximum Likelihood Estimation (MLE)

res.MLE <- fit.qvb(length = sample.data$Size,
                   age    = sample.data$Age,
                   Lhat   = Lhat,
                   r      = r,
                   q      = q,
                   tau    = tau,
                   t0     = t0,
                   method = "MLE")

# Repeat fit.qvb several times (10 times)
for(i in 1:10){
  res.MLE <- fit.qvb(length = sample.data$Size,
                   age    = sample.data$Age,
                   Lhat   = res.MLE[1],
                   r      = res.MLE[2],
                   q      = res.MLE[3],
                   tau    = res.MLE[4],
                   t0     = res.MLE[5],
                   method = "MLE")
  }

# Draw growth trajectory estimated with MLE
ggg <- g +
  ggplot2::stat_function(
    fun = function(x)qvb(age = x,res.MLE[1], res.MLE[2], res.MLE[3], res.MLE[4], res.MLE[5]),
    # color = "#d7191c",
    aes(colour = "MLE"),
    size = 1.2) +
  ggtheme

# Draw graph
ggg

# Draw graph with both OLS and MLE
graph <- g +

  ggplot2::stat_function(
    fun = function(x)qvb(age = x,res.OLS[1], res.OLS[2], res.OLS[3], res.OLS[4], res.OLS[5]),
    # color = "#2c7bb6",
    aes(colour = "OLS"),
    size = 1.2) +

  ggplot2::stat_function(
    fun = function(x)qvb(age = x,res.MLE[1], res.MLE[2], res.MLE[3], res.MLE[4], res.MLE[5]),
    # color = "#d7191c",
    aes(colour = "MLE"),
    size = 1.2) +

  ggplot2::scale_colour_manual("Fitting method",
                               values = c("#2c7bb6", "#d7191c"))

# Draw graph
graph
