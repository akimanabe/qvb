#' Calculate log likelihood
#'
#' @param length Length data
#' @param age Age data
#' @param p list
#'  \describe{
#'  \item{Linf}{Parameter L_inf}
#'  \item{r}{Initial parameter for r}
#'  \item{q}{Initial parameter for q \code{0 < q < 1} or \code{1 < q}}
#'  \item{tau}{Initial parameter for tau}
#'  \item{t0}{Initial parameter for t0}
#'  }
#' @param takelog Set log = TRUE for log_likelihood

loglike <- function(length, age,
                    p = list(Linf, r, q, tau, t0),takelog = TRUE){
  -sum(stats::dnorm(qvb(age, p = p)), log = takelog)
}

