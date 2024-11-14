#' @name TFM
#' @title The TFM function is to generate Truncated factor model data.
#' @description The TFM function generates truncated factor model data supporting various distribution types for related analyses using multiple methods.
#' @param n Total number of observations.
#' @param mu The mean of the distribution.
#' @param sigma The parameter of the distribution.
#' @param lower The lower bound of the interval.
#' @param upper The upper bound of the interval.
#' @param distribution_type String specifying the distribution type to use.
#' @return A list containing:
#' \item{X}{A matrix of generated truncated factor model data based on the specified distribution type. Each row corresponds to an observation, and each column corresponds to a variable.}
#' @examples
#' library(relliptical)
#' set.seed(123)
#' mu <- c(0, 1)
#' n <- 100
#' sigma <- matrix(c(1, 0.70, 0.70, 3), 2, 2)
#' lower <- c(-2, -3)
#' upper <- c(3, 3)
#' distribution_type <- "truncated_normal"
#' X <- TFM(n, mu, sigma, lower, upper, distribution_type)
#' @export
#' @importFrom relliptical rtelliptical
TFM <- function(n, mu, sigma, lower, upper, distribution_type) {
  switch(distribution_type,
         "truncated_normal" = {
           X = rtelliptical(n, mu, sigma, lower, upper, dist="Normal")
         },
         "truncated_student" = {
           X = rtelliptical(n, mu, sigma, lower, upper, dist="t")
         },
         "truncated_laplace" = {
           X = rtelliptical(n, mu, sigma, lower, upper, dist="Laplace")
         },
         "truncated_power_exponential" = {
           X = rtelliptical(n, mu, sigma, lower, upper, dist="PE")
         },
         "kotz_type" = {
           X = rtelliptical(n, mu, sigma, lower, upper,gFun=function(t){ t^(-1/2)*exp(-2*t^(1/4)) })
         },
         "truncated_slash" = {
           X = rtelliptical(n, mu, sigma, lower, upper, dist="Slash")
         },
         {
           stop("Invalid distribution type")

         }
  )
  return(X)
}
