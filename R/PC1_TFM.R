#' @name PC1_TFM
#' @title Apply the PC method to the Truncated factor model
#' @description This function performs Principal Component Analysis (PCA) on a given data set to reduce dimensionality. It calculates the estimated values for the loadings, specific variances, and the covariance matrix.
#' @param data The total data set to be analyzed.
#' @param m The number of principal components to retain in the analysis.
#' @param A The true factor loadings matrix.
#' @param D The true uniquenesses matrix.
#' @return A list containing:
#' \item{A1}{Estimated factor loadings.}
#' \item{D1}{Estimated uniquenesses.}
#' \item{MSESigmaA}{Mean squared error for factor loadings.}
#' \item{MSESigmaD}{Mean squared error for uniquenesses.}
#' \item{LSigmaA}{Loss metric for factor loadings.}
#' \item{LSigmaD}{Loss metric for uniquenesses.}
#' @examples
#' \dontrun{
#' library(SOPC)
#' library(relliptical)
#' library(MASS)
#' results <- PC1_TFM(data, m, A, D)
#' print(results)}
#' @export
#' @importFrom matrixcalc frobenius.norm
#' @importFrom stats cov 
#' @import corrplot
#' @import ggplot2
PC1_TFM <- function(data, m, A, D) {
  X <- scale(data, scale = FALSE)
  p <- ncol(X)
  n <- nrow(X)
  Sigmahat <- cov(X)
  eig <- eigen(Sigmahat)
  lambdahat <- eig$values[1:m]
  ind <- order(lambdahat, decreasing = TRUE)
  lambdahat <- lambdahat[ind]
  Q <- eig$vectors
  Q <- Q[, ind]
  Qhat <- Q[, 1:m]
  Ahat <- matrix(0, nrow = p, ncol = m)
  for (j in 1:m) {
    Ahat[, j] <- sqrt(lambdahat[j]) * Qhat[, j]
  }
  h0 <- diag(Ahat %*% t(Ahat))
  Dhat <- diag(Sigmahat - h0)
  S2 <- Ahat %*% t(Ahat) + Dhat
  
  MSESigmaA = frobenius.norm(Ahat - A)^2 / (p^2)
  MSESigmaD = frobenius.norm(Dhat - D)^2 / (p^2)
  LSigmaA = frobenius.norm(Ahat - A)^2 / frobenius.norm(A)^2
  LSigmaD = frobenius.norm(Dhat - D)^2 / frobenius.norm(D)^2
  return(list(A1 = Ahat,
              D1 = Dhat,
              MSESigmaA = MSESigmaA,
              MSESigmaD = MSESigmaD,
              LSigmaA = LSigmaA,
              LSigmaD = LSigmaD))
}
