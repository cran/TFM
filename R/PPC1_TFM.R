#' @name PPC1_TFM
#' @title Projected Principal Component Analysis
#' @description This function computes Projected Principal Component Analysis (PPC) for the provided input data, estimating factor loadings and uniquenesses. It calculates mean squared errors and loss metrics for the estimated values compared to true values.
#' @param x A matrix of input data.
#' @param m The number of principal components to extract (integer).
#' @param A The true factor loadings matrix (matrix).
#' @param D The true uniquenesses matrix (matrix).
#' @param p The number of variables (integer).
#' @return A list containing:
#' \item{Ap}{Estimated factor loadings.}
#' \item{Dp}{Estimated uniquenesses.}
#' \item{MSESigmaA}{Mean squared error for factor loadings.}
#' \item{MSESigmaD}{Mean squared error for uniquenesses.}
#' \item{LSigmaA}{Loss metric for factor loadings.}
#' \item{LSigmaD}{Loss metric for uniquenesses.}
#' @examples
#' \dontrun{
#' library(MASS)
#' library(relliptical)
#' library(SOPC)
#'
#' PPC_MSESigmaA <- c()
#' PPC_MSESigmaD <- c()
#' PPC_LSigmaA <- c()
#' PPC_LSigmaD <- c()
#'
#' result <- PPC1_TFM(data, m, A, D, p)
#'
#' data_G <- data.frame(n = n,
#'                      MSEA = result$MSESigmaA,
#'                      MSED = result$MSESigmaD,
#'                      LSA = result$LSigmaA,
#'                      LSD = result$LSigmaD)
#'
#' print(data_G)}
#' @export
#' @importFrom SOPC PPC
PPC1_TFM <- function(x, m, A, D, p) {
  frobenius.norm <- function(matrix) {
    return(norm(matrix, type = "F"))
  }
  Ap = PPC(data = x, m = m, eta = 0.8)$Ap
  Dp = PPC(data = x, m = m, eta = 0.8)$Dp
  MSESigmaA = frobenius.norm(Ap - A)^2 / (p^2)
  MSESigmaD = frobenius.norm(Dp - D)^2 / (p^2)
  LSigmaA = frobenius.norm(Ap - A)^2 / frobenius.norm(A)^2
  LSigmaD = frobenius.norm(Dp - D)^2 / frobenius.norm(D)^2

  return(list(Ap = Ap,
              Dp = Dp,
              MSESigmaA = MSESigmaA,
              MSESigmaD = MSESigmaD,
              LSigmaA = LSigmaA,
              LSigmaD = LSigmaD))
}

