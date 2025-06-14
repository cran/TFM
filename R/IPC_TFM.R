#' @name IPC_TFM
#' @title Incremental Principal Component Analysis
#' @description This function performs Incremental Principal Component Analysis (IPC) on the provided data. It updates the estimated factor loadings and uniquenesses as new data points are processed, calculating mean squared errors and loss metrics for comparison with true values.
#' @param x The data used in the IPC analysis.
#' @param m The number of common factors.
#' @param A The true factor loadings matrix.
#' @param D The true uniquenesses matrix.
#' @param p The number of variables.
#' @return A list of metrics including:
#' \item{Ai}{Estimated factor loadings updated during the IPC analysis, a matrix of estimated factor loadings.}
#' \item{Di}{Estimated uniquenesses updated during the IPC analysis, a vector of estimated uniquenesses corresponding to each variable.}
#' \item{MSESigmaA}{Mean squared error of the estimated factor loadings (Ai) compared to the true loadings (A).}
#' \item{MSESigmaD}{Mean squared error of the estimated uniquenesses (Di) compared to the true uniquenesses (D).}
#' \item{LSigmaA}{Loss metric for the estimated factor loadings (Ai), indicating the relative error compared to the true loadings (A).}
#' \item{LSigmaD}{Loss metric for the estimated uniquenesses (Di), indicating the relative error compared to the true uniquenesses (D).}
#' @examples
#' \dontrun{
#' library(MASS)
#' library(relliptical)
#' library(SOPC)
#'
#' IPC_MSESigmaA = c()
#' IPC_MSESigmaD = c()
#' IPC_LSigmaA = c()
#' IPC_LSigmaD = c()
#' 
#' Z = data.frame(IPC_TFM(data, m = m, A = A, D = D, p = p))[c(3, 4, 5, 6),]
#' IPC_MSESigmaA = Z[1]
#' IPC_MSESigmaD = Z[2]
#' IPC_LSigmaA = Z[3]
#' IPC_LSigmaD = Z[4]
#'
#' data_M = data.frame(n = n, MSEA = IPC_MSESigmaA, MSED = IPC_MSESigmaD,
#'  LSA = IPC_LSigmaA, LSD = IPC_LSigmaD)
#' print(data_M)}
#'
#'
#' @export
#' @importFrom SOPC IPC
# IPC_print function
IPC_TFM <- function(x, m, A, D, p) {
  # Define the Frobenius norm function
  frobenius.norm <- function(matrix) {
    return(norm(matrix, type = "F"))
  }
  Ai = IPC(data = x, m = m, eta = 0.8)$Ai
  Di = IPC(data = x, m = m, eta = 0.8)$Di
  MSESigmaA = frobenius.norm(Ai - A)^2 / (p^2)
  MSESigmaD = frobenius.norm(Di - D)^2 / (p^2)
  LSigmaA = frobenius.norm(Ai - A)^2 / frobenius.norm(A)^2
  LSigmaD = frobenius.norm(Di - D)^2 / frobenius.norm(D)^2
  return(c('Ai' = Ai,
           'Di' = Di,
           'MSESigmaA' = MSESigmaA,
           'MSESigmaD' = MSESigmaD,
           'LSigmaA' = LSigmaA,
           'LSigmaD' = LSigmaD))
}

