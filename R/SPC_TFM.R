#' @name SPC_TFM
#' @title Sparse Principal Component Analysis
#' @description This function performs Sparse Principal Component Analysis (SPC) on the input data. It estimates factor loadings and uniquenesses while calculating mean squared errors and loss metrics for comparison with true values. Additionally, it computes the proportion of zero factor loadings.
#' @param data The data used in the SPC analysis.
#' @param A The true factor loadings matrix.
#' @param D The true uniquenesses matrix.
#' @param m The number of common factors.
#' @param p The number of variables.
#' @return A list containing:
#' \item{As}{Estimated factor loadings, a matrix of estimated factor loadings from the SPC analysis.}
#' \item{Ds}{Estimated uniquenesses, a vector of estimated uniquenesses corresponding to each variable.}
#' \item{MSESigmaA}{Mean squared error of the estimated factor loadings (As) compared to the true loadings (A).}
#' \item{MSESigmaD}{Mean squared error of the estimated uniquenesses (Ds) compared to the true uniquenesses (D).}
#' \item{LSigmaA}{Loss metric for the estimated factor loadings (As), indicating the relative error compared to the true loadings (A).}
#' \item{LSigmaD}{Loss metric for the estimated uniquenesses (Ds), indicating the relative error compared to the true uniquenesses (D).}
#' \item{tau}{Proportion of zero factor loadings in the estimated loadings matrix (As).}
#' @examples
#' \dontrun{
#' library(MASS)
#' library(relliptical)
#' library(SOPC)
#'
#' SPC_MSESigmaA <- c()
#' SPC_MSESigmaD <- c()
#' SPC_LSigmaA <- c()
#' SPC_LSigmaD <- c()
#' SPC_tau <- c()
#'
#' result <- SPC_TFM(data, A, D, m, p)
#'
#' SPC_MSESigmaA <- c(SPC_MSESigmaA, result$MSESigmaA)
#' SPC_MSESigmaD <- c(SPC_MSESigmaD, result$MSESigmaD)
#' SPC_LSigmaA <- c(SPC_LSigmaA, result$LSigmaA)
#' SPC_LSigmaD <- c(SPC_LSigmaD, result$LSigmaD)
#' SPC_tau <- c(SPC_tau, result$tau)
#'
#' data_G <- data.frame(n = n,
#'                      MSEA = SPC_MSESigmaA,
#'                      MSED = SPC_MSESigmaD,
#'                      LSA = SPC_LSigmaA,
#'                      LSD = SPC_LSigmaD,
#'                      tau = SPC_tau)
#'
#' print(data_G)}
#' @export
#' @importFrom SOPC SPC
SPC_TFM <- function(data, A, D, m, p) {
  frobenius.norm <- function(matrix) {
    return(norm(matrix, type = "F"))
  }

  As <- SPC(data, m = m, gamma = 0.1)$As
  Ds <- SPC(data, m = m, gamma = 0.1)$Ds

  MSESigmaA <- frobenius.norm(As - A)^2 / (p^2)
  MSESigmaD <- frobenius.norm(Ds - D)^2 / (p^2)

  LSigmaA <- frobenius.norm(As - A)^2 / frobenius.norm(A)^2
  LSigmaD <- frobenius.norm(Ds - D)^2 / frobenius.norm(D)^2

  tau <- as.vector(table(As == 0) / (p * m))[2]

  return(list('As' = As,
              'Ds' = Ds,
              'MSESigmaA' = MSESigmaA,
              'MSESigmaD' = MSESigmaD,
              'LSigmaA' = LSigmaA,
              'LSigmaD' = LSigmaD,
              'tau' = tau))
}

