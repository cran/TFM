#' @name SOPC
#' @title Sparse Online Principal Component Analysis
#' @description This function calculates various metrics for the Sparse Online Principal Component Analysis (SOPC) method. It estimates the factor loadings and uniquenesses while calculating mean squared errors and loss metrics for comparison with true values. Additionally, it computes the proportion of zero factor loadings in the estimated loadings matrix.
#' @param data The data used in the SOPC analysis.
#' @param m the number of common factors
#' @param p the number of variables
#' @param gamma Tuning parameter for the sparseness of the loadings matrix.
#' @param eta Tuning parameter for the sparseness of the uniquenesses matrix.
#' @param A The true A matrix.
#' @param D The true D matrix.
#' @usage SOPC_Print(data, m, p, gamma, eta, A, D)
#' @importFrom SOPC SOPC
#' @return A list of metrics including:
#' \item{Aso}{Estimated factor loadings matrix obtained from the SOPC analysis.}
#' \item{Dso}{Estimated uniquenesses vector obtained from the SOPC analysis.}
#' \item{MSEA}{Mean squared error of the estimated factor loadings (Aso) compared to the true loadings (A).}
#' \item{MSED}{Mean squared error of the estimated uniquenesses (Dso) compared to the true uniquenesses (D).}
#' \item{LSA}{Loss metric for the estimated factor loadings (Aso), indicating the relative error compared to the true loadings (A).}
#' \item{LSD}{Loss metric for the estimated uniquenesses (Dso), indicating the relative error compared to the true uniquenesses (D).}
#' \item{tauA}{Proportion of zero factor loadings in the estimated loadings matrix (Aso), indicating the sparsity of the loadings.}
#' @examples
#' library(MASS)
#' library(relliptical)
#' library(SOPC)
#'
#' SOPC_MSEA <- c()
#' SOPC_MSED <- c()
#' SOPC_LSA <- c()
#' SOPC_LSD <- c()
#' SOPC_TAUA <- c()
#'
#' p = 10; m = 5
#' n = 2000  # Set n to 2000
#' mu = t(matrix(rep(runif(p, 0, 1000), n), p, n))
#' mu0 = as.matrix(runif(m, 0))
#' sigma0 = diag(runif(m, 1))
#' F = matrix(mvrnorm(n, mu0, sigma0), nrow = n)
#' A = matrix(runif(p * m, -1, 1), nrow = p)
#'
#' # Sampling from the Truncated Normal distribution
#' lower = c(rep(-0.5, p - 3), -5, -5, -Inf)
#' upper = c(rep(0.5, p - 3), 5, 5, Inf)
#' Sigma = as.matrix(diag(rep(runif(p, 0, 1))))
#' mut = runif(p, 0, 10)
#' trnor = rtelliptical(n, mut, Sigma, lower, upper, dist = "Normal")
#' epsilon = matrix(trnor, nrow = n)
#' D = Sigma
#'
#' data = mu + F %*% t(A) + epsilon
#'
#' Z = data.frame(SOPC_Print(data, m = m, p = p, gamma = 0.1, eta = 0.8, A = A, D = D))
#' SOPC_MSEA = c(SOPC_MSEA, Z$MSEA)
#' SOPC_MSED = c(SOPC_MSED, Z$MSED)
#' SOPC_LSA = c(SOPC_LSA, Z$LSA)
#' SOPC_LSD = c(SOPC_LSD, Z$LSD)
#' SOPC_TAUA = c(SOPC_TAUA, Z$tauA)
#'
#' # Ensure the data frame has the correct column structure, even with one value
#' data_F = data.frame(n = rep(n, length(SOPC_MSEA)), MSEA = SOPC_MSEA, MSED = SOPC_MSED,
#'  LSA = SOPC_LSA, LSD = SOPC_LSD, tauA = SOPC_TAUA)
#' data_F
#' @export

SOPC_Print <- function(data, m, p, gamma, eta, A, D) {
  # Calculate Frobenius norm
  frobenius.norm <- function(matrix) {
    norm(matrix, type = "F")
  }

  # Perform SOPC analysis with given parameters
  result <- SOPC(data, m = m, gamma = gamma, eta = eta)
  Aso <- result$Aso
  Dso <- result$Dso

  # Calculate metrics
  MSEA <- frobenius.norm(Aso - A)^2 / (p^2)
  MSED <- frobenius.norm(Dso - D)^2 / (p^2)
  LSA <- frobenius.norm(Aso - A)^2 / frobenius.norm(A)^2
  LSD <- frobenius.norm(Dso - D)^2 / frobenius.norm(D)^2

  # Calculate proportion of zero factor loadings
  zero_count <- sum(Aso == 0)
  tauA <- ifelse(zero_count > 0, zero_count / (p * m), 0)

  # Return results as a list
  return(list(Aso = Aso, Dso = Dso, MSEA = MSEA, MSED = MSED, LSA = LSA, LSD = LSD, tauA = tauA))
}

