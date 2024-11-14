#' @name SAPC
#' @title Stochastic Approximation Principal Component Analysis
#' @description This function calculates several metrics for the SAPC method,
#' including the estimated factor loadings and uniquenesses, and various
#' error metrics comparing the estimated matrices with the true matrices.
#' @param x The data used in the SAPC analysis.
#' @param m The number of common factors.
#' @param A The true factor loadings matrix.
#' @param D The true uniquenesses matrix.
#' @param p The number of variables.
#' @return A list of metrics including:
#' \item{Asa}{Estimated factor loadings matrix obtained from the SAPC analysis.}
#' \item{Dsa}{Estimated uniquenesses vector obtained from the SAPC analysis.}
#' \item{MSESigmaA}{Mean squared error of the estimated factor loadings (Asa) compared to the true loadings (A).}
#' \item{MSESigmaD}{Mean squared error of the estimated uniquenesses (Dsa) compared to the true uniquenesses (D).}
#' \item{LSigmaA}{Loss metric for the estimated factor loadings (Asa), indicating the relative error compared to the true loadings (A).}
#' \item{LSigmaD}{Loss metric for the estimated uniquenesses (Dsa), indicating the relative error compared to the true uniquenesses (D).}
#' @importFrom SOPC SAPC
#' @examples
#' library(MASS)
#' library(relliptical)
#' library(SOPC)
#'
#' SAPC_MSESigmaA <- c()
#' SAPC_MSESigmaD <- c()
#' SAPC_LSigmaA <- c()
#' SAPC_LSigmaD <- c()
#'
#' p <- 10
#' m <- 5
#' n <- 2000
#'
#' mu <- t(matrix(rep(runif(p, 0, 1000), n), p, n))
#' mu0 <- as.matrix(runif(m, 0))
#' sigma0 <- diag(runif(m, 1))
#' F <- matrix(mvrnorm(n, mu0, sigma0), nrow = n)
#' A <- matrix(runif(p * m, -1, 1), nrow = p)
#'
#' lower <- c(rep(-0.5, p - 3), -5, -5, -Inf)
#' upper <- c(rep(0.5, p - 3), 5, 5, Inf)
#' Sigma <- diag(runif(p, 0, 1))
#' mut <- runif(p, 0, 10)
#' trnor <- rtelliptical(n, mut, Sigma, lower, upper, dist = "Normal")
#' epsilon <- matrix(trnor, nrow = n)
#' D <- Sigma
#'
#' data <- mu + F %*% t(A) + epsilon
#'
#' result <- SAPC_print(data, m = m, A = A, D = D, p = p)
#'
#' SAPC_MSESigmaA <- result$MSESigmaA
#' SAPC_MSESigmaD <- result$MSESigmaD
#' SAPC_LSigmaA <- result$LSigmaA
#' SAPC_LSigmaD <- result$LSigmaD
#'
#' data_K <- data.frame(
#'   n = n,
#'   MSEA = SAPC_MSESigmaA,
#'   MSED = SAPC_MSESigmaD,
#'   LSA = SAPC_LSigmaA,
#'   LSD = SAPC_LSigmaD
#' )
#'
#' print(data_K)
#' @export
SAPC_print <- function(x, m, A, D, p) {
  frobenius.norm <- function(matrix) {
    return(norm(matrix, type = "F"))
  }

  Asa = SAPC(data = x, m = m, eta = 0.8)$Asa
  Dsa = SAPC(data = x, m = m, eta = 0.8)$Dsa

  MSESigmaA = frobenius.norm(Asa - A)^2 / (p^2)
  MSESigmaD = frobenius.norm(Dsa - D)^2 / (p^2)

  LSigmaA = frobenius.norm(Asa - A)^2 / frobenius.norm(A)^2
  LSigmaD = frobenius.norm(Dsa - D)^2 / frobenius.norm(D)^2

  return(list('Asa' = Asa, 'Dsa' = Dsa, 'MSESigmaA' = MSESigmaA, 'MSESigmaD' = MSESigmaD, 'LSigmaA' = LSigmaA, 'LSigmaD' = LSigmaD))
}
