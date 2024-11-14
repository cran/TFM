#' @name T_test_TFM
#' @title T-test for Truncated Factor Model
#' @description This function performs a simple t-test for each variable in the dataset of a truncated factor model and calculates the False Discovery Rate (FDR) and power.
#' @param X A matrix or data frame of simulated or observed data from a truncated factor model.
#' @param p The number of variables (columns) in the dataset.
#' @param alpha The significance level for the t-test.
#' @return A list containing:
#' \item{FDR}{The False Discovery Rate calculated from the rejected hypotheses.}
#' \item{Power}{The power of the test, representing the proportion of true positives among the non-zero hypotheses.}
#' \item{pValues}{A numeric vector of p-values obtained from the t-tests for each variable.}
#' \item{RejectedHypotheses}{A logical vector indicating which hypotheses were rejected based on the specified significance level.}
#' @export
#' @importFrom stats t.test
#' @examples
#' library(MASS)
#' library(mvtnorm)  # Add this line to load the mvtnorm package
#' set.seed(100)
#' p <- 400
#' n <- 120
#' K <- 5
#' B <- matrix(rnorm(p * K), nrow = p, ncol = K)
#' mu <- c(rep(1, 100), rep(0, p - 100))
#' FX <- MASS::mvrnorm(n, rep(0, K), diag(K))
#' U <- mvtnorm::rmvt(n, df = 3, sigma = diag(p))  # Use mvtnorm::rmvt
#' X <- rep(1, n) %*% t(mu) + FX %*% t(B) + U
#'
#' # Now we can call the function with the simulated data
#' results <- ttestTFM(X, p, alpha = 0.05)
#' print(results)
#' @importFrom MASS mvrnorm
#' @importFrom mvtnorm rmvt

ttestTFM <- function(X, p, alpha = 0.05) {

  # Ensure the MASS package is available
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required but not installed.")
  }

  # Ensure X is a matrix
  X <- as.matrix(X)

  # Check if the number of columns in X matches p
  if (ncol(X) != p) {
    stop("The number of columns in X does not match the specified number of variables p.")
  }

  # Perform a t-test for each variable and extract p-values
  t_tests <- apply(X, 2, function(x) {
    t_test_result <- t.test(x)
    return(t_test_result$p.value)
  })

  # Determine which hypotheses are rejected
  rejected <- t_tests < alpha

  # Assume the number of non-zero hypotheses for calculating power
  true_non_zero <- min(100, p)

  # Calculate FDR and power, handling cases with no rejected hypotheses
  num_rejected <- sum(rejected)
  false_positives <- sum(rejected & (1:p > true_non_zero))
  true_positives <- sum(rejected & (1:p <= true_non_zero))

  fdr <- ifelse(num_rejected > 0, false_positives / num_rejected, 0)
  power <- ifelse(true_non_zero > 0, true_positives / true_non_zero, 0)

  # Return the results
  list(
    FDR = fdr,
    Power = power,
    pValues = t_tests,
    RejectedHypotheses = rejected
  )
}
