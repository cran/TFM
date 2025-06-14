#' @name FanPC_TFM
#' @title Apply the FanPC method to the Truncated factor model
#' @description This function performs Factor Analysis via Principal Component (FanPC) on a given data set. It calculates the estimated factor loading matrix (AF), specific variance matrix (DF), and the mean squared errors.
#' @param data A matrix of input data.
#' @param m The number of principal components.
#' @param A The true factor loadings matrix.
#' @param D The true uniquenesses matrix.
#' @param p The number of variables.
#' @return A list containing:
#' \item{AF}{Estimated factor loadings.}
#' \item{DF}{Estimated uniquenesses.}
#' \item{MSESigmaA}{Mean squared error for factor loadings.}
#' \item{MSESigmaD}{Mean squared error for uniquenesses.}
#' \item{LSigmaA}{Loss metric for factor loadings.}
#' \item{LSigmaD}{Loss metric for uniquenesses.}
#' @examples
#' \dontrun{
#' library(SOPC)
#' library(relliptical)
#' library(MASS)
#' results <- FanPC_TFM(data, m, A, D, p)
#' print(results)
#' }
#' @export
#' @importFrom SOPC PPC
#' @importFrom matrixcalc frobenius.norm
FanPC_TFM <- function(data, m, A, D,p) {
  # Validate input parameters
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data frame.")
  }
  if (!is.numeric(m) || m <= 0 || m > ncol(data)) {
    stop("The number of principal components 'm' must be a positive integer not exceeding the number of columns in data.")
  }
  
  # Standardize the data to have zero mean and unit variance
  X <- scale(data)
  
  # Get the number of observations
  n <- nrow(X)
  
  # Calculate the correlation matrix of the standardized data
  SigmahatF <- cor(X)
  
  # Perform eigenvalue decomposition on the correlation matrix
  eig <- eigen(SigmahatF)
  lambdahat <- eig$values[1:m]  # Extract the first 'm' eigenvalues
  ind <- order(lambdahat, decreasing = TRUE)  # Sort the eigenvalues in descending order
  lambdahat <- lambdahat[ind]
  Q <- eig$vectors  # Extract the eigenvectors
  Q <- Q[, ind]
  AF <- Q[, 1:m]  # Select the first 'm' eigenvectors
  
  # Calculate the hat matrix (projection matrix) and the specific variance matrix
  hF <- diag(AF %*% t(AF))
  DF <- diag(SigmahatF - hF)
  
  MSESigmaA = frobenius.norm(AF - A)^2 / (p^2)
  MSESigmaD = frobenius.norm(DF - D)^2 / (p^2)
  LSigmaA = frobenius.norm(AF - A)^2 / frobenius.norm(A)^2
  LSigmaD = frobenius.norm(DF - D)^2 / frobenius.norm(D)^2
  # Return the results as a list
  return(list(AF = AF,
              DF = DF,
              MSESigmaA = MSESigmaA,
              MSESigmaD = MSESigmaD,
              LSigmaA = LSigmaA,
              LSigmaD = LSigmaD))
}