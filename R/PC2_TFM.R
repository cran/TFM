#' @name PC2_TFM
#' @title Apply the PC method to the Truncated factor model
#' @description This function performs Principal Component Analysis (PCA) on a given data set to reduce dimensionality. It calculates the estimated values for the loadings, specific variances, and the covariance matrix.
#' @param data The total data set to be analyzed.
#' @param m The number of principal components to retain in the analysis.
#' @param A The true factor loadings matrix.
#' @param D The true uniquenesses matrix.
#' @return A list containing:
#' \item{A2}{Estimated factor loadings.}
#' \item{D2}{Estimated uniquenesses.}
#' \item{MSESigmaA}{Mean squared error for factor loadings.}
#' \item{MSESigmaD}{Mean squared error for uniquenesses.}
#' \item{LSigmaA}{Loss metric for factor loadings.}
#' \item{LSigmaD}{Loss metric for uniquenesses.}
#' @examples
#' \dontrun{
#' library(SOPC)
#' library(relliptical)
#' library(MASS)
#' results <- PC2_TFM(data, m, A, D)
#' print(results)}
#' @export
#' @importFrom matrixcalc frobenius.norm
#' @importFrom stats cor
PC2_TFM<- function(data, m, A, D) {
  X<-scale(data)
  R<-cor(X)
  S<-R
  eig<-eigen(S)
  p<-nrow(S)
  diag_S<-diag(S)
  sum_rank<-sum(diag_S)
  rowname<-paste("X",1:p,sep="")
  colname<-paste("Factor",1:m,sep="")
  Ahat<-matrix(0,nrow=p,ncol=m, dimnames=list(rowname,colname))
  rowname<-c("SS loadings","Proportion Var","Cumulative Var")
  for (i in 1:m){
    Ahat[,i]<-sqrt(eig$values[i])*eig$vectors[,i]
  }
  h2<-diag(Ahat%*%t(Ahat))
  Dhat<-diag(S-h2)
  
  MSESigmaA = frobenius.norm(Ahat - A)^2 / (p^2)
  MSESigmaD = frobenius.norm(Dhat - D)^2 / (p^2)
  LSigmaA = frobenius.norm(Ahat - A)^2 / frobenius.norm(A)^2
  LSigmaD = frobenius.norm(Dhat - D)^2 / frobenius.norm(D)^2
  return(list(A2 = Ahat,
              D2 = Dhat,
              MSESigmaA = MSESigmaA,
              MSESigmaD = MSESigmaD,
              LSigmaA = LSigmaA,
              LSigmaD = LSigmaD))
}