#' @name OPC_TFM
#' @title Apply the OPC method to the Truncated factor model
#' @description This function computes Online Principal Component Analysis (OPC) for the provided input data, estimating factor loadings and uniquenesses. It calculates mean squared errors and sparsity for the estimated values compared to true values.
#' @param data A matrix of input data.
#' @param m The number of principal components.
#' @param A The true factor loadings matrix.
#' @param D The true uniquenesses matrix.
#' @param p The number of variables.
#' @return A list containing:
#' \item{Ao}{Estimated factor loadings.}
#' \item{Do}{Estimated uniquenesses.}
#' \item{MSEA}{Mean squared error for factor loadings.}
#' \item{MSED}{Mean squared error for uniquenesses.}
#' \item{tau}{The sparsity.}
#' @examples
#' \dontrun{
#' library(SOPC)
#' library(relliptical)
#' library(MASS)
#' results <- OPC_TFM(data, m, A, D, p)
#' print(results)}
#' @export
#' @importFrom SOPC PPC
#' @importFrom matrixcalc frobenius.norm
#' @importFrom SOPC OPC

OPC_TFM<- function(data,m=m, A, D, p){
  Ao=OPC(data,m=m,eta=0.5)$Ao
  Do=OPC(data,m=m,eta=0.5)$Do
  MSEA=frobenius.norm(Ao-A)^2/(p^2)
  MSED=frobenius.norm(Do-diag(D))^2/(p^2)
  tau=as.vector(table(Ao==0)/(p*m))[2]
  
  return(list(Ao = Ao,
              Do = Do,
              MSEA = MSEA,
              MSED = MSED,
              tau = tau))
}
