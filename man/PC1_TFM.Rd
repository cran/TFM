% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PC1_TFM.R
\name{PC1_TFM}
\alias{PC1_TFM}
\title{Apply the PC method to the Truncated factor model}
\usage{
PC1_TFM(data, m, A, D)
}
\arguments{
\item{data}{The total data set to be analyzed.}

\item{m}{The number of principal components to retain in the analysis.}

\item{A}{The true factor loadings matrix.}

\item{D}{The true uniquenesses matrix.}
}
\value{
A list containing:
\item{A1}{Estimated factor loadings.}
\item{D1}{Estimated uniquenesses.}
\item{MSESigmaA}{Mean squared error for factor loadings.}
\item{MSESigmaD}{Mean squared error for uniquenesses.}
\item{LSigmaA}{Loss metric for factor loadings.}
\item{LSigmaD}{Loss metric for uniquenesses.}
}
\description{
This function performs Principal Component Analysis (PCA) on a given data set to reduce dimensionality. It calculates the estimated values for the loadings, specific variances, and the covariance matrix.
}
\examples{
\dontrun{
library(SOPC)
library(relliptical)
library(MASS)
n=1000
p=10
m=5
mu=t(matrix(rep(runif(p,0,1000),n),p,n))
mu0=as.matrix(runif(m,0))
sigma0=diag(runif(m,1))
F=matrix(mvrnorm(n,mu0,sigma0),nrow=n)
A=matrix(runif(p*m,-1,1),nrow=p)
trnor <- relliptical(n*p,0,1)
epsilon=matrix(trnor,nrow=n)
D=diag(t(epsilon)\%*\%epsilon)
data=mu+F\%*\%t(A)+epsilon
results <- PC1_TFM(data, m, A, D)
print(results)}
}
