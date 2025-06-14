% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IPC_TFM.R
\name{IPC_TFM}
\alias{IPC_TFM}
\title{Incremental Principal Component Analysis}
\usage{
IPC_TFM(x, m, A, D, p)
}
\arguments{
\item{x}{The data used in the IPC analysis.}

\item{m}{The number of common factors.}

\item{A}{The true factor loadings matrix.}

\item{D}{The true uniquenesses matrix.}

\item{p}{The number of variables.}
}
\value{
A list of metrics including:
\item{Ai}{Estimated factor loadings updated during the IPC analysis, a matrix of estimated factor loadings.}
\item{Di}{Estimated uniquenesses updated during the IPC analysis, a vector of estimated uniquenesses corresponding to each variable.}
\item{MSESigmaA}{Mean squared error of the estimated factor loadings (Ai) compared to the true loadings (A).}
\item{MSESigmaD}{Mean squared error of the estimated uniquenesses (Di) compared to the true uniquenesses (D).}
\item{LSigmaA}{Loss metric for the estimated factor loadings (Ai), indicating the relative error compared to the true loadings (A).}
\item{LSigmaD}{Loss metric for the estimated uniquenesses (Di), indicating the relative error compared to the true uniquenesses (D).}
}
\description{
This function performs Incremental Principal Component Analysis (IPC) on the provided data. It updates the estimated factor loadings and uniquenesses as new data points are processed, calculating mean squared errors and loss metrics for comparison with true values.
}
\examples{
\dontrun{
library(MASS)
library(relliptical)
library(SOPC)

IPC_MSESigmaA = c()
IPC_MSESigmaD = c()
IPC_LSigmaA = c()
IPC_LSigmaD = c()
data_M = data.frame(n = n, MSEA = IPC_MSESigmaA, MSED = IPC_MSESigmaD,
 LSA = IPC_LSigmaA, LSD = IPC_LSigmaD)
print(data_M)}
}
