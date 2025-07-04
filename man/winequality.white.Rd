% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/winequality.white.R
\docType{data}
\name{winequality.white}
\alias{winequality.white}
\title{White Wine Quality Dataset  
 
A dataset containing physicochemical properties and sensory quality ratings  
of Portuguese "Vinho Verde" white wine samples.}
\format{
An object of class \code{data.frame} with 4898 rows and 12 columns.
}
\usage{
winequality.white
}
\description{
White Wine Quality Dataset  
 
A dataset containing physicochemical properties and sensory quality ratings  
of Portuguese "Vinho Verde" white wine samples.
}
\examples{
 
\dontrun{  
data(winequality.white)  
summary(winequality.white)  
hist(winequality.white$quality,  
     main = "White Wine Quality Distribution",  
     xlab = "Quality Rating",  
     col = "lightblue")  
boxplot(fixed.acidity ~ quality,  
        data = winequality.white,  
        xlab = "Quality Rating",  
        ylab = "Fixed Acidity (g/dm³)",  
        col = "lightgreen")  
if (requireNamespace("corrplot", quietly = TRUE)) {  
  key_features <- winequality.white[, c("fixed.acidity", "volatile.acidity",  
                                        "citric.acid", "alcohol", "quality")]  
  corr_matrix <- cor(key_features, use = "complete.obs")  
  corrplot::corrplot(corr_matrix, method = "color", type = "upper",  
                     order = "hclust", tl.col = "black")  
}  
}  
}
\keyword{datasets}
