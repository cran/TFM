\name{review}
\alias{review}
\docType{data}
\title{Travel Review Dataset}
\description{
This dataset contains travel reviews from TripAdvisor.com, covering destinations in 10 categories across East Asia. Each traveler's rating is mapped to a scale from Terrible (0) to Excellent (4), and the average rating for each category per user is provided.
}
\usage{
data(review)
}
\format{
A data frame with multiple rows and 10 columns.
\describe{
  \item{\code{1}}{Unique identifier for each user (Categorical)}
  \item{\code{2}}{Average user feedback on art galleries}
  \item{\code{3}}{Average user feedback on dance clubs}
  \item{\code{4}}{Average user feedback on juice bars}
  \item{\code{5}}{Average user feedback on restaurants}
  \item{\code{6}}{Average user feedback on museums}
  \item{\code{7}}{Average user feedback on resorts}
  \item{\code{8}}{Average user feedback on parks and picnic spots}
  \item{\code{9}}{Average user feedback on beaches}
  \item{\code{10}}{Average user feedback on theaters}
}
}
\details{
The dataset is populated by crawling TripAdvisor.com and includes reviews on destinations in 10 categories across East Asia. Each traveler's rating is mapped as follows: 
  \itemize{
    \item Excellent (4)
    \item Very Good (3)
    \item Average (2)
    \item Poor (1)
    \item Terrible (0)
  }
The average rating for each category per user is used.
}
\note{
This dataset is licensed under a Creative Commons Attribution 4.0 International (CC BY 4.0) license.
}
\source{
UCI Machine Learning Repository
}
\examples{
data(review)
head(review)
review$`1`  # User IDs
mean(review$`5`)  # Average rating for restaurants
}
\keyword{datasets}