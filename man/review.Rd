\name{review}
\alias{review}
\docType{data}
\title{Review}
\description{
This dataset contains travel reviews from TripAdvisor.com, covering destinations in 11 categories across East Asia. Each traveler's rating is mapped to a scale from Terrible (0) to Excellent (4), and the average rating for each category per user is provided.
}
\usage{
data(review)
}
\format{
A data frame with multiple rows and 11 columns.
\describe{
\item{V1}{Unique identifier for each user (Categorical).}
\item{V2}{Average user feedback on art galleries.}
\item{V3}{Average user feedback on dance clubs.}
\item{V4}{Average user feedback on juice bars.}
\item{V5}{Average user feedback on restaurants.}
\item{V6}{Average user feedback on museums.}
\item{V7}{Average user feedback on resorts.}
\item{V8}{Average user feedback on parks and picnic spots.}
\item{V9}{Average user feedback on beaches.}
\item{V10}{Average user feedback on theaters.}
\item{V11}{Average user feedback on religious institutions.}
}
}
\details{
The dataset is populated by crawling TripAdvisor.com and includes reviews on destinations in 11 categories across East Asia. Each traveler's rating is mapped as follows: Excellent (4), Very Good (3), Average (2), Poor (1), and Terrible (0). The average rating for each category per user is used.
}
\note{
This dataset is licensed under a Creative Commons Attribution 4.0 International (CC BY 4.0) license, which allows for sharing and adaptation of the data for any purpose, provided that appropriate credit is given.
}
\source{
UCI Machine Learning Repository
}
\examples{
# Load the dataset
data(review)

# Print the first few rows of the dataset
print(head(review))
}
\keyword{datasets}