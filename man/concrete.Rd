\name{concrete}
\alias{concrete}
\docType{data}
\title{Concrete Slump Test Data}
\description{
This dataset contains measurements related to the slump test of concrete, including input variables (concrete ingredients) and output variables (slump, flow, and compressive strength).
}
\usage{
data(concrete)
}
\format{
A data frame with 103 rows and 11 columns.
\describe{
\item{V1}{Amount of cement (kg in one M^3 concrete).}
\item{V2}{Amount of slag (kg in one M^3 concrete).}
\item{V3}{Amount of fly ash (kg in one M^3 concrete).}
\item{V4}{Amount of water (kg in one M^3 concrete).}
\item{V5}{Amount of superplasticizer (kg in one M^3 concrete).}
\item{V6}{Amount of coarse aggregate (kg in one M^3 concrete).}
\item{V7}{Amount of fine aggregate (kg in one M^3 concrete).}
\item{V8}{Slump of the concrete (cm).}
\item{V9}{Flow of the concrete (cm).}
\item{V10}{28-day compressive strength of the concrete (MPa).}
\item{V11}{Additional variable (if applicable).}
}
}
\details{
The dataset includes 7 input variables (concrete ingredients) and 3 output variables (slump, flow, and compressive strength). The initial dataset had 78 data points, with an additional 25 data points added later.
}
\note{
The dataset assumes that all measurements are accurate and does not account for measurement errors. The slump flow of concrete is influenced by multiple factors, including water content and other ingredients.
}
\source{
Donor: I-Cheng Yeh \\
Email: icyeh 'at' chu.edu.tw \\
Institution: Department of Information Management, Chung-Hua University (Republic of China) \\
Other contact information: Department of Information Management, Chung-Hua University, Hsin Chu, Taiwan 30067, R.O.C.
}
\examples{
# Load the dataset
data(concrete)

# Print the first few rows of the dataset
print(head(concrete))
}
\keyword{datasets}