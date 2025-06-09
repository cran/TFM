#' Real Estate Valuation Dataset  
#'  
#' A dataset containing features related to residential property valuation in New Taipei City, Taiwan,  
#' including house age, proximity to transit (MRT), local amenities (convenience stores), geographic coordinates, and the target valuation.  
#'  
#' @format A data frame with 414 observations (rows) and 6 variables:  
#' \describe{  
#'   \item{\code{X1 house age}}{House age (\emph{years}). Older properties typically experience depreciation, which influences valuation.}  
#'   \item{\code{X2 distance to the nearest MRT station}}{Distance to the nearest MRT (Mass Rapid Transit) station (\emph{meters}). Closer transit access generally increases property value.}  
#'   \item{\code{X3 number of convenience stores}}{Number of nearby convenience stores. More amenities correlate with higher desirability and valuation.}  
#'   \item{\code{X4 latitude}}{Latitude (\emph{decimal degrees}). Geographic coordinate for spatial analysis of location-based value trends.}  
#'   \item{\code{X5 longitude}}{Longitude (\emph{decimal degrees}). Geographic coordinate for spatial analysis of location-based value trends.}  
#'   \item{\code{Y}}{Real estate valuation (\emph{10,000 New Taiwan Dollars per ping}). The target variable representing property value.  
#'   (\emph{Note}: 1 ping = 3.3058 m², a local unit of area in Taiwan.)}  
#' }  
#'  
"real_estate_valuation" 