#' Yacht Hydrodynamics (Residuary Resistance) Dataset  
#'  
#' A dataset for predicting the \strong{residuary resistance} of sailing yachts during early design stages.  
#' It contains hull geometry, operational parameters, and experimental resistance measurements from the Delft Ship Hydromechanics Laboratory.  


#' @format A data frame with 308 observations (rows) and 7 variables:  
#' \describe{  
#'   \item{\code{V1}}{Hull length (\emph{typical unit: meters}). A core geometric parameter that shapes hydrodynamic performance.}  
#'   \item{\code{V2}}{Hull beam (width) (\emph{typical unit: meters}). Influences the yacht’s stability and resistance properties.}  
#'   \item{\code{V3}}{Hull draft (\emph{typical unit: meters}). The depth of the hull beneath the waterline, a vital factor for hydrodynamics.}  
#'   \item{\code{V4}}{Displacement (\emph{typical unit: kilograms or metric tons}). The total mass of the yacht (hull + payload), a key design limitation.}  
#'   \item{\code{V5}}{Trim angle (\emph{typical unit: degrees}). The longitudinal tilt of the hull, which has an impact on resistance and speed.}  
#'   \item{\code{V6}}{Boat velocity (\emph{typical unit: m/s or knots}). The speed of the yacht during resistance testing.}  
#'   \item{\code{V7}}{Residuary resistance (\emph{typical unit: Newtons}). The target variable, representing resistance from wave formation and hull friction (air resistance not included).}  
#' }  
#' @examples  
#' data(yacht_hydrodynamics)  

#' # 1. Basic summary statistics  
#' summary(yacht_hydrodynamics)  

#' # 2. Scatter plot: Velocity vs Residuary Resistance  
#' plot(  
#'   x = yacht_hydrodynamics$V6,  
#'   y = yacht_hydrodynamics$V7,  
#'   xlab = "Boat Velocity",  
#'   ylab = "Residuary Resistance",  
#'   main = "Velocity vs Residuary Resistance"  
#' )  

#' # 3. Correlation matrix (requires the 'corrplot' package)  
#' if (requireNamespace("corrplot", quietly = TRUE)) {  
#'   yacht_corr <- cor(yacht_hydrodynamics, use = "complete.obs")  
#'   corrplot::corrplot(yacht_corr, method = "color", type = "upper", order = "hclust")  
#' }  

#' # 4. Simple linear regression (Resistance ~ Velocity + Length)  
#' model <- lm(V7 ~ V6 + V1, data = yacht_hydrodynamics)  
#' summary(model)  


"yacht_hydrodynamics" 