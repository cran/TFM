#' Taxi Trip Pricing Dataset
#'
#' A dataset containing various factors related to taxi trips and their corresponding prices.
#'
#' @format A data frame with `n` observations (rows, where `n` is the number of taxi trips) and 11 variables:
#' \describe{
#'   \item{\code{Trip_Distance_km}}{Trip distance in kilometers. This variable measures how far the taxi has traveled.}
#'   \item{\code{Time_of_Day}}{A categorization of the time of the day (e.g., 1 might represent morning, 2 afternoon, etc.). It can potentially affect pricing due to demand patterns.}
#'   \item{\code{Day_of_Week}}{The day of the week (0 - 6, where 0 could represent Sunday). Weekend vs. weekday trips might have different pricing considerations.}
#'   \item{\code{Passenger_Count}}{Number of passengers in the taxi. It could influence the pricing structure in some taxi systems.}
#'   \item{\code{Traffic_Conditions}}{A measure of traffic conditions (e.g., 1 for light traffic, 4 for heavy traffic). Traffic can impact trip duration and thus price.}
#'   \item{\code{Weather}}{A classification of weather conditions (e.g., 1 for clear, 3 for rainy). Weather might have an impact on demand and thus pricing.}
#'   \item{\code{Base_Fare}}{The base fare amount for the taxi trip. This is a fixed component of the price.}
#'   \item{\code{Per_Km_Rate}}{The rate charged per kilometer traveled.}
#'   \item{\code{Per_Minute_Rate}}{The rate charged per minute of the trip (usually applicable when the taxi is idling or in slow - moving traffic).}
#'   \item{\code{Trip_Duration_Minutes}}{The duration of the trip in minutes.}
#'   \item{\code{Trip_Price}}{The final price of the taxi trip.}
#' }
#'
#'
#' @examples
#' data(taxi_trip_pricing)
#'
#' # Basic summary statistics
#' summary(taxi_trip_pricing)
#'
#' # Scatter plot: Trip Distance vs. Trip Price
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(taxi_trip_pricing, ggplot2::aes(x = Trip_Distance_km, y = Trip_Price)) +
#'     ggplot2::geom_point() +
#'     ggplot2::labs(x = "Trip Distance (km)", y = "Trip Price")
#' }
#'
"taxi_trip_pricing"