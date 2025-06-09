#' White Wine Quality Dataset  
#'  
#' A dataset containing physicochemical properties and sensory quality ratings  
#' of Portuguese "Vinho Verde" white wine samples.  
#'  
#'  
#' @examples  
#' \dontrun{  
#' data(winequality.white)  
#'  
#' # 1. Basic summary statistics  
#' summary(winequality.white)  
#'  
#' # 2. Quality distribution histogram  
#' hist(winequality.white$quality,  
#'      main = "White Wine Quality Distribution",  
#'      xlab = "Quality Rating",  
#'      col = "lightblue")  
#'  
#' # 3. Fixed acidity vs Quality boxplot  
#' boxplot(fixed.acidity ~ quality,  
#'         data = winequality.white,  
#'         xlab = "Quality Rating",  
#'         ylab = "Fixed Acidity (g/dm³)",  
#'         col = "lightgreen")  
#'  
#' # 4. Correlation heatmap (requires `corrplot` package)  
#' if (requireNamespace("corrplot", quietly = TRUE)) {  
#'   key_features <- winequality.white[, c("fixed.acidity", "volatile.acidity",  
#'                                         "citric.acid", "alcohol", "quality")]  
#'   corr_matrix <- cor(key_features, use = "complete.obs")  
#'   corrplot::corrplot(corr_matrix, method = "color", type = "upper",  
#'                      order = "hclust", tl.col = "black")  
#' }  
#' }  
#'  
"winequality.white"  