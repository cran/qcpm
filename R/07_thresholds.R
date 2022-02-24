#' @title Admisibles quantile thresholds
#'
#' @description
#' \code{thresholds} provides the maximum and minimum admisible quantile threshold.
#'
#' @details
#' The argument \code{x} is data frame that contains the manifest variables used to
#' estimate the qcpm models 
#' 
#'
#' @param x  is a data frame that contains the manifest variables used to
#' estimate the qcpm models
#' @param \dots Further arguments passed on to \code{thresholds}. 
#' 
#' @return A vector containing the maximum and minimum admisible quantile threshold values.
#' 
#' @author Cristina Davino, Pasquale Dolce, Giuseppe Lamberti, Domenico Vistocco
#'
#' 
#' 
#' @references Davino, C., Dolce, P., Taralli, S., Vistocco, D. (2020)  Composite-Based Path 
#' Modeling for Conditional Quantiles Prediction. An Application to Assess 
#' Health Differences at Local Level in a Well-Being Perspective. 
#' \emph{Social Indicator Research}, pp. 1-30, doi:10.1007/s11205-020-02425-5.
#' 
#' @references Davino, C., Vinzi, V.E. (2016) Quantile composite-based path
#' modeling. \emph{Advansed Data Analysis and Classification}, \bold{10}, pp. 
#' 491–520, doi:10.1007/s11634-015-0231-9.
#' 
#' @references Dolce, P., Davino, C., Vistocco, D. (2021) Quantile composite-based path modeling: 
#' algorithms, properties and applications.\emph{Advansed Data Analysis and Classification},
#' doi:10.1007/s11634-021-00469-0.
#' 
#' 
#' @export
#' @examples
#' 
#' # Example of QC-PM in Well-Being analysis
#' # model with three LVs and reflective indicators
#' 
#' # load library and dataset province
#' library(qcpm)
#' data(province)
#' 
#' thresholds(province)
#' 
#'
thresholds <- function(x,...){
  
  max_threshold <- function(x){
    vec_max <- apply(x, 2, function(mv){
      tb <- prop.table(table(mv))
      return(tb[length(tb)])
    })
    return(round(1 - max(vec_max), 2))
  }

  min_threshold <- function(x){
    vec_min <- apply(x, 2, function(mv){
      tb <- prop.table(table(mv))
      return(tb[1])
    })
    return(as.numeric(substr(as.character(max(vec_min)), 1, 4)))
  }
  
  return(
  c(tau_min = min_threshold(x), 
    tau_max = max_threshold(x))
  )
}  
  
  
  
  