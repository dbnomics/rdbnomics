#' Package rdbnomics
#'
#' DBnomics R client.
#'
#' @docType package
#' @name rdbnomics
#' 
#' @import jsonlite data.table
#' 
NULL

if (getRversion() >= "2.15.1") {
  vars <- c(".", ":=", "value", "dotI", "period", "period_start_day")
  utils::globalVariables(unique(vars))
}