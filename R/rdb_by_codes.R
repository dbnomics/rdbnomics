#' Download DBnomics data using series codes.
#'
#' \code{rdb_by_codes} downloads data series from
#' \href{https://next.nomics.world/}{DBnomics}.
#'
#' This function gives you access to hundreds of millions data series from
#'  \href{https://api.next.nomics.world/}{DBnomics API} (documentation about
#'   the API can be found \href{https://api.next.nomics.world/apidocs}{here}).
#'   The code of each series is given on the
#'   \href{https://next.nomics.world/}{DBnomics website}.
#'
#' @param provider_code Character string. DBnomics code of the provider.
#' @param dataset_code Character string. DBnomics code of the dataset.
#' @param series_codes Character string or vector of character strings. DBnomics
#'  code of one or several series in the specified dataset.
#' @param api_base_url Character string. DBnomics API link.
#' @return A data frame.
#' @examples
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb_by_codes('AMECO','ZUTN','EA19.1.0.0.0.ZUTN')
#'
#' # Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb_by_codes('AMECO','ZUTN',c('EA19.1.0.0.0.ZUTN','DNK.1.0.0.0.ZUTN'))
#'
#' # Fetch one dataset 'Exports and imports by Member States of the EU/third countries'
#'  (namq_10_exi) of Eurostat provider:
#' df3 <- rdb_by_codes('Eurostat','namq_10_exi')
#' @import jsonlite dplyr tidyr stringr
#' @seealso \code{\link{rdb_by_url}} \code{\link{rdb_by_dimensions}}
#' @export

rdb_by_codes <- function(provider_code,dataset_code,series_codes=NA,api_base_url="https://api.next.nomics.world"){
  
  if (is.na(series_codes[1])) {
    url_code <- paste0(api_base_url,"/",provider_code,"/",dataset_code)
  } else {
    url_series_codes <- paste0(series_codes, collapse = ",")
    url_code <- paste0(api_base_url,"/",provider_code,"/",dataset_code,"?series_codes=",url_series_codes)
  }
  
  rdb_by_url(url_code)
  
}
