#' Download DBnomics data using dimensions codes.
#'
#' \code{rdb_by_dimensions} downloads data series from
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
#' @param dimensions Character string (single quote). DBnomics
#'  code of one or several dimensions in the specified dataset.
#' @param api_base_url Character string. DBnomics API link.
#' @return A data frame.
#' @examples
#' # Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb_by_dimensions('AMECO','ZUTN','{"geo": ["ea12"]}')
#'
#' # Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb_by_dimensions('AMECO','ZUTN','{"geo": ["ea12", "dnk"]}')
#'
#' # Fetch several values of several dimensions from dataset 'Doing business' (DB) of World bank:
#' df3 <- rdb_by_dimensions('world-bank','DB','{"country": ["DZ", "BT", "PE"],"indicator": ["IC.DCP.BQCI","IC.REG.COST.PC.ZS"]}')
#'
#' # Fetch one dataset 'Exports and imports by Member States of the EU/third countries'
#'  (namq_10_exi) of Eurostat provider:
#' df4 <- rdb_by_dimensions('Eurostat','namq_10_exi')
#' @import jsonlite dplyr tidyr stringr
#' @seealso \code{\link{rdb_by_url}}, \code{\link{rdb_by_codes}}
#' @export

rdb_by_dimensions <- function(provider_code,dataset_code,dimensions=NA,api_base_url="https://api.next.nomics.world"){

  dimensions <- stringr::str_replace_all(dimensions,"\\s","")
  
  if (is.na(dimensions[1])) {
    url_code <- paste0(api_base_url,"/",provider_code,"/",dataset_code)
  } else {
    url_code <- paste0(api_base_url,"/",provider_code,"/",dataset_code,"?dimensions=",dimensions)
  }
  
  rdb_by_url(url_code)
  
}
