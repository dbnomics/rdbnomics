#' Download DBnomics data using API link (deprecated).
#'
#' \code{rdb_by_api_link} downloads data series from
#' \href{https://db.nomics.world/}{DBnomics}.
#'
#' This function gives you access to hundreds of millions data series from
#' \href{https://api.db.nomics.world/}{DBnomics API} (documentation about
#' the API can be found \href{https://api.db.nomics.world/v22/apidocs}{here}).
#' The API link is given on the
#' \href{https://db.nomics.world/}{DBnomics website}.
#'
#' @param api_link Character string. DBnomics API link of the search.
#' @param use_readLines Logical (default \code{FALSE}). If \code{TRUE}, then
#' the data are requested and read with the base function \code{readLines} i.e.
#' through the default R internet connection. This can be used to get round the
#' error \code{Could not resolve host: api.db.nomics.world}.
#' @param curl_config Named list (default \code{NULL}). If not
#' \code{NULL}, it is used to configure a proxy connection. This
#' configuration is passed to the function \code{curl_fetch_memory} of the package
#' \pkg{curl}. A temporary \code{curl_handle} object is created internally
#' with arguments equal to the provided list in \code{curl_config}.\cr
#' For \code{curl_fetch_memory} arguments see \code{\link[curl]{curl_fetch}}.
#' For available curl options see \code{\link[curl]{curl_options}},
#' \code{names(curl_options())} and
#' \href{https://curl.haxx.se/libcurl/c/curl_easy_setopt.html}{libcurl}.
#' @param filters List (default \code{NULL}). This argument must be a named
#' list for one filter because the function \code{toJSON} of the package \pkg{jsonlite}
#' is used before sending the request to the server. For multiple filters,
#' you have to provide a list of valid filters (see examples).\cr
#' A valid filter is a named list with an element \code{code} which is a character string,
#' and an element \code{parameters} which is a named list with elements \code{frequency}
#' and \code{method} or a NULL.
#' @return A \code{data.table}.
#' @examples
#' \dontrun{
#' # Fetch two series from different datasets of different providers :
#' df1 <- rdb_by_api_link(
#'   paste0(
#'     "https://api.db.nomics.world/v22/",
#'     "series?observations=1&series_ids=AMECO/ZUTN/EA19.1.0.0.0.ZUTN,IMF/CPI/A.AT.PCPIT_IX"
#'   )
#' )
#' 
#' # Fetch one series from the dataset 'Doing Business' of WB provider :
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22",
#'     "indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business",
#'     "&observations=1&format=json&align_periods=1&offset=0&facets=0"
#'   )
#' )
#' 
#' 
#' ## Use a specific proxy to fetch the data
#' # Fetch one series from the dataset 'Doing Business' of WB provider :
#' h <- list(
#'   proxy = "<proxy>",
#'   proxyport = <port>,
#'   proxyusername = "<username>",
#'   proxypassword = "<password>"
#' )
#' options(rdbnomics.curl_config = h)
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22",
#'     "indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business",
#'     "&observations=1&format=json&align_periods=1&offset=0&facets=0"
#'   )
#' )
#' # or to use once
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22",
#'     "indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business",
#'     "&observations=1&format=json&align_periods=1&offset=0&facets=0"
#'   ),
#'   curl_config = h
#' )
#'
#' 
#' ## Use R default connection to avoid a proxy failure (in some cases)
#' # Fetch one series from the dataset 'Doing Business' of WB provider :
#' options(rdbnomics.use_readLines = TRUE)
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22",
#'     "indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business",
#'     "&observations=1&format=json&align_periods=1&offset=0&facets=0"
#'   )
#' )
#' # or to use once
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22",
#'     "indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business",
#'     "&observations=1&format=json&align_periods=1&offset=0&facets=0"
#'   ),
#'   use_readLines = TRUE
#' )
#' 
#' 
#' ## Apply filter(s) to the series
#' # One filter
#' df3 <- rdb_by_api_link(
#'   "https://api.db.nomics.world/v22/series/IMF/WEO:2019-10/ABW.BCA?observations=1",
#'   filters = list(
#'     code = "interpolate",
#'     parameters = list(frequency = "daily", method = "spline")
#'   )
#' )
#' 
#' # Two filters
#' df3 <- rdb_by_api_link(
#'   "https://api.db.nomics.world/v22/series/IMF/WEO:2019-10/ABW.BCA?observations=1",
#'   filters = list(
#'     list(
#'       code = "interpolate",
#'       parameters = list(frequency = "quarterly", method = "spline")
#'     ),
#'     list(
#'       code = "aggregate",
#'       parameters = list(frequency = "annual", method = "average")
#'     )
#'   )
#' )
#' }
#' @seealso \code{\link{rdb}}
#' @author Sebastien Galais
#' @export
rdb_by_api_link <- function(
  api_link,
  use_readLines = getOption("rdbnomics.use_readLines"),
  curl_config = getOption("rdbnomics.curl_config"),
  filters = getOption("rdbnomics.filters")
) {
  .Deprecated("rdb(api_link = ...)", old = "rdb_by_api_link(api_link = ...)")

  .rdb(
    api_link = api_link,
    use_readLines = use_readLines,
    curl_config = curl_config,
    filters = filters
  )
}
