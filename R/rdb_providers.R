#' Download list of DBnomics providers.
#'
#' \code{rdb_providers} downloads the list of providers from
#' \href{https://db.nomics.world/}{DBnomics}.
#'
#' By default, the function returns a \code{data.table}
#' containing the list of providers from
#' \href{https://db.nomics.world/}{DBnomics} with additional informations such as
#' the region, the website, etc.
#' 
#' @param code Logical (default \code{FALSE}). If \code{TRUE}, then only the
#' providers are returned in a vector.
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
#' @return A \code{data.table} or a vector.
#' @examples
#' \dontrun{
#' rdb_providers()
#' 
#' rdb_providers(code = TRUE)
#' 
#' rdb_providers(use_readLines = TRUE)
#' 
#' rdb_providers(curl_config = list(proxy = "<proxy>", proxyport = <port>))
#' }
#' @seealso \code{\link{rdb_last_updates}}, \code{\link{rdb_datasets}},
#' \code{\link{rdb_dimensions}}
#' @author Sebastien Galais
#' @export
rdb_providers <- function(
  code = FALSE, use_readLines = getOption("rdbnomics.use_readLines"),
  curl_config = getOption("rdbnomics.curl_config")
) {
  # Checking arguments
  check_argument(code, "logical")
  check_argument(use_readLines, "logical")

  # Setting API url
  api_base_url <- getOption("rdbnomics.api_base_url")
  check_argument(api_base_url, "character")

  # Setting API version
  api_version <- getOption("rdbnomics.api_version")
  check_argument(api_version, c("numeric", "integer"))
  authorized_version(api_version)

  providers <- paste0(api_base_url, "/v", api_version, "/providers")
  providers <- get_data(providers, use_readLines, curl_config)
  providers <- providers$providers$docs
  data.table::setDT(providers)

  if (code) {
    providers <- providers$code
    providers <- no_empty_char(providers)
    providers <- sort(providers)
  } else {
    transform_date_timestamp(providers)
    providers <- providers[order(code)]
  }

  providers[]
}
