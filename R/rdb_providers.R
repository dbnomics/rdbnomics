#' Download list of DBnomics providers.
#'
#' \code{rdb_providers} downloads the list of providers from
#' \href{https://db.nomics.world/}{DBnomics}.
#'
#' By default, the function returns a \code{data.frame} (or a \code{data.table})
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
#' @param curl_config Curl_handle or list (default \code{NULL}). If not
#' \code{NULL}, it is used for setting up a specific proxy connection. This
#' setup is passed to the function \code{curl_fetch_memory} of the package
#' \pkg{curl}. If it is a \code{curl_handle} object then it is considered to
#' be the argument \code{handle} of \code{curl_fetch_memory}. In the case of a
#' list, the names of the object elements are the names of the arguments of
#' \code{curl_fetch_memory}. It means that \code{curl_config = h} is
#' equivalent to \code{curl_config = list(handle = h)}. For
#' \code{curl_fetch_memory} arguments see
#' \code{\link[curl]{curl_fetch}}.
#' For available curl options see \code{\link[curl]{curl_options}},
#' \code{names(curl_options())} and
#' \href{https://curl.haxx.se/libcurl/c/curl_easy_setopt.html}{libcurl}.
#' @return A \code{data.frame}, a \code{data.table} or a vector.
#' @examples
#' \dontrun{
#' rdb_providers()
#' 
#' rdb_providers(code = TRUE)
#' }
#' @seealso \code{\link{rdb_last_updates}}
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
