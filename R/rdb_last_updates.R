#' Download informations about the last DBnomics updates.
#'
#' \code{rdb_last_updates} downloads informations about the last updates from
#' \href{https://db.nomics.world/}{DBnomics}.
#'
#' By default, the function returns a \code{data.frame} (or a \code{data.table})
#' containing the last 100 updates from
#' \href{https://db.nomics.world/}{DBnomics} with additional informations.
#'
#' @param all Logical (default \code{FALSE}). If \code{TRUE}, then the entire
#' dataset of the last updates is retrieved.
#' @param use_readLines Logical (default \code{FALSE}). If \code{TRUE}, then
#' the data are requested and read with the base function \code{readLines} i.e.
#' through the default R internet connection. This can be used to get round the
#' error \code{Could not resolve host: api.db.nomics.world}.
#' @param curl_config Curl_handle or list (default \code{NULL}). If not
#' \code{NULL}, it is used to set up a specific proxy connection. This
#' configuration is passed to the function \code{curl_fetch_memory} of the package
#' \pkg{curl}. If it is a \code{curl_handle} object then it is considered to
#' be the argument \code{handle} of \code{curl_fetch_memory}. In the case of a
#' list, the names of the object are the names of the arguments of
#' \code{curl_fetch_memory}. It means that \code{curl_config = h} is
#' equivalent to \code{curl_config = list(handle = h)}.  
#' For \code{curl_fetch_memory} arguments see
#' \code{\link[curl]{curl_fetch}}.
#' For available curl options see \code{\link[curl]{curl_options}},
#' \code{names(curl_options())} and
#' \href{https://curl.haxx.se/libcurl/c/curl_easy_setopt.html}{libcurl}.
#' @return A \code{data.frame} or a \code{data.table}.
#' @examples
#' \dontrun{
#' rdb_last_updates()
#' 
#' rdb_last_updates(all = TRUE)
#' }
#' @seealso \code{\link{rdb_providers}}
#' @export
rdb_last_updates <- function(
  all = FALSE, use_readLines = getOption("rdbnomics.use_readLines"),
  curl_config = getOption("rdbnomics.curl_config")
) {
  # Checking arguments
  check_argument(all, "logical")
  check_argument(use_readLines, "logical")

  # Setting API url
  api_base_url <- getOption("rdbnomics.api_base_url")
  check_argument(api_base_url, "character")

  # Setting API version
  api_version <- getOption("rdbnomics.api_version")
  check_argument(api_version, c("numeric", "integer"))
  authorized_version(api_version)

  updates <- paste0(api_base_url, "/v", api_version, "/last-updates")
  updates <- get_data(updates, use_readLines, curl_config)

  if (api_version == 21) {
    updates <- updates$datasets
    data.table::setDT(updates)
  } else if (api_version == 22){
    n <- updates$datasets$num_found
    lim <- updates$datasets$limit
    updates <- updates$datasets$docs
    data.table::setDT(updates)

    if (all) {
      sequence <- seq(0, floor(n/lim) * lim, lim)
      updates <- lapply(sequence, function(x) {
        link <- paste0(
          api_base_url, "/v", api_version, "/last-updates?datasets.offset=",
          x
        )
        dataset <- get_data(link, use_readLines, curl_config)
        data.table::setDT(dataset$datasets$docs)
      })
      updates <- data.table::rbindlist(updates, use.names = TRUE, fill = TRUE)
    }
  } else {
    stop(paste0("Don't know what to do for API version ", api_version, "."))
  }

  transform_date_timestamp(updates)

  updates[]
}
