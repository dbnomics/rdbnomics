#' Download DBnomics data using API link.
#'
#' \code{rdb_by_api_link} downloads data series from
#' \href{https://db.nomics.world/}{DBnomics}.
#'
#' This function gives you access to hundreds of millions data series from
#' \href{https://api.db.nomics.world/}{DBnomics API} (documentation about
#' the API can be found \href{https://api.db.nomics.world/apidocs}{here}).
#' The API link is given on the
#' \href{https://db.nomics.world/}{DBnomics website}.
#'
#' @param api_link Character string. DBnomics API link of the search.
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
#' @return A \code{data.frame} or a \code{data.table}.
#' @examples
#' \dontrun{
#' # Fetch two series from different datasets of different providers:
#' df1 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/v22/',
#'     'series?observations=1&series_ids=AMECO/ZUTN/EA19.1.0.0.0.ZUTN,IMF/CPI/A.AT.PCPIT_IX'
#'   )
#' )
#' 
#' # Fetch one series from the dataset 'Doing Business' of WB provider:
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22',
#'     'indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business',
#'     '&observations=1&format=json&align_periods=1&offset=0&facets=0'
#'   )
#' )
#' 
#' 
#' ## Use a specific proxy to fetch the data
#' # Fetch one series from the dataset 'Doing Business' of WB provider:
#' h <- curl::new_handle(
#'   proxy = "<proxy>",
#'   proxyport = <port>,
#'   proxyusername = "<username>",
#'   proxypassword = "<password>"
#' )
#' options(rdbnomics.curl_config = h)
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22',
#'     'indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business',
#'     '&observations=1&format=json&align_periods=1&offset=0&facets=0'
#'   )
#' )
#' # or to use once
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22',
#'     'indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business',
#'     '&observations=1&format=json&align_periods=1&offset=0&facets=0'
#'   ),
#'   curl_config = h
#' )
#'
#' 
#' ## Use R default connection to avoid a proxy failure (in some cases)
#' # Fetch one series from the dataset 'Doing Business' of WB provider:
#' options(rdbnomics.use_readLines = TRUE)
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22',
#'     'indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business',
#'     '&observations=1&format=json&align_periods=1&offset=0&facets=0'
#'   )
#' )
#' # or to use once
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22',
#'     'indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&q=Doing%20Business',
#'     '&observations=1&format=json&align_periods=1&offset=0&facets=0'
#'   ),
#'   use_readLines = TRUE
#' )
#' }
#' @seealso \code{\link{rdb}}
#' @export
rdb_by_api_link <- function(
  api_link, use_readLines = getOption("rdbnomics.use_readLines"),
  curl_config = getOption("rdbnomics.curl_config")
) {
  # Checking 'api_link'
  if (is.null(api_link)) { return(NULL) }
  check_argument(api_link, "character")

  # Checking 'use_readLines'
  check_argument(use_readLines, "logical")

  # Fetching data
  DBlist <- get_data(api_link, use_readLines, curl_config)

  # Getting API version
  api_version <- get_version(DBlist)

  if (api_version == 21) {
    data_elt <- "data"
  } else if (api_version == 22) {
    data_elt <- "docs"
  } else {
    stop(paste0("Don't know what to do for API version ", api_version, "."))
  }

  # If data is empty, return NULL
  if (is.data.frame(DBlist$series[[data_elt]])) {
    if (nrow(DBlist$series[[data_elt]]) <= 0) {
      return(NULL)
    }
  }

  if (inherits(DBlist$series[[data_elt]], "list")) {
    if (length(DBlist$series[[data_elt]]) <= 0) {
      return(NULL)
    }
  }

  # Checking if the limit has been reached
  num_found <- DBlist$series$num_found
  limit <- DBlist$series$limit
  # Extracting data
  DBdata <- list(DBlist$series[[data_elt]])
  rm(DBlist)

  if (num_found > limit) {
    DBdata0 <- DBdata
    rm(DBdata)

    sequence <- seq(1, floor(num_found / limit), 1)

    # Modifying link
    if (grepl("offset=", api_link)) {
      api_link <- gsub("\\&offset=[0-9]+", "", api_link)
      api_link <- gsub("\\?offset=[0-9]+", "", api_link)
    }
    sep <- ifelse(grepl("\\?", api_link), "&", "?")

    DBdata <- lapply(sequence, function(i) {
      # Modifying link
      tmp_api_link <- paste0(api_link, sep, "offset=", i * limit)
      # Fetching data
      DBlist <- get_data(tmp_api_link, use_readLines, curl_config)
      # Extracting data
      DBlist$series[[data_elt]]
    })

    DBdata <- append(DBdata, DBdata0, 0)
    rm(DBdata0)
  }

  # Transform data.frames inside DBdata
  if (list_has_dataframe(DBdata)) {
    DBdata <- lapply(DBdata, dataframe_to_columns)
  }

  # Transforming into data.table
  DBdata <- lapply(DBdata, data.table::setDT)
  # Gathering data
  DBdata <- data.table::rbindlist(DBdata, use.names = TRUE, fill = TRUE)
  # Expanding list columns
  DBdata <- deploy(DBdata)
  # Transforming date format and timestamp format
  transform_date_timestamp(DBdata)

  # Modifying column names
  data.table::setnames(DBdata, "period", "original_period")
  data.table::setnames(DBdata, "period_start_day", "period")

  DBdata[]
}
