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
#' the data are requested and read with the base function \code{readLines}.
#' @return A data.frame.
#' @examples
#' \dontrun{
#' # Fetch two series from different datasets of different providers:
#' df1 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/',
#'     'series?series_ids=AMECO/ZUTN/EA19.1.0.0.0.ZUTN,IMF/CPI/A.AT.PCPIT_IX'
#'   )
#' )
#' 
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb_by_api_link(
#'   paste0(
#'     'https://api.db.nomics.world/',
#'     'series?provider_code=AMECO&dataset_code=ZUTN&dimensions=%7B%22geo%22%3A%5B%22ea12%22%5D%7D'
#'   )
#' )
#' }
#' @seealso \code{\link{rdb}}
#' @export
rdb_by_api_link <- function(
  api_link,
  use_readLines = getOption("rdbnomics.use_readLines")
) {
  # Checking 'api_link'
  if (is.null(api_link)) {
    return(NULL)
  }
  stopifnot(is.character(api_link), length(api_link) == 1)

  # Checking 'use_readLines'
  stopifnot(!is.null(use_readLines))
  stopifnot(is.logical(use_readLines), length(use_readLines) == 1)

  # Fetching data
  DBlist <- read_lines(use_readLines, api_link)

  # If data is empty, return NULL
  if (is.data.frame(DBlist$series$data)) {
    if (nrow(DBlist$series$data) <= 0) {
      return(NULL)
    }
  }

  if (inherits(DBlist$series$data, "list")) {
    if (length(DBlist$series$data) <= 0) {
      return(NULL)
    }
  }

  # Extracting data
  DBdata <- list(DBlist$series$data)
  # Checking if the limit has been reached
  num_found <- DBlist$series$num_found
  limit <- DBlist$series$limit

  if (num_found > limit) {
    n_iter <- ceiling(num_found / limit) - 1

    for (i in 1:n_iter) {
      # Modifying link
      api_link2 <- paste0(api_link, "?offset=", paste(i * limit))

      # Fetching data
      DBlist <- read_lines(use_readLines, api_link2)

      # Extracting data
      DBdata[[i + 1]] <- DBlist$series$data
    }
  }

  # Transforming into data.table
  DT <- lapply(DBdata, data.table::setDT)
  # Gathering data
  DT <- data.table::rbindlist(DT, use.names = TRUE, fill = TRUE)
  # Expanding list columns
  DT <- deploy(DT)
  # Transforming to date format
  DT[, period_start_day := as.Date(period_start_day)]
  # Modifying column names
  setnames(DT, "period", "original_period")
  setnames(DT, "period_start_day", "period")
  # Returning a data.frame
  setDF(DT)

  DT[]
}
