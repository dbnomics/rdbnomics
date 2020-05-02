#' Download list of datasets for DBnomics providers.
#'
#' \code{rdb_datasets} downloads the list of available datasets for a selection
#' of providers (or all of them) from \href{https://db.nomics.world/}{DBnomics}.
#'
#' By default, the function returns a named list of \code{data.table}s
#' containing the datasets of the providers from
#' \href{https://db.nomics.world/}{DBnomics}.
#' 
#' @param provider_code Character string (default \code{NULL}). DBnomics code
#' of one or multiple providers. If \code{NULL}, the providers are firstly
#' dowloaded with the function \code{\link{rdb_providers}} and then the
#' available datasets are requested.
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
#' @param simplify Logical (default \code{FALSE}). If \code{TRUE}, when the
#' datasets are requested for only one provider then a \code{data.table}
#' is returned, not a list of \code{data.table}s.
#' @param ... Additionals arguments.
#' @return A named list of \code{data.table}s or a \code{data.table}.
#' @examples
#' \dontrun{
#' rdb_datasets(provider_code = "IMF")
#' 
#' rdb_datasets(provider_code = "IMF", simplify = TRUE)
#'
#' rdb_datasets(provider_code = c("IMF", "BDF"))
#' 
#' options(rdbnomics.progress_bar_datasets = TRUE)
#' rdb_datasets()
#' options(rdbnomics.progress_bar_datasets = FALSE)
#' 
#' 
#' rdb_datasets(provider_code = "IMF", use_readLines = TRUE)
#' 
#' rdb_datasets(
#'   provider_code = "IMF",
#'   curl_config = list(proxy = "<proxy>", proxyport = <port>)
#' )
#' }
#' @seealso \code{\link{rdb_providers}}, \code{\link{rdb_last_updates}},
#' \code{\link{rdb_dimensions}}, \code{\link{rdb_series}}
#' @author Sebastien Galais
#' @export
rdb_datasets <- function(
  provider_code = NULL,
  use_readLines = getOption("rdbnomics.use_readLines"),
  curl_config = getOption("rdbnomics.curl_config"),
  simplify = FALSE,
  ...
) {
  # TO CHECK THE DATASETS
  # parse_web <- sapply(provider_code, function(x) {
  #   y <- readLines(paste0("https://db.nomics.world/", x))
  #   y <- y[grepl(paste0("<a rel=\"prefetch\" href=\"/", x), y)]
  #   y <- gsub(paste0(".*href=\"/", x, "/"), "", y)
  #   gsub("\".*", "", y)
  # }, simplify = FALSE)

  # Additionals arguments
  progress_bar <- ellipsis_default("progress_bar", list(...), TRUE)
  check_argument(progress_bar, "logical")

  # All providers
  if (is.null(provider_code)) {
    provider_code <- rdb_providers(
      code = TRUE,
      use_readLines = use_readLines, curl_config = curl_config
    )
  }

  # Checking arguments
  check_argument(provider_code, "character", len = FALSE)
  check_argument(use_readLines, "logical")
  check_argument(simplify, "logical")

  # Setting API url
  api_base_url <- getOption("rdbnomics.api_base_url")
  check_argument(api_base_url, "character")

  # Setting API version
  api_version <- getOption("rdbnomics.api_version")
  check_argument(api_version, c("numeric", "integer"))
  authorized_version(api_version)

  # Fetching all datasets
  if (getOption("rdbnomics.progress_bar_datasets") & progress_bar) {
    pb <- utils::txtProgressBar(min = 0, max = length(provider_code), style = 3)
  }

  datasets <- sapply(seq_along(provider_code), function(i) {
    tryCatch({
      pc <- provider_code[i]

      tmp <- paste0(api_base_url, "/v", api_version, "/providers/", pc)
      tmp <- get_data(tmp, use_readLines, curl_config)
      tmp <- tmp$category_tree
      tmp <- unpack(tmp)
      tmp <- rbindlist_recursive(tmp)
      tmp <- tmp[, .(code, name)]
      tmp <- unique(tmp)

      if (getOption("rdbnomics.progress_bar_datasets") & progress_bar) {
        utils::setTxtProgressBar(pb, i)
      }

      tmp[order(code)]
    }, error = function(e) {
      NULL
    })
  }, simplify = FALSE)

  if (getOption("rdbnomics.progress_bar_datasets") & progress_bar) {
    close(pb)
  }

  datasets <- stats::setNames(datasets, provider_code)
  datasets <- Filter(Negate(is.null), datasets)
  datasets <- check_datasets(datasets)

  if (length(datasets) <= 0) {
    warning(
      "Error when fetching the datasets codes.",
      call. = FALSE
    )
    return(NULL)
  }

  if (simplify) {
    if (length(datasets) == 1) {
      return(datasets[[1]])
    }
  }
  
  datasets
}
