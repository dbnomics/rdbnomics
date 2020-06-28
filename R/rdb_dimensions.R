#' Download list of dimensions for datasets of DBnomics providers.
#'
#' \code{rdb_dimensions} downloads the list of dimensions (if they exist) for
#' available datasets of a selection of providers from
#' \href{https://db.nomics.world/}{DBnomics}.
#'
#' By default, the function returns a nested named list of \code{data.table}s
#' containing the dimensions of datasets for providers from
#' \href{https://db.nomics.world/}{DBnomics}.
#' 
#' @param provider_code Character string (default \code{NULL}). DBnomics code
#' of one or multiple providers. If \code{NULL}, the providers are firstly
#' dowloaded with the function \code{\link{rdb_providers}} and then the
#' datasets are requested.
#' @param dataset_code Character string (default \code{NULL}). DBnomics code
#' of one or multiple datasets of a provider. If \code{NULL}, the datasets
#' codes are dowloaded with the function \code{\link{rdb_datasets}} and then
#' the dimensions are requested.
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
#' dimensions are requested for only one provider and one dataset then a
#' named list of \code{data.table}s is returned, not a nested named list of
#' \code{data.table}s.
#' @param ... Additionals arguments.
#' @return A nested named list of \code{data.table}s or a named list of
#' \code{data.table}s.
#' @examples
#' \dontrun{
#' rdb_dimensions(provider_code = "IMF", dataset_code = "WEO")
#' 
#' rdb_dimensions(provider_code = "IMF", dataset_code = "WEO", simplify = TRUE)
#' 
#' rdb_dimensions(provider_code = "IMF")
#' 
#' # /!\ It is very long !
#' options(rdbnomics.progress_bar_dimensions = TRUE)
#' rdb_dimensions()
#' options(rdbnomics.progress_bar_dimensions = FALSE)
#' 
#' rdb_dimensions(
#'   provider_code = "IMF", dataset_code = "WEO",
#'   use_readLines = TRUE
#' )
#' 
#' rdb_dimensions(
#'   provider_code = "IMF", dataset_code = "WEO",
#'   curl_config = list(proxy = "<proxy>", proxyport = <port>)
#' )
#' }
#' @seealso \code{\link{rdb_providers}}, \code{\link{rdb_last_updates}},
#' \code{\link{rdb_datasets}}, \code{\link{rdb_series}}
#' @author Sebastien Galais
#' @export
rdb_dimensions <- function(
  provider_code = NULL, dataset_code = NULL,
  use_readLines = getOption("rdbnomics.use_readLines"),
  curl_config = getOption("rdbnomics.curl_config"),
  simplify = FALSE,
  ...
) {
  # Additionals arguments
  progress_bar <- ellipsis_default("progress_bar", list(...), TRUE)
  check_argument(progress_bar, "logical")

  # All providers
  if (is.null(provider_code) & !is.null(dataset_code)) {
    stop(
      "If you give datasets codes, please give also a provider code.",
      call. = FALSE
    )
  }

  if (is.null(provider_code)) {
    provider_code <- rdb_providers(
      code = TRUE,
      use_readLines = use_readLines, curl_config = curl_config
    )
  }
  check_argument(provider_code, "character", len = FALSE)

  if (is.null(dataset_code)) {
    dataset_code <- rdb_datasets(
      provider_code = provider_code,
      use_readLines = use_readLines, curl_config = curl_config,
      simplify = FALSE, progress_bar = FALSE
    )
    dataset_code <- sapply(dataset_code, `[[`, "code", simplify = FALSE)
  } else {
    check_argument(dataset_code, "character", len = FALSE)
    dataset_code <- list(dataset_code)
    dataset_code <- stats::setNames(dataset_code, provider_code)
  }

  # Checking arguments
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
  dimensions <- sapply(provider_code, function(pc) {
    if (getOption("rdbnomics.progress_bar_dimensions") & progress_bar) {
      pb <- utils::txtProgressBar(
        min = 0, max = length(dataset_code[[pc]]), style = 3
      )
    }
    
    tmp_dim <- sapply(seq_along(dataset_code[[pc]]), function(i) {
      tryCatch({
        dc <- dataset_code[[pc]][i]

        tmp <- paste0(api_base_url, "/v", api_version, "/datasets/", pc, "/", dc)
        tmp <- get_data(tmp, use_readLines, curl_config)

        tmp1 <- tmp$datasets$docs$dimensions_labels
        if (is.null(tmp1)) {  
          tmp1 <- try(
            tmp$datasets[[paste0(pc, "/", dc)]]$dimensions_labels,
            silent = TRUE
          )
          if (inherits(tmp1, "try-error")) {
            tmp1 <- NULL
          }
        }
        if (is.null(tmp1)) {
          # Sometimes "dimensions_labels" is missing
          tmp1 <- data.table::data.table(A = character(), B = character())
        } else {
          tmp1 <- as.list(tmp1)
          tmp1 <- data.table::data.table(A = unlist(tmp1), B = names(tmp1))
          tmp1 <- unique(tmp1)
          # Normally column B is in capital letters
          if (nrow(tmp1) > 0) {
            tmp1[, EQUAL := as.numeric(A == B)]
            tmp1[
              EQUAL == 1,
              A := ifelse(A == capital_first(A), toupper(A), capital_first(A))
            ]
            tmp1[, EQUAL := NULL]
          }
        }

        tmp2 <- tmp$datasets$docs$dimensions_values_labels
        if (is.null(tmp2)) {  
          tmp2 <- try(
            tmp$datasets[[paste0(pc, "/", dc)]]$dimensions_values_labels,
            silent = TRUE
          )
          if (inherits(tmp2, "try-error")) {
            tmp2 <- NULL
          }
        }
        
        tmp3 <- sapply(names(tmp2), function(nm) {
          z <- tmp2[[nm]]
          if (
            is.list(z) & !is.data.frame(z) &
            !data.table::is.data.table(z)
          ) {
            # "z" is actually a list with a matrix
            z <- z[[1]]
            z <- as.data.table(z)
          } else {
            if (is.matrix(z)) {
              # "z" is actually a matrix
              z <- as.data.table(z)
            } else {
              z <- as.data.table(z)
              z <- as.list(z)
              z <- data.table::data.table(V1 = names(z), V2 = unlist(z))
            }
          }
          data.table::setnames(z, "V1", nm)
          new_name <- tmp1[B == nm]$A
          data.table::setnames(
            z, "V2", ifelse(length(new_name) <= 0, new_title(nm), new_name)
          )
          z
        }, simplify = FALSE)

        if (getOption("rdbnomics.progress_bar_dimensions") & progress_bar) {
          utils::setTxtProgressBar(pb, i)
        }

        tmp3
      }, error = function(e) {
        NULL
      })
    }, simplify = FALSE)

    if (getOption("rdbnomics.progress_bar_dimensions") & progress_bar) {
      close(pb)
    }

    tmp_dim <- stats::setNames(tmp_dim, dataset_code[[pc]])
    Filter(Negate(is.null), tmp_dim)
  }, simplify = FALSE)
  dimensions <- Filter(Negate(is.null), dimensions)
  # We remove the empty lists, the empty data.tables, etc.
  dimensions <- check_dimensions(dimensions, 2)

  if (length(dimensions) <= 0) {
    warning(
      "Error when fetching the dimensions.",
      call. = FALSE
    )
    return(NULL)
  }

  if (simplify) {
    len <- sapply(dimensions, length)
    if (length(dimensions) == 1 & len[1] == 1) {
      return(dimensions[[1]][[1]])
    }
  }

  dimensions
}
