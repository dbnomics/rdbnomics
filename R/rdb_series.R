#' Download list of series for datasets of DBnomics providers.
#'
#' \code{rdb_series} downloads the list of series for
#' available datasets of a selection of providers from
#' \href{https://db.nomics.world/}{DBnomics}. \cr
#' /!\ We warn the user that this function can be (very) long to execute. We remind
#' that DBnomics requests data from 63 providers to retrieve 21675 datasets for
#' a total of approximately 720 millions series.
#'
#' By default, the function returns a nested named list of \code{data.table}s
#' containing the series of datasets for providers from
#' \href{https://db.nomics.world/}{DBnomics}.
#' 
#' @param provider_code Character string (default \code{NULL}). DBnomics code
#' of one or multiple providers. If \code{NULL}, the providers are firstly
#' dowloaded with the function \code{\link{rdb_providers}} and then the
#' datasets are requested.
#' @param dataset_code Character string (default \code{NULL}). DBnomics code
#' of one or multiple datasets of a provider. If \code{NULL}, the datasets
#' codes are dowloaded with the function \code{\link{rdb_datasets}} and then
#' the series are requested.
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
#' series are requested for only one provider and one dataset then a
#' \code{data.table} is returned, not a nested named list of
#' \code{data.table}s.
#' @param verbose Logical (default \code{FALSE}). Show number of series per
#' datasets and providers.
#' @param ... Additionals arguments.
#' @return A nested named list of \code{data.table}s or a \code{data.table}.
#' @examples
#' \dontrun{
#' rdb_series(provider_code = "IMF", dataset_code = "WEO")
#' 
#' rdb_series(provider_code = "IMF", dataset_code = "WEO", simplify = TRUE)
#' 
#' rdb_series(provider_code = "IMF", verbose = TRUE)
#' 
#' options(rdbnomics.progress_bar_series = TRUE)
#' rdb_series(provider_code = "IMF", dataset_code = "WEO")
#' options(rdbnomics.progress_bar_series = FALSE)
#' 
#' rdb_series(
#'   provider_code = "IMF", dataset_code = "WEO",
#'   use_readLines = TRUE
#' )
#' 
#' rdb_series(
#'   provider_code = "IMF", dataset_code = "WEO",
#'   curl_config = list(proxy = "<proxy>", proxyport = <port>)
#' )
#' }
#' @seealso \code{\link{rdb_providers}}, \code{\link{rdb_last_updates}},
#' \code{\link{rdb_datasets}}, \code{\link{rdb_dimensions}}
#' @author Sebastien Galais
#' @export
rdb_series <- function(
  provider_code = NULL, dataset_code = NULL,
  use_readLines = getOption("rdbnomics.use_readLines"),
  curl_config = getOption("rdbnomics.curl_config"),
  simplify = FALSE, verbose = FALSE,
  ...
) {
  # Additionals arguments
  progress_bar <- TRUE
  if (length(list(...)) > 0) {
    tmp_progress_bar <- list(...)$progress_bar
    if (!is.null(tmp_progress_bar)) {
      progress_bar <- tmp_progress_bar
    }
  }
  check_argument(progress_bar, "logical")

  only_first_two <- FALSE
  if (length(list(...)) > 0) {
    tmp_only_first_two <- list(...)$only_first_two
    if (!is.null(tmp_only_first_two)) {
      only_first_two <- tmp_only_first_two
    }
  }
  check_argument(only_first_two, "logical")

  only_number_series <- FALSE
  if (length(list(...)) > 0) {
    tmp_only_number_series <- list(...)$only_number_series
    if (!is.null(tmp_only_number_series)) {
      only_number_series <- tmp_only_number_series
    }
  }
  check_argument(only_number_series, "logical")

  # All providers
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
  series <- sapply(provider_code, function(pc) {
    if (getOption("rdbnomics.progress_bar_series") & progress_bar) {
      pb <- utils::txtProgressBar(
        min = 0, max = length(dataset_code[[pc]]), style = 3
      )
    }
    
    tmp_ser <- sapply(seq_along(dataset_code[[pc]]), function(i) {
      tryCatch({
        dc <- dataset_code[[pc]][i]

        api_link <- paste0(api_base_url, "/v", api_version, "/series/", pc, "/", dc)
        DBlist <- get_data(api_link, use_readLines, curl_config)

        limit <- DBlist$series$limit
        num_found <- DBlist$series$num_found

        if (only_number_series) {
          return(data.table(Number_of_series = num_found))
        }

        if (verbose) {
          if (getOption("rdbnomics.progress_bar_series") & progress_bar) {
            cat("\n")
          }
          cat(
            paste0(
              "The dataset '", dc, "' from provider '", pc, "' contains ",
              num_found, " series."
            ),
            "\n"
          )
        }

        DBdata <- list(
          data.table::data.table(
            series_code = DBlist$series$docs$series_code,
            series_name = DBlist$series$docs$series_name
          )
        )

        if (num_found > limit) {
          DBdata0 <- DBdata
          rm(DBdata)

          sequence <- seq(1, floor(num_found / limit), 1)

          if (getOption("rdbnomics.progress_bar_series") & progress_bar) {
            pb_offset <- utils::txtProgressBar(
              min = 0, max = max(sequence), style = 3
            )
          }

          # Modifying link
          if (grepl("offset=", api_link)) {
            api_link <- gsub("\\&offset=[0-9]+", "", api_link)
            api_link <- gsub("\\?offset=[0-9]+", "", api_link)
          }
          sep <- ifelse(grepl("\\?", api_link), "&", "?")

          if (only_first_two) {
            sequence <- utils::head(sequence, 1)
          }

          DBdata <- lapply(sequence, function(j) {
            # Modifying link
            tmp_api_link <- paste0(
              api_link, sep, "offset=", format(j * limit, scientific = FALSE)
            )
            # Fetching data
            DBlist <- get_data(tmp_api_link, use_readLines, curl_config)

            if (getOption("rdbnomics.progress_bar_series") & progress_bar) {
              utils::setTxtProgressBar(pb_offset, j)
            }

            # Extracting data
            data.table::data.table(
              series_code = DBlist$series$docs$series_code,
              series_name = DBlist$series$docs$series_name
            )
          })

          DBdata <- append(DBdata, DBdata0, 0)
          rm(DBdata0)
    
          if (getOption("rdbnomics.progress_bar_series") & progress_bar) {
            close(pb_offset)
          }
        }

        DBdata <- rbindlist(DBdata, use.names = TRUE, fill = TRUE)

        if (getOption("rdbnomics.progress_bar_series") & progress_bar) {
          utils::setTxtProgressBar(pb, i)
        }

        DBdata
      }, error = function(e) {
        NULL
      })
    }, simplify = FALSE)

    if (getOption("rdbnomics.progress_bar_series") & progress_bar) {
      close(pb)
    }

    tmp_ser <- stats::setNames(tmp_ser, dataset_code[[pc]])
    Filter(Negate(is.null), tmp_ser)
  }, simplify = FALSE)
  series <- Filter(Negate(is.null), series)
  # We remove the empty lists, the empty data.tables, etc.
  series <- check_dimensions(series, 2)

  if (length(series) <= 0) {
    warning(
      "Error when fetching the series.",
      call. = FALSE
    )
    return(NULL)
  }

  if (simplify) {
    len <- sapply(series, length)
    if (length(series) == 1 & len[1] == 1) {
      return(series[[1]][[1]])
    }
  }

  series
}
