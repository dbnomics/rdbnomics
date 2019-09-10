#' Download DBnomics data.
#'
#' \code{rdb} downloads data series from
#' \href{https://db.nomics.world/}{DBnomics} using shortcuts like \code{ids},
#' \code{dimensions} or \code{mask}.
#'
#' This function gives you access to hundreds of millions data series from
#' \href{https://api.db.nomics.world/}{DBnomics API} (documentation about
#' the API can be found \href{https://api.db.nomics.world/v22/apidocs}{here}).
#' The code of each series is given on the
#' \href{https://db.nomics.world/}{DBnomics website}. \cr\cr
#' In the event that only the argument \code{ids} is provided (and those in the
#' ellipsis \code{...}), the argument name can be dropped. The character string
#' vector is directly passed to \code{ids}. \cr
#' In the same way, if only \code{provider_code}, \code{dataset_code} and
#' \code{mask} are provided then the arguments names can be dropped. The
#' last character string is automatically passed to \code{mask}.
#'
#' @param provider_code Character string (default \code{NULL}). DBnomics code
#' of the provider.
#' @param dataset_code Character string (default \code{NULL}). DBnomics code
#' of the dataset.
#' @param ids Character string (default \code{NULL}). DBnomics code of one or
#' several series.
#' @param dimensions List or character string (single quoted)  (default \code{NULL}).
#' DBnomics code of one or several dimensions in the specified provider and dataset.
#' If it is a named list, then the function \code{toJSON} (from the
#' package \pkg{jsonlite}) is applied to generate the json object.
#' @param mask Character string (default \code{NULL}). DBnomics code of one or
#' several masks in the specified provider and dataset.
#' @param filters List (default \code{NULL}). This argument must be a named
#' list for one filter because the function \code{toJSON} of the package \pkg{jsonlite}
#' is used before sending the request to the server. For multiple filters,
#' you have to provide a list of valid filters (see examples).\cr
#' A valid filter is a named list with an element \code{code} which is a character string,
#' and an element \code{parameters} which is a named list with elements \code{frequency}
#' and \code{method} or a NULL.
#' @param verbose Logical (default \code{FALSE}). Show warnings of the function.
#' @param ... Arguments to be passed to \code{\link{rdb_by_api_link}}. These
#' arguments concern connection configuration. See \code{\link{rdb_by_api_link}}
#' for details.
#' @return A \code{data.table}.
#' @examples
#' \dontrun{
#' ## By ids
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider :
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # or when no argument names are given (provider_code -> ids)
#' df1 <- rdb('AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#'
#' # Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider :
#' df2 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))
#'
#' # Fetch two series from different datasets of different providers :
#' df3 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'IMF/CPI/A.AT.PCPIT_IX'))
#'
#'
#' ## By dimensions
#' # Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider :
#' df1 <- rdb('AMECO', 'ZUTN', dimensions = list(geo = "ea12"))
#' # or
#' df1 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12"]}')
#'
#' # Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider :
#' df2 <- rdb('AMECO', 'ZUTN', dimensions = list(geo = c("ea12", "dnk")))
#' # or
#' df2 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12", "dnk"]}')
#'
#' # Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank :
#' dim <- list(
#'   country = c("DZ", "PE"),
#'   indicator = c("ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS")
#' )
#' df3 <- rdb('WB', 'DB', dimensions = dim)
#' # or
#' dim <- paste0(
#'   '{"country": ["DZ", "PE"],',
#'   '"indicator": ["ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS"]}'
#' )
#' df3 <- rdb('WB', 'DB', dimensions = dim)
#'
#'
#' ## By mask
#' # Fetch one series from dataset 'Consumer Price Index' (CPI) of IMF :
#' df1 <- rdb('IMF', 'CPI', mask = 'M.DE.PCPIEC_WT')
#' # or when no argument names are given except provider_code and dataset_code (ids -> mask)
#' df1 <- rdb('IMF', 'CPI', 'M.DE.PCPIEC_WT')
#'
#' # Fetch two series from dataset 'Consumer Price Index' (CPI) of IMF :
#' df2 <- rdb('IMF', 'CPI', mask = 'M.DE+FR.PCPIEC_WT')
#'
#' # Fetch all series along one dimension from dataset 'Consumer Price Index' (CPI) of IMF :
#' df3 <- rdb('IMF', 'CPI', mask = 'M..PCPIEC_WT')
#'
#' # Fetch series along multiple dimensions from dataset 'Consumer Price Index' (CPI) of IMF :
#' df4 <- rdb('IMF', 'CPI', mask = 'M..PCPIEC_IX+PCPIA_IX')
#'
#'
#' ## Use a specific proxy to fetch the data
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider :
#' h <- list(
#'   proxy = "<proxy>",
#'   proxyport = <port>,
#'   proxyusername = "<username>",
#'   proxypassword = "<password>"
#' )
#' options(rdbnomics.curl_config = h)
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # or to use once
#' options(rdbnomics.curl_config = NULL)
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN', curl_config = h)
#'
#'
#' ## Use R default connection to avoid a proxy failure (in some cases)
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider :
#' options(rdbnomics.use_readLines = TRUE)
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # or to use once
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN', use_readLines = TRUE)
#' 
#' 
#' ## Apply filter(s) to the series
#' # One filter
#' df1 <- rdb(
#'   ids = c("IMF/WEO/ABW.BCA", "IMF/WEO/ABW.BCA_NGDPD"),
#'   filters = list(
#'     code = "interpolate",
#'     parameters = list(frequency = "daily", method = "spline")
#'   )
#' )
#' 
#' # Two filters
#' df1 <- rdb(
#'   ids = c("IMF/WEO/ABW.BCA", "IMF/WEO/ABW.BCA_NGDPD"),
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
#' @seealso \code{\link{rdb_by_api_link}}
#' @export
rdb <- function(
  provider_code = NULL, dataset_code = NULL,
  ids = NULL, dimensions = NULL, mask = NULL,
  filters = getOption("rdbnomics.filters"),
  verbose = getOption("rdbnomics.verbose_warning"),
  ...
) {
  # Checking 'verbose'
  check_argument(verbose, "logical")

  # Setting API url
  api_base_url <- getOption("rdbnomics.api_base_url")
  check_argument(api_base_url, "character")

  # Setting API version
  api_version <- getOption("rdbnomics.api_version")
  check_argument(api_version, c("numeric", "integer"))
  authorized_version(api_version)

  # Setting API metadata
  metadata <- getOption("rdbnomics.metadata")
  check_argument(metadata, "logical")

  # Building API base url
  api_base_url <- paste0(api_base_url, "/v", api_version, "/series")

  # Checking arguments
  provider_code_null <- is.null(provider_code)
  provider_code_not_null <- !provider_code_null

  dataset_code_null <- is.null(dataset_code)
  dataset_code_not_null <- !dataset_code_null

  dimensions_null <- is.null(dimensions)
  dimensions_not_null <- !dimensions_null

  mask_null <- is.null(mask)
  mask_not_null <- !mask_null

  ids_null <- is.null(ids)
  ids_not_null <- !ids_null

  # provider_code is considered as ids in some cases
  if (
    provider_code_not_null & dataset_code_null & dimensions_null & mask_null &
    ids_null & getOption("rdbnomics.rdb_no_arg")
  ) {
    fcall <- sys.call()
    modif_arg <- call_ok(fcall)

    if (modif_arg) {
      ids <- provider_code
      provider_code <- NULL

      provider_code_null <- TRUE
      provider_code_not_null <- !provider_code_null

      ids_null <- FALSE
      ids_not_null <- !ids_null
    }
  }

  # ids is considered as mask in some cases
  if (
    provider_code_not_null & dataset_code_not_null & dimensions_null &
    mask_null & ids_not_null & getOption("rdbnomics.rdb_no_arg")
  ) {
    fcall <- sys.call()
    modif_arg <- call_ok(fcall)

    if (modif_arg) {
      mask <- ids
      ids <- NULL

      mask_null <- FALSE
      mask_not_null <- !mask_null

      ids_null <- TRUE
      ids_not_null <- !ids_null
    }
  }

  # By dimensions
  if (dimensions_not_null) {
    if (provider_code_null | dataset_code_null) {
      stop(
        paste0(
          "When you filter with 'dimensions', you must specifiy ",
          "'provider_code' and 'dataset_code' as arguments of the function."
        ),
        call. = FALSE
      )
    }

    dimensions <- to_json_if_list(dimensions)
    check_argument(dimensions, c("character", "json"), not_null = FALSE)
    check_argument(provider_code, "character", not_null = FALSE)
    check_argument(dataset_code, "character", not_null = FALSE)

    if (api_version == 21) {
      link <- paste0(
        api_base_url, "?provider_code=", provider_code,
        "&dataset_code=", dataset_code, "&dimensions=", dimensions
      )
    } else if (api_version == 22) {
      link <- paste0(
        api_base_url, "/", provider_code, "/", dataset_code,
        ifelse(metadata, "?", paste0("?metadata=", as.numeric(metadata), "&")),
        "observations=1&dimensions=", dimensions
      )
    } else {
      stop(
        paste0("Don't know what to do for API version ", api_version, "."),
        call. = FALSE
      )
    }

    return(rdb_by_api_link(api_link = link, filters = filters, ...))
  }

  # By mask
  if (mask_not_null) {
    if (provider_code_null | dataset_code_null) {
      stop(
        paste0(
          "When you filter with 'mask', you must specifiy 'provider_code' ",
          "and 'dataset_code' as arguments of the function."
        ),
        call. = FALSE
      )
    }

    check_argument(mask, "character", not_null = FALSE)
    check_argument(provider_code, "character", not_null = FALSE)
    check_argument(dataset_code, "character", not_null = FALSE)

    if (api_version == 21) {
      link <- paste0(
        api_base_url, "?provider_code=", provider_code,
        "&dataset_code=", dataset_code, "&series_code_mask=", mask
      )
    } else if (api_version == 22) {
      link <- paste0(
        api_base_url, "/", provider_code, "/", dataset_code,
        "/", mask,
        ifelse(metadata, "?", paste0("?metadata=", as.numeric(metadata), "&")),
        "observations=1"
      )
    } else {
      stop(
        paste0("Don't know what to do for API version ", api_version, "."),
        call. = FALSE
      )
    }

    return(rdb_by_api_link(api_link = link, filters = filters, ...))
  }

  # By ids
  if (ids_not_null) {
    if (provider_code_not_null | dataset_code_not_null) {
      if (verbose) {
        warning(
          paste0(
            "When you filter with 'ids', ",
            "'provider_code' and 'dataset_code' are not considered."
          )
        )
      }
    }

    if (!is.character(ids)) {
      stop("'ids' must be of class 'character'.", call. = FALSE)
    }
    if (length(ids) <= 0) {
      stop("'ids' is empty.", call. = FALSE)
    }

    if (api_version == 21) {
      link <- paste0(
        api_base_url, "?series_ids=", paste(ids, collapse = ",")
      )
    } else if (api_version == 22) {
      link <- paste0(
        api_base_url,
        ifelse(metadata, "?", paste0("?metadata=", as.numeric(metadata), "&")),
        "observations=1&series_ids=",
        paste(ids, collapse = ",")
      )
    } else {
      stop(
        paste0("Don't know what to do for API version ", api_version, "."),
        call. = FALSE
      )
    }

    return(rdb_by_api_link(api_link = link, filters = filters, ...))
  }

  stop(
    "Please provide correct 'dimensions', 'mask', 'ids' or 'filters'.",
    call. = FALSE
  )
}
