#' Download DBnomics data.
#'
#' \code{rdb} downloads data series from
#' \href{https://db.nomics.world/}{DBnomics} using shortcuts like \code{ids},
#' \code{dimensions} or \code{mask}.
#'
#' This function gives you access to hundreds of millions data series from
#' \href{https://api.db.nomics.world/}{DBnomics API} (documentation about
#' the API can be found \href{https://api.db.nomics.world/apidocs}{here}).
#' The code of each series is given on the
#' \href{https://db.nomics.world/}{DBnomics website}.
#'
#' @param provider_code Character string. DBnomics code of the provider.
#' @param dataset_code Character string. DBnomics code of the dataset.
#' @param ids Character string. DBnomics code of one or several series.
#' @param dimensions List or character string (single quoted). DBnomics
#' code of one or several dimensions in the specified provider and dataset.
#' If it is a named list, then the function \code{toJSON} (from the
#' package \pkg{jsonlite}) is applied to generate the json object.
#' @param mask Character string. DBnomics code of one or several masks
#' in the specified provider and dataset.
#' @param verbose Logical (default \code{getOption("rdbnomics.verbose_warning")}).
#' Show warnings of the function.
#' @param ... Arguments to be passed to \code{\link{rdb_by_api_link}}. These 
#' arguments concern proxy configuration. See \code{\link{rdb_by_api_link}}
#' for details.
#' @return A \code{data.frame} or a \code{data.table}.
#' @examples
#' \dontrun{
#' ## By ids
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' 
#' # Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))
#' 
#' # Fetch two series from different datasets of different providers:
#' df3 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'IMF/CPI/A.AT.PCPIT_IX'))
#' 
#' 
#' ## By dimensions
#' # Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb('AMECO', 'ZUTN', dimensions = list(geo = "ea12"))
#' # or
#' df1 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12"]}')
#' 
#' # Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb('AMECO', 'ZUTN', dimensions = list(geo = c("ea12", "dnk")))
#' # or
#' df2 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12", "dnk"]}')
#' 
#' # Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank:
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
#' ## By mask (only for some providers, check the list here :
#' # https://git.nomics.world/dbnomics/dbnomics-api/blob/master/dbnomics_api/application.cfg.)
#' # Fetch one series from dataset 'Consumer Price Index' (CPI) of IMF:
#' df1 <- rdb('IMF', 'CPI', mask = 'M.DE.PCPIEC_WT')
#' 
#' # Fetch two series from dataset 'Consumer Price Index' (CPI) of IMF:
#' df2 <- rdb('IMF', 'CPI', mask = 'M.DE+FR.PCPIEC_WT')
#' 
#' # Fetch all series along one dimension from dataset 'Consumer Price Index' (CPI) of IMF:
#' df3 <- rdb('IMF', 'CPI', mask = 'M..PCPIEC_WT')
#' 
#' # Fetch series along multiple dimensions from dataset 'Consumer Price Index' (CPI) of IMF:
#' df4 <- rdb('IMF', 'CPI', mask = 'M..PCPIEC_IX+PCPIA_IX')
#' 
#' 
#' ## Use a specific proxy to fetch the data
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' h <- curl::new_handle(
#'   proxy = "<proxy>",
#'   proxyport = <port>,
#'   proxyusername = "<username>",
#'   proxypassword = "<password>"
#' )
#' options(rdbnomics.curl_config = h)
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # or to use once
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN', curl_config = h)
#' 
#' 
#' ## Use R default connection to avoid a proxy failure (in some cases)
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' options(rdbnomics.use_readLines = TRUE)
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # or to use once
#' df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN', use_readLines = TRUE)
#' }
#' @seealso \code{\link{rdb_by_api_link}}
#' @export
rdb <- function(
  provider_code = NULL, dataset_code = NULL,
  ids = NULL, dimensions = NULL, mask = NULL,
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

  # By dimensions
  if (dimensions_not_null) {
    if (provider_code_null | dataset_code_null) {
      stop(
        paste0(
          "When you filter with 'dimensions', you must specifiy ",
          "'provider_code' and 'dataset_code' as arguments of the function."
        )
      )
    }

    dimensions <- to_json_if_list(dimensions)
    check_argument(dimensions, c("character", "json"), not_null = FALSE)
    check_argument(provider_code, "character", not_null = FALSE)
    check_argument(dataset_code, "character", not_null = FALSE)

    if (api_version == 21) {
      api_link <- paste0(
        api_base_url, "?provider_code=", provider_code,
        "&dataset_code=", dataset_code, "&dimensions=", dimensions
      )
    } else if (api_version == 22) {
      api_link <- paste0(
        api_base_url, "/", provider_code, "/", dataset_code,
        "?observations=1&dimensions=", dimensions
      )
    } else {
      stop(paste0("Don't know what to do for API version ", api_version, "."))
    }

    return(rdb_by_api_link(api_link, ...))
  }

  # By mask
  if (mask_not_null) {
    if (provider_code_null | dataset_code_null) {
      stop(
        paste0(
          "When you filter with 'mask', you must specifiy 'provider_code' ",
          "and 'dataset_code' as arguments of the function."
        )
      )
    }

    check_argument(mask, "character", not_null = FALSE)
    check_argument(provider_code, "character", not_null = FALSE)
    check_argument(dataset_code, "character", not_null = FALSE)

    mcp <- getOption("rdbnomics.mask_compatible_providers")
    mcp_null <- is.null(mcp)
    mcp_not_null <- !mcp_null

    if (mcp_not_null) {
      if (!is.character(mcp)) {
        stop(
          paste0(
            "The vector of providers compatible with the argument 'mask' ",
            "must be of class 'character'."
          )
        )
      }
      if (length(mcp) <= 0) {
        stop(
          paste0(
            "The vector of providers compatible with the argument 'mask' ",
            "is empty."
          )
        )
      }

      if (provider_code %notin% mcp) {
        stop(
          paste0(
            "To use the argument 'mask', the provider must be in the ",
            "vector : '",
            paste0(mcp, collapse = "', '"),
            "'."
          )
        )
      }
    }

    if (api_version == 21) {
      api_link <- paste0(
        api_base_url, "?provider_code=", provider_code,
        "&dataset_code=", dataset_code, "&series_code_mask=", mask
      )
    } else if (api_version == 22) {
      api_link <- paste0(
        api_base_url, "/", provider_code, "/", dataset_code,
        "/", mask, "?observations=1"
      )
    } else {
      stop(paste0("Don't know what to do for API version ", api_version, "."))
    }

    return(rdb_by_api_link(api_link, ...))
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
      stop("'ids' must be of class 'character'.")
    }
    if (length(ids) <= 0) {
      stop("'ids' is empty.")
    }

    if (api_version == 21) {
      api_link <- paste0(
        api_base_url, "?series_ids=", paste(ids, collapse = ",")
      ) 
    } else if (api_version == 22) {
      api_link <- paste0(
        api_base_url, "?observations=1&series_ids=", paste(ids, collapse = ",")
      )
    } else {
      stop(paste0("Don't know what to do for API version ", api_version, "."))
    }

    return(rdb_by_api_link(api_link, ...))
  }

  stop("Please provide correct 'dimensions', 'mask' or 'ids'.")
}
