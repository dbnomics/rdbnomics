#' Download DBnomics data.
#'
#' \code{rdb} downloads data series from
#' \href{https://db.nomics.world/}{DBnomics}.
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
#' @param dimensions Character string (single quote). DBnomics
#' code of one or several dimensions in the specified provider and dataset.
#' @param mask Character string. DBnomics code of one or several dimensions
#' in the specified provider and dataset.
#' @param api_base_url Character string. DBnomics API link.
#' @param verbose Logical (default \code{getOption("rdbnomics.verbose_warning")}).
#' Show warnings of the function.
#' @param ... Arguments to be passed to \code{\link{rdb_by_api_link}}. For
#' example, you can set \code{use_readLines = TRUE} to request and read the data
#' with the base function \code{readLines}. This can be used to get round the error
#' \code{Could not resolve host: api.db.nomics.world}.
#' @return A data.frame.
#' @examples
#' \dontrun{
#' # By ids
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb(ids='AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb(ids=c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN','AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))
#' # Fetch two series from different datasets of different providers:
#' df3 <- rdb(ids=c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN','IMF/CPI/A.AT.PCPIT_IX'))
#' 
#' # By dimensions
#' # Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb('AMECO','ZUTN',dimensions='{"geo": ["ea12"]}')
#' # Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb('AMECO','ZUTN',dimensions='{"geo": ["ea12", "dnk"]}')
#' # Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank:
#' dim <- '{"country": ["DZ", "PE"],"indicator": ["ENF.CONT.COEN.COST.ZS","IC.REG.COST.PC.FE.ZS"]}'
#' df3 <- rdb('WB', 'DB', dimensions = dim)
#' 
#' # By mask (only for some providers, check the list here :
#' # https://git.nomics.world/dbnomics/dbnomics-api/blob/master/dbnomics_api/application.cfg.)
#' # Fetch one series from dataset 'Consumer Price Index' (CPI) of IMF:
#' df1 <- rdb('IMF','CPI',mask='M.DE.PCPIEC_WT')
#' # Fetch two series from dataset 'Consumer Price Index' (CPI) of IMF:
#' df2 <- rdb('IMF','CPI',mask='M.DE+FR.PCPIEC_WT')
#' # Fetch all series along one dimension from dataset 'Consumer Price Index' (CPI) of IMF:
#' df3 <- rdb('IMF','CPI',mask='M..PCPIEC_WT')
#' # Fetch series along multiple dimensions from dataset 'Consumer Price Index' (CPI) of IMF:
#' df4 <- rdb('IMF','CPI',mask='M..PCPIEC_IX+PCPIA_IX')
#' 
#' # Use readLines before fromJSON to avoid a proxy failure
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' options(rdbnomics.use_readLines = TRUE)
#' df1 <- rdb(ids='AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # or
#' df1 <- rdb(ids='AMECO/ZUTN/EA19.1.0.0.0.ZUTN', use_readLines = TRUE)
#' }
#' @seealso \code{\link{rdb_by_api_link}}
#' @export
rdb <- function(
  provider_code = NULL, dataset_code = NULL, ids = NULL,
  dimensions = NULL, mask = NULL,
  api_base_url = "https://api.db.nomics.world/series?",
  verbose = getOption("rdbnomics.verbose_warning"),
  ...
) {
  # Checking 'verbose'
  stopifnot(!is.null(verbose))
  stopifnot(is.logical(verbose), length(verbose) == 1)

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

    stopifnot(is.character(dimensions), length(dimensions) == 1)
    stopifnot(is.character(provider_code), length(provider_code) == 1)
    stopifnot(is.character(dataset_code), length(dataset_code) == 1)

    api_link <- paste0(
      api_base_url,
      "provider_code=", provider_code,
      "&dataset_code=", dataset_code,
      "&dimensions=", dimensions
    )

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

    stopifnot(is.character(mask), length(mask) == 1)
    stopifnot(is.character(provider_code), length(provider_code) == 1)
    stopifnot(is.character(dataset_code), length(dataset_code) == 1)

    mcp <- getOption("rdbnomics.mask_compatible_providers")
    mcp_null <- is.null(mcp)
    mcp_not_null <- !mcp_null

    if (mcp_not_null) {
      stopifnot(is.character(mcp), length(mcp) > 0)

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

    api_link <- paste0(
      api_base_url,
      "provider_code=", provider_code,
      "&dataset_code=", dataset_code,
      "&series_code_mask=", mask
    )
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

    stopifnot(is.character(ids), length(ids) > 0)

    api_link <- paste0(
      api_base_url, "series_ids=", paste(ids, collapse = ",")
    )
    return(rdb_by_api_link(api_link, ...))
  }

  stop("Please provide correct 'dimensions', 'mask' or 'ids'.")
}
