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
#' the data are requested and read with the base function \code{readLines}.
#' This can be used to get round the error \code{Could not resolve host: api.db.nomics.world}.
#' @return A \code{data.frame}, a \code{data.table} or a vector.
#' @examples
#' \dontrun{
#' rdb_providers()
#' #>    code         converted_at           created_at               indexed_at  ...
#' #> 1:  AFDB 2018-12-21T17:04:45Z 2018-12-21T17:04:45Z 2018-12-21T17:05:44.751Z ...
#' #> 2: AMECO 2019-01-08T10:11:32Z 2017-03-03T16:31:12Z 2019-01-08T10:11:38.800Z ...
#' #> 3:   BCB 2019-01-08T17:14:07Z 2018-06-06T09:52:03Z 2019-01-08T17:14:11.510Z ...
#' #> 4: BCEAO 2019-01-07T03:35:01Z 2017-12-12T14:55:17Z 2019-01-07T18:36:41.997Z ...
#' #> 5:   BEA 2019-01-09T03:58:46Z 2018-03-13T17:34:38Z 2019-01-09T03:59:30.970Z ...
#' #> 6:   BIS 2018-12-20T04:10:50Z 2018-02-21T18:58:15Z 2018-12-20T04:11:05.478Z ...
#' #> ...
#' 
#' rdb_providers(code = TRUE)
#' #>  [1] "AFDB"        "AMECO"       "BCB"         "BCEAO"       "BEA"         ...  
#' #>  [8] "BOE"         "BOJ"         "BUBA"        "CBO"         "CEPII"       ...  
#' #> [15] "DESTATIS"    "DREES"       "ECB"         "EIA"         "ELSTAT"      ...  
#' #> [22] "FED"         "FHFA"        "GGDC"        "ILO"         "IMF"         ...  
#' #> [29] "INEPT"       "INSEE"       "ISM"         "LBMA"        "METI"        ...  
#' #> [36] "OECD"        "ONS"         "pole-emploi" "RBA"         "ROSSTAT"     ...  
#' #> [43] "SECO"        "STATCAN"     "STATJP"      "STATPOL"     "TCMB"        ...  
#' #> [50] "WTO"
#' }
#' @seealso \code{\link{rdb_last_updates}}
#' @export
rdb_providers <- function(
  code = FALSE, use_readLines = getOption("rdbnomics.use_readLines")
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
  providers <- read_lines(use_readLines, providers)
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
