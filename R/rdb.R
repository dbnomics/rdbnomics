#' Download DBnomics data
#'
#' \code{rdb} downloads data series from
#' \href{https://next.nomics.world/}{DBnomics}.
#'
#' This function gives you access to hundreds of millions data series from
#'  \href{https://api.next.nomics.world/}{DBnomics API} (documentation about
#'   the API can be found \href{https://api.next.nomics.world/apidocs}{here}).
#'   The code of each series is given on the
#'   \href{https://next.nomics.world/}{DBnomics website}.
#'
#' @param provider_code Character string. DBnomics code of the provider.
#' @param dataset_code Character string. DBnomics code of the dataset.
#' @param series_code Character string or vector of character strings. DBnomics
#'  code of one or several series in the specified dataset.
#' @param api_base_url Character string. DBnomics API link.
#' @return A data frame.
#' @examples
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb('AMECO','ZUTN','EA19.1.0.0.0.ZUTN')
#'
#' # Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb('AMECO','ZUTN',c('EA19.1.0.0.0.ZUTN','DNK.1.0.0.0.ZUTN'))
#'
#' # Fetch one dataset 'Exports and imports by Member States of the EU/third countries'
#'  (namq_10_exi) of Eurostat provider:
#' df3 <- rdb('Eurostat','namq_10_exi')
#' @import jsonlite dplyr tidyr
#' @export

rdb <- function(provider_code,dataset_code,series_code=NA,api_base_url="https://api.next.nomics.world"){

  if (is.na(series_code[1])) {
    url <- paste0(api_base_url,"/",provider_code,"/",dataset_code)
  } else {
    url_series_code <- paste0(series_code, collapse = "&series_code=")
    url <- paste0(api_base_url,"/",provider_code,"/",dataset_code,"/?series_code=",url_series_code)
  }

  DBlist <- jsonlite::fromJSON(url)
  DBdata <- list()
  DBdata[[1]] <- DBlist$series$data

  num_found <- DBlist$series$num_found
  limit <- DBlist$series$limit

  if (num_found > limit){

    n_iter <- ceiling(num_found/limit)-1

    for (i in 1:n_iter){
      url2 <- paste0(url,"?offset=",paste(i*limit))
      DBlist <- jsonlite::fromJSON(url2)
      DBdata[[i+1]] <- DBlist$series$data
    }

  }

  DBdf <- jsonlite::rbind_pages(DBdata)

  if("OBS_STATUS" %in% colnames(DBdf))
    DBdf <- DBdf %>% select(-OBS_STATUS)

  DBdf <- 
    DBdf %>%
    unnest() %>%
    mutate(dataset_code=DBlist$dataset$code,
           dataset_name=DBlist$dataset$name)

}
