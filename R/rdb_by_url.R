#' Download DBnomics data using url.
#'
#' \code{rdb_by_url} downloads data series from
#' \href{https://next.nomics.world/}{DBnomics}.
#'
#' This function gives you access to hundreds of millions data series from
#'  \href{https://api.next.nomics.world/}{DBnomics API} (documentation about
#'   the API can be found \href{https://api.next.nomics.world/apidocs}{here}).
#'   The url is given on the
#'   \href{https://next.nomics.world/}{DBnomics website}.
#'
#' @param url_code Character string. DBnomics url of the search.
#' @return A data frame.
#' @examples
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb_by_url('https://api.next.nomics.world/AMECO/ZUTN?series_codes=EA19.1.0.0.0.ZUTN')
#'
#' # Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb_by_url('https://api.next.nomics.world/AMECO/ZUTN?series_codes=EA19.1.0.0.0.ZUTN,DNK.1.0.0.0.ZUTN')
#'
#' # Fetch one dataset 'Exports and imports by Member States of the EU/third countries'
#'  (namq_10_exi) of Eurostat provider:
#' df3 <- rdb_by_url('https://api.next.nomics.world/Eurostat/namq_10_exi')
#' @import jsonlite dplyr tidyr
#' @seealso \code{\link{rdb_by_codes}} \code{\link{rdb_by_dimensions}}
#' @export

rdb_by_url <- function(url_code){

  DBlist <- jsonlite::fromJSON(url_code)
  DBdata <- list()
  DBdata[[1]] <- DBlist$series$data

  num_found <- DBlist$series$num_found
  limit <- DBlist$series$limit

  if (num_found > limit){

    n_iter <- ceiling(num_found/limit)-1

    for (i in 1:n_iter){
      url_code2 <- paste0(url_code,"?offset=",paste(i*limit))
      DBlist <- jsonlite::fromJSON(url_code2)
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
