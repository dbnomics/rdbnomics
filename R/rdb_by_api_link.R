#' Download DBnomics data using API link.
#'
#' \code{rdb_by_api_link} downloads data series from
#' \href{https://next.nomics.world/}{DBnomics}.
#'
#' This function gives you access to hundreds of millions data series from
#'  \href{https://api.next.nomics.world/}{DBnomics API} (documentation about
#'   the API can be found \href{https://api.next.nomics.world/apidocs}{here}).
#'   The API link is given on the
#'   \href{https://next.nomics.world/}{DBnomics website}.
#'
#' @param api_link Character string. DBnomics API link of the search.
#' @return A data frame.
#' @examples
#' # Fetch two series from different datasets of different providers:
#' df1 <- rdb_by_api_link('https://api.next.nomics.world/series?series_ids=AMECO/ZUTN/EA19.1.0.0.0.ZUTN,IMF/CPI/A.AT.PCPIT_IX')
#' 
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb_by_api_link('https://api.next.nomics.world/series?provider_code=AMECO&dataset_code=ZUTN&dimensions=%7B%22geo%22%3A%5B%22ea12%22%5D%7D')
#'
#' @import jsonlite dplyr tidyr stringr
#' @seealso \code{\link{rdb}}
#' @export

rdb_by_api_link <- function(api_link){

  DBlist <- jsonlite::fromJSON(api_link)
  DBdata <- list()
  DBdata[[1]] <- DBlist$series$data

  num_found <- DBlist$series$num_found
  limit <- DBlist$series$limit

  if (num_found > limit){

    n_iter <- ceiling(num_found/limit)-1

    for (i in 1:n_iter){
      api_link2 <- paste0(api_link,"?offset=",paste(i*limit))
      DBlist <- jsonlite::fromJSON(api_link2)
      DBdata[[i+1]] <- DBlist$series$data
    }

  }

  DBdf <- jsonlite::rbind_pages(DBdata)
  
  DBdf <-
    DBdf %>%
    unnest(period,value)

}
