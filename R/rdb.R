#' Download DBnomics data.
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
#' @param ids Character string. DBnomics
#'  code of one or several series.
#' @param dimensions Character string (single quote). DBnomics
#'  code of one or several dimensions in the specified provider and dataset.
#' @param mask Character string. DBnomics
#'  code of one or several dimensions in the specified provider and dataset.
#' @param api_base_url Character string. DBnomics API link.
#' @return A data frame.
#' @examples
#' By id
#' # Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb(ids='AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
#' # Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb(ids=c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN','AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))
#' # Fetch two series from different datasets of different providers:
#' df3 <- rdb(ids=c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN','IMF/CPI/A.AT.PCPIT_IX'))
#' 
#' By dimension
#' # Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df1 <- rdb('AMECO','ZUTN',dimensions='{"geo": ["ea12"]}')
#' # Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
#' df2 <- rdb('AMECO','ZUTN',dimensions='{"geo": ["ea12", "dnk"]}')
#' # Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank:
#' df3 <- rdb('WB','DB',dimensions='{"country": ["DZ", "BT", "PE"],"indicator": ["IC.DCP.BQCI","IC.REG.COST.PC.ZS"]}')
#' 
#' By mask (only for some providers, check the list here : https://git.nomics.world/dbnomics/dbnomics-api/blob/master/dbnomics_api/application.cfg.)
#' # Fetch one series from dataset 'Consumer Price Index' (CPI) of IMF:
#' df1 <- rdb('IMF','CPI',mask='M.DE.PCPIEC_WT')
#' # Fetch two series from dataset 'Consumer Price Index' (CPI) of IMF:
#' df2 <- rdb('IMF','CPI',mask='M.DE+FR.PCPIEC_WT')
#' # Fetch all series along one dimension from dataset 'Consumer Price Index' (CPI) of IMF:
#' df3 <- rdb('IMF','CPI',mask='M..PCPIEC_WT')
#' # Fetch series along multiple dimensions from dataset 'Consumer Price Index' (CPI) of IMF:
#' df4 <- rdb('IMF','CPI',mask='M..PCPIEC_IX+PCPIA_IX')
#' 
#' @import jsonlite dplyr tidyr stringr
#' @seealso \code{\link{rdb_by_api_link}}
#' @export

rdb <- function(provider_code=NULL,dataset_code=NULL,ids=NULL,dimensions=NULL,mask=NULL,
                api_base_url="https://api.next.nomics.world/series?"){
  
  if (!is.null(dimensions)){
    
    if (!is.null(provider_code) & !is.null(dataset_code)){

      api_link <- paste0(api_base_url,
                         "provider_code=",provider_code,
                         "&dataset_code=",dataset_code,
                         "&dimensions=",dimensions)
      rdb_by_api_link(api_link)
      
    } else {
      
      stop("When you filter with dimensions, you must specifiy provider_code and dataset_code as arguments of the function.")
    
    }
    

  } else {
    
    if (!is.null(mask)){
      
      if (!is.null(provider_code) & !is.null(dataset_code)){
        
        api_link <- paste0(api_base_url,
                           "provider_code=",provider_code,
                           "&dataset_code=",dataset_code,
                           "&series_code_mask=",mask)
        rdb_by_api_link(api_link) 
        
      } else {
        
        stop("When you filter with mask, you must specifiy provider_code and dataset_code as arguments of the function.")
      
      }
      
    } else {
      
      if (!is.null(ids)){
        
        if (!is.null(provider_code) | !is.null(dataset_code)){
          
          stop("When you filter with ids, you must not specifiy provider_code nor dataset_code as arguments of the function.") 
          
        } else {
        
          api_link <- paste0(api_base_url,"series_ids=",paste(ids,collapse=","))
          rdb_by_api_link(api_link)          
          
        }
        
      }
      
    }

  }

}
