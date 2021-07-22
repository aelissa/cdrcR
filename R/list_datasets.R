#' This function returns a list of all CDRC datasets that can be retrieved with this package. This function takes no arguments.
#'
#' @return A dataframe of the datasets available to request with the API.
#' @examples \dontrun{
#' dataset_list <- list_data()
#' View(dataset_list)
#' }
#' @export
#'

list_datasets<-function(){
  token<-paste0("Bearer ",Sys.getenv("CDRC_API_KEY"))
  records <- httr::GET("https://api.cdrc.ac.uk/v1/DataSetMetaData",httr::add_headers(Authorization = token))
  if(httr::http_error(records)){
    if (records$status_code == 500){
      message("The token is expired. Login again with CDRC_login() to generate a new one.")
    }else{
      print(httr::message_for_status(records,"get the datasets list"))
      }
  }
  else{
    records<-httr::content(records)
  }

  datacode<-unnest_datacode(records)
  records<-as.data.frame(do.call(rbind,records))
  geography_level<-purrr:::map(records[["granularities"]],unlist)
  geography_level<-purrr:::map(geography_level,1)

  records$DataCode<-datacode
  records$GeographyLevel<-do.call(rbind,geography_level)
  records$Title<-do.call(rbind,records$title)
  records$dataSetURL<-do.call(rbind,records$dataSetURL)
  records$GeographicalCoverage<-do.call(rbind,records$spatialGeographicalCoverageLocation)
  records<-records[c("Title","DataCode","dataSetURL","GeographicalCoverage","GeographyLevel")]

  return(records)
}

unnest_datacode<-function(records){
  datacode<-list()
    for (i in 1:length(records)) {
      tmp<-purrr:::map(records[[i]][["apiEndpoints"]],unlist)
      tmp<-purrr:::map(tmp,"url")
      tmp<-do.call(rbind,tmp)
      tmp<-sub(".*https://api.cdrc.ac.uk/v1/", "", tmp)
      tmp<-sub("/.*", "", tmp)
      tmp<-as.data.frame(tmp)
      tmp<-paste(tmp[!(duplicated(tmp$V1) | tmp$V1=="" | tmp$V1=="https:"),],sep = "",collapse = ", ")
      datacode[[i]]<-tmp
    }
  datacode<-do.call(rbind,datacode)
  return(datacode)
}

