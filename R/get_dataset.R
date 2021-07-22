#' Get data from the CDRC
#'
#' Obtain data from the CDRC datasets. To find out what datasets are available and their respective dataCode run `list_datasets()`.
#'
#'
#' @param dataCode A character-string API identifier associated which each dataset in the CDRC. To find out the dataCode of your desired dataset run `list_datasets()`.
#' @param geography The geographical levels in which the data can be retrieved. It can be postcode, MSOA or LSOA.
#' Note that the geography in which the data are retrieved does not necessarily correspond with the geography of the data. For example, it is possible to query data of the AHAH index by postcodes although the index is originally at LSOA level. Therefore you will see returned the LSOAs that better match the required postcodes.
#' @param geographyCode A character-vector of one or more postcodes, LSOA codes or MSOA codes.
#' @param boundaries if FALSE (the default), returns a data frame of the desired data. if TRUE, uses the Open Geography Portal API to return an sf with the 'geometry' column.
#' @return A dataframe or sf depending whether boundaries are set to FALSE or TRUE respectively.
#' @examples \dontrun{
#' ahah_data <- get_dataset("AHAHOverallIndexDomain",geography = "postcode", geographyCode = c("CH430UQ","LS61EF","L83UL"), boundaries = TRUE)
#' View(ahah_data)
#' plot(ahah_data$geometry)
#' }
#' @export

get_dataset<-function(dataCode,geography=c("postcode","MSOA","LSOA"),geographyCode,boundaries=FALSE){

  geography <- match.arg(geography)
  if(any(!is.character(geographyCode)))stop("geographyCode needs to be a character vector.")
  if(geography=="MSOA"){geography="msoaCode"}
  if(geography=="LSOA"){geography="lsoaCode"}
  if(geography=="postcode"){geography="postCode"}

  data_list<-list_datasets()
  if(!any(grepl(dataCode,data_list$DataCode)))stop("The dataCode is not in the list of available datasets. Check the list with `list_datasets()`.")

  ####check geographyCode length

  if(length(geographyCode)==1){
    url <- paste0("https://api.cdrc.ac.uk/v1/",
                  dataCode,
                  "/",
                  geography,
                  "=",
                  geographyCode)
    single_code<-TRUE
  }
  else{
    single_code<-FALSE
    if(geography=="msoaCode"){
      geography<-"msoaCodes"
    }
    if(geography=="lsoaCode"){
      geography<-"lsoaCodes"
    }
    if(geography=="postCode"){
      geography<-"postCodes"
    }

    url<-paste0("https://api.cdrc.ac.uk/v1/",
                dataCode,
                "/",
                geography)
  }

  token<-paste0("Bearer ",Sys.getenv("CDRC_API_KEY"))

  if (single_code){
    call<-httr::GET(url,httr::add_headers(Authorization = token))
  } else {
    request_body_json <- rjson::toJSON(geographyCode)
    call <- httr::POST(url,
                       body = request_body_json,
                       httr::accept("*/*"),
                       httr::add_headers(Authorization = token,.headers = c("Content-Type"="application/json")))
  }
  if(httr::http_error(call)){
    if (call$status_code == 500){
      message("The token is expired. Login again with CDRC_login() to generate a new one.")
    }else{
      message(httr::message_for_status(call,"get the data"))
    }
    stop()
  }
  else{
    data<-httr::content(call,as = "text")
    data<-jsonlite::fromJSON(data)
    if(single_code==TRUE){
      data<-as.data.frame(data[[2]])
    }
    else{
      data<-as.data.frame(rlist::list.rbind(data[[2]]))
      if(any(duplicated(data[,1]))){
        data<-unique(data)
      }
    }
  }

  if(boundaries){
    data_list<-dplyr::filter(data_list, grepl(dataCode, DataCode))
    data<-get_boundaries(data,data_list$GeographyLevel,single_code)
  }

  return(data)
}

get_boundaries<-function(data,geo,single_code){
  #codeList<-paste(data[[c(1)]],sep = "",collapse = "', '")
  if(geo =="LSOA"){
    ogpURL<-"https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_V2/FeatureServer/0/query"
    geocode<-"LSOA11CD"
    geocode_<-"LSOA11CD"
    geocode__<-"LSOA11CD"
  }
  if(geo =="OA"){
    ogpURL<-"https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_December_2011_Boundaries_EW_BGC/FeatureServer/0/query"
    geocode<-"OA11CD"
    geocode_<-"outputArea"
    geocode__<-"outputArea"
  }
  if(geo =="WZ"){
    ogpURL<-"https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Workplace_Zone_December_2011_Boundaries/MapServer/0/query"
    geocode<-"wz11cd"
    geocode_<-"wzCode"
    geocode__<-"workPlaceZone"
  }
  cd<-data %>% dplyr::select(contains(geocode)|contains(geocode_)|contains(geocode__))

  if(nrow(cd)==1){
    whereClause<-urltools::url_encode(paste0("?where=",cd[,1]))
    ogpURL<-paste0(ogpURL,whereClause,"&outFields=*&outSR=4326&f=json")
    sf <- sf::st_read(ogpURL)
  }else{
    seq<-splitAt(cd[,1],50)
    clause<-seq %>% purrr::map(~paste(.x,sep = "",collapse = "', '"))
    clause<-clause %>% purrr::map(~paste0(geocode," IN ('",noquote(.x),"')"))
    req <- clause %>% purrr::map(~ httr::POST(url = ogpURL,
                                              body = list(where= .x,
                                              outfields="*",
                                              outSR = '4326',
                                              f='json'),
                                .headers = c("application/x-www-form-urlencoded"))
                                )
    sf <- req %>% purrr::map_df(~sf::read_sf(httr::content(.x,type='text',encoding='UTF-8')))
  }

  sf <- sf[c("geometry")]
  data<-sf::st_as_sf(cbind(data,sf))
  return(data)
}

splitAt <- function(x, pos) {
  unname(split(x,rep(c(0:(round(length(x)/pos))),each=pos)[1:length(x)]))
}









