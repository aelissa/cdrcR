#' Login to the CDRC
#'
#' This function will log in a CDRC user based on the username and password. If you do not
#' have a CDRC user yet, please register on https://apps.cdrc.ac.uk/datasetportal/Identity/Account/Register
#' When you log in an api token is automatically generated, saved in your R environment and loaded, no further action is required from you to access the API.
#'
#' For safety reasons the API token will expire in 24h. This means that after 24h you need to log in again to generate a new API token which will be self-updated and load.
#'
#'
#' @param username A character-string with your CDRC username.
#' @param password A character-string with your CDRC password.
#' @return A response message which confirms the login was successful and how to see the API key.
#' @examples \dontrun{
#' loginCDRC(name='your-username',password='your-password')
#' }
#' @export

loginCDRC<-function(username,password){

  if(!is.character(username))stop("Username must be a string")

  request_body <- data.frame(
    username = username,
    password = password
  )

  request_body_json <- rjson::toJSON(request_body)
  result <- httr::POST("https://api.cdrc.ac.uk/Login",
                       body = request_body_json,
                       httr::accept("*/*"),
                       httr::add_headers(.headers = c("Content-Type"="application/json")))
  Output <- httr::content(result)
  print(Output[["message"]])

  save_api_key(Output[["token"]],Output[["message"]])

}


save_api_key<-function(key, message){
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")
  if(file.exists(renv)){
    # Backup original .Renviron before doing anything else here.
    file.copy(renv, file.path(home, ".Renviron_backup"))
  }
  if(!file.exists(renv)){
    file.create(renv)
  }
  tv <- readLines(renv)
  if(!any(grepl("CDRC_API_KEY",tv))){
    keyconcat <- paste0("CDRC_API_KEY='", key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    readRenviron("~/.Renviron")
    if(message=="Login successful"){
    message('Your API token has been stored in your .Renviron and loaded. You can now access the CDRC API! To see your token run Sys.getenv("CDRC_API_KEY").') #\nTo use now, restart R or run `readRenviron("~/.Renviron")`
    }else{message('Login must be successful to load the API token.')}
  } else {
  oldenv<-utils::read.table(renv, stringsAsFactors = FALSE)
  newenv <- oldenv[- grepl("CDRC_API_KEY", oldenv),]
  keyconcat <- paste0("CDRC_API_KEY='", key, "'")
  utils::write.table(newenv, renv, quote = FALSE, sep = "\n",
              col.names = FALSE, row.names = FALSE)
  # Append updated API key to .Renviron file
  write(keyconcat, renv, sep = "\n", append = TRUE)
  readRenviron("~/.Renviron")
  if(message=="Login successful"){
    message('Your API token has been stored in your .Renviron and loaded. You can now access the CDRC API! To see your token run Sys.getenv("CDRC_API_KEY").') #\nTo use now, restart R or run `readRenviron("~/.Renviron")`
  }else{message('Login must be successful to load the API token.')}
  }
}


