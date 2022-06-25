# API Filtre data_democratie

library(plumber)
library(tidyverse)
library(future)
library(data.table)
path <- "C:/Users/Quentin GOLLENTZ/Documents/PROJET PERSO/bureaudevote/"

setwd(paste0(path,"/data/data_democratie"))
list.files()

vote_final <- fread("data_democratie_2.csv")  %>%
  filter(str_trim(nom_loi)!="")

#* @filter logger 
#* Log some information about the incoming request
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

#' @filter cors 
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
  
}

#* @apiTitle Filter data_democratie   
#* @apiDescription  L'objectif est en fonction d'un filtre par exemple sur une loi de filtrer les données  
#* @param filtre_loi Le filtre fait par le client  
#* @get /filer_data_democratie
#* @response 200 Retourne un filtre de data democratie  
function(filtre_loi) {
  future({
    vote_final%>%
      filter(nom_loi==filtre_loi)%>%
      na.omit()
  })
}