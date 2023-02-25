# API KEY : UWGxYHjThutaNKzpFSIYdA0Vc
# API KEY SECRET : lM0ZufWObUvQFUF6ARIgxdwKc5ukBsIngAsQaLAOzXCwncZX1F
# BEARER TOKEN : AAAAAAAAAAAAAAAAAAAAAMXQhgEAAAAAPSdyxhX2ZIeFRQqyXU3TT7ZvTrg%3DakptizDAt5qMYIY8x72VH2LWKWa8tdv05zD2p4Cw9XL9TD69p4
# ACCES TOKEN : 1545465600549523460-2I2rv0Z2CgkTMuy7pZAviJ8ikGDHKx
# ACCES TOKEN SECRET : cj3ruID6SqLv8OIoaSh6RgUuECUbSc1NhvO4rjnu0yizA
# CLIENT ID T3V1bjJ2T0Z0dUJFTFp2ekp3SGk6MTpjaQ
# CLIENT SECRET : 6g2VNW6JtTEVSao_r6gWFrSFe0pdDAZ4L9HcCYetnZAKV9w5M4

res <- VERB("POST", url = "https://api.twitter.com/2/tweets", body = body_function("HELLO WORLD"), add_headers(headers),encoding="UTF-8")
jsonRespText <- content(res, "text") 
fromJSON(jsonRespText)
##################
library(rtweet)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(lexicon)
library(textclean)
library(httr)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

headers = c(
  'Authorization' = 'OAuth oauth_consumer_key="UWGxYHjThutaNKzpFSIYdA0Vc",oauth_token="1545465600549523460-2I2rv0Z2CgkTMuy7pZAviJ8ikGDHKx",oauth_signature_method="HMAC-SHA1",oauth_timestamp="1676151064",oauth_nonce="GTxWnCRVWdh",oauth_version="1.0",oauth_signature="2Z7wzpgPQ%2BDYQkkSj4FXtCpLZAQ%3D"',
  'Content-Type' = 'application/json',
  'Cookie' = 'guest_id=v1%3A167614740576836029'
)

body_function = function(input_message) {
 paste0('{"text": "',input_message,'"}')
}
body_function_reply = function(input_message,reply_id) {
  chartr("áéèàôî","aeeaoi",paste0('{"text": "',input_message,'" ,"quote_tweet_id":"',reply_id, '"}'))
}
body_function_reply = function(input_message,reply_id) {
  paste0('{"text": "',input_message,'" ,"quote_tweet_id":"',reply_id, '"}')
}


data_deomcratie_v3 <-readRDS("C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/data/data_democratie/data_democratie_v3_twitter.rds")%>%
  mutate(uid_loi = as.character(uid_loi)) %>%
  select(-geometry)


data_depute <- read.csv("C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/data/data_depute/depute.csv",encoding="UTF-8") %>%
  rename(depute_code=id)%>%
  select(depute_code,twitter)

data_democratie_v3_jour <-  data_deomcratie_v3 %>%
  inner_join(data_depute,by="depute_code") %>%
  mutate(vote_code = case_when(vote_code == 1 ~ "POUR",
                               vote_code == 0 ~ "CONTRE",
                               vote_code == 0.5 ~ "ABSENTENTION",
                               is.na(vote_code)~ "ABSENT")) %>%
  filter(date_vote == as.character(Sys.Date()-88))  %>%
  #filter(date_vote == as.character(Sys.Date()))  %>%
    dplyr::mutate(
      script_titre =paste0(
        "Vote du jour à l'Assemblé Nationale : ",
        date_vote,
        " \\n ",
        firstup(titre)
      ),
    .keep = "unused"
  ) %>%
  dplyr::mutate(
    script_position =paste0(
      "Nom du députés : ",
      toupper(nom),
      " \\n ",
      "Prénom du députés : ",
      prenom,
      " \\n ",
      "Position : ",
      vote_code,
      " \\n ",
      "Mail : ",
      mail,   
      " \\n ",
      "Twitter : ",
      twitter
    ),
    .keep = "unused"
  ) %>%
  dplyr::mutate(
  script_amendement =paste0(
    "Nombre de votants : ",
    " \\n ",
    nb_votant,
    " \\n ",
    "Nombre d'absents : ",
    " \\n ",
    nb_absence,
    " \\n ",
    "Nombre d'abstentions : ",
    " \\n ",
    nb_abstentions,
    " \\n ",
    "Nombre de vote POUR : ",
    " \\n ",
    nb_pour,
    " \\n ",
    "Nombre de vote CONTRE : ",
    " \\n ",
    nb_contre
  ),
.keep = "unused"
) %>% select(script_position,script_amendement,script_titre,uid_loi)

uid_loi_jour <- data_democratie_v3_jour %>%
  select(uid_loi,script_amendement,script_titre) %>%
  unique()
uid_loi_jour$script_titre[i]
if (nrow(data_democratie_v3_jour)>0) {
for (i in 1:1) {
  res <- VERB("POST", url = "https://api.twitter.com/2/tweets", body = body_function(uid_loi_jour$script_titre[i]), add_headers(headers))
  jsonRespText <- content(res, "text") 
  jsonresponse <- fromJSON(jsonRespText)
  jsonresponse
  res <- VERB("POST", url = "https://api.twitter.com/2/tweets", body = body_function_reply(uid_loi_jour$script_amendement[i],jsonresponse$data$id), add_headers(headers))
  jsonRespText <- content(res, "text") 
  jsonresponse <- fromJSON(jsonRespText)
  jsonresponse
  data_democratie_v3_jour_amendement <- data_democratie_v3_jour %>%
    filter(uid_loi == uid_loi_jour$uid_loi[i])
  for(j in 1:8){
    res <- VERB("POST", url = "https://api.twitter.com/2/tweets", body = body_function_reply(data_democratie_v3_jour_amendement$script_position[j],jsonresponse$data$id), add_headers(headers))
    jsonRespText <- content(res, "text") 
    fromJSON(jsonRespText)
    }
  }
} else {
  res <- VERB("POST", url = "https://api.twitter.com/2/tweets", body = body_function("Pas de vote pour aujourd'hui"), add_headers(headers))
  jsonRespText <- content(res, "text") 
  fromJSON(jsonRespText)
}




