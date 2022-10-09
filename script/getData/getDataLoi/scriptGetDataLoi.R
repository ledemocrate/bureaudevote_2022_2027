######################################
# Création des fichiers amendement/loi
######################################

#LIBRAIRIE UTILISEE

library(rvest) #Pour le web scrapping
library(stringr) #Pour la manipulation textuelle
library(tidyverse) #Couteau suisse
library(purrr) #Pour certaine fonction
library(data.table)
library(jsonlite)  # Pour ouvrir fichier json

extract_num <- function(x) {as.numeric(gsub("[^0-9.-]+", "", as.character(x)))} 
#Permet d'éviter le warning deprecated lorsque utilisation de extract_numeric

path <-  getwd()

setwd(paste0(path,"/data/data_vote/json"))
#On recupere la liste des fichiers
liste_vote <- list.files()

#Fonction visant pour chaque fichier de la liste a recuperer les informations
read_data <- function(liste){
  #On lit le fichier de la liste
  vote <- fromJSON(liste, flatten=TRUE)
  
  #On replique pour chaque vote les informations de la loi pour laquelle il vote
  scrutin_numero <- vote[["scrutin"]][["numero"]]
  titre <- vote[["scrutin"]][["titre"]]
  date_vote <- vote[["scrutin"]][["dateScrutin"]]
  
  data <- data.frame(scrutin_numero,titre,date_vote)
  return(data)
}
#Application de la fonction pour chaque fichier de la liste
titre_vote <- lapply(liste_vote, read_data)

#Creation d'un data.frame
titre_vote <- bind_rows(titre_vote)

# Récupération du lien amendement/loi sur le site :
# https://www2.assemblee-nationale.fr/scrutins/liste/(offset)/{sequence_offset}/(legislature)/16/(type)/SOR/(idDossier)/TOUS

# Vérifié manuellement que la sequence_offset prend bien en compte l'ensemble des pages
sequence_offset <- c("",c(1:3)*100)
#Fonction permettant de récupérer les données d'une page
function_get_url_dosier_scrutin <- function(url){
  url <-
  print(url)
  url_prov <- paste0("https://www2.assemblee-nationale.fr/scrutins/liste/(offset)/",
                     url,
                     "/(legislature)/16/(type)/TOUS/(idDossier)/TOUS")
  scrutins <- read_html(url_prov)
  
  url_dossier_associe_prov<- scrutins %>% html_nodes("a")%>% 
    html_attr('href') 
  
  prov <- str_detect(as.character(url_dossier_associe_prov),"/scrutins/detail.")
  prov_prov <- rep(FALSE,length(prov))
  for (i in 1:length(prov)-1){
    if(prov[i+1]==TRUE){
      prov_prov[i]<-TRUE}
  }
  
  prov_2 <- str_detect(as.character(url_dossier_associe_prov),"/scrutins/detail.")
  
  url_dossier_associe <- url_dossier_associe_prov[prov_prov]
  url_dossier_associe[str_detect(url_dossier_associe, "/scrutins/detail.")] <- ""
  extract_numeric
  scrutin_numero<-str_remove(extract_num(url_dossier_associe_prov[prov_2]),"16")
  
  scrutin_dossier_data <- data.frame(scrutin_numero,url_dossier_associe)
  return(scrutin_dossier_data)
}
#Application de la fonction pour chaque page
dossier_scrutin <- lapply(sequence_offset,function_get_url_dosier_scrutin)
#Création d'un data.frame
dossier_scrutin <- unique(bind_rows(dossier_scrutin))
#On s'assure du type de certaine variable
dossier_scrutin$url_dossier_associe <- as.character(dossier_scrutin$url_dossier_associe)
head(dossier_scrutin)

dossier_scrutin <- left_join(titre_vote,dossier_scrutin, by = "scrutin_numero")

#Fonction permettant de récupérer l'adresse du texte initale de loi à partir de l'adresse du dossier associé
fonction_url_texte_loi <- function(url){
  print(url)
  resume_loi_url <- read_html(url) %>% 
    html_nodes("a")%>% 
    html_attr('href') %>%
    as_tibble() %>%
    mutate_at("value", str_match, pattern="/dyn/16/textes/.*_proposition-loi$|/dyn/16/textes/.*_proposition-resolution$|/dyn/16/textes/.*_projet-loi$") %>%
    na.omit() %>%
    filter(value == max(value)) %>%
    mutate(value = paste0("https://www.assemblee-nationale.fr",value)) %>%
    unique()
  
  resume_loi_url <- resume_loi_url[1,1]
  resume_loi_url <- data.frame(url,resume_loi_url)
  return(resume_loi_url)
}
#Certaine page n'existe pas, cela cause un problème, 
#On s'assure donc que le fait d'avoir d'un probleme n'empeche pas la continuation de l'application de la fonction
fonction_url_texte_loi <- possibly(fonction_url_texte_loi, otherwise = FALSE)
#Application de la fonction pour chaque dossier legislatif
url_texte_loi <- lapply(unique(dossier_scrutin$url_dossier_associe),fonction_url_texte_loi)
#On supprime les fois où la fonction n'a rien récupérer
url_texte_loi <- url_texte_loi[lapply(url_texte_loi, isFALSE) == FALSE]
#On crée un data.frame
url_texte_loi <- bind_rows(na.omit(url_texte_loi))




#On s'assure du type de certaine variable
url_texte_loi$value <- as.character(url_texte_loi$value)


#Fonction permettant de récupérer l'adresse du texte de loi publié au JO à partir de l'adresse du dossier associé
fonction_url_texte_loi_JO <- function(url){
  print(url)
  resume_loi_url <- read_html(url) %>% html_nodes("a")%>% 
    html_attr('href') %>%
    as_tibble() %>%
    mutate_at("value", str_match, pattern="http://www.legifrance.gouv.fr/.*$") %>%
    na.omit() %>%
    unique() 
  
  resume_loi_url <- resume_loi_url[1,1]
  resume_loi_url <- data.frame(url,resume_loi_url)
  return(resume_loi_url)
}
#Certaine page n'existe pas, cela cause un problème, 
#On s'assure donc que le fait d'avoir d'un probleme n'empeche pas la continuation de l'application de la fonction
fonction_url_texte_loi_JO <- possibly(fonction_url_texte_loi_JO, otherwise = FALSE)
#Application de la fonction pour chaque dossier legislatif
url_texte_loi_JO <- lapply(unique(dossier_scrutin$url_dossier_associe),fonction_url_texte_loi_JO)
#On supprime les fois où la fonction n'a rien récupérer
url_texte_loi_JO <- url_texte_loi_JO[lapply(url_texte_loi_JO, isFALSE) == FALSE]
url_texte_loi_JO <- bind_rows(na.omit(url_texte_loi_JO))
#On s'assure du type de certaine variable
url_texte_loi_JO$value <- as.character(url_texte_loi_JO$value)


names(url_texte_loi)[1] <- "url_dossier_associe"
names(url_texte_loi)[2] <- "texte_loi"
names(url_texte_loi_JO)[1] <- "url_dossier_associe"
names(url_texte_loi_JO)[2] <- "texte_loi_JO"

data_loi <- left_join(dossier_scrutin,url_texte_loi,by="url_dossier_associe")
data_loi <- left_join(data_loi,url_texte_loi_JO,by="url_dossier_associe") %>%
  mutate(nom_loi = str_replace_all(
    str_remove(
      str_remove(
        str_remove(
          str_remove(
            str_remove(
              str_remove(url_dossier_associe,"https://www.assemblee-nationale.fr/16/dossiers/")
              ,"https://www.assemblee-nationale.fr/dyn/15/dossiers/")
            ,".asp")
          ,"https://www.assemblee-nationale.fr/dyn/16/dossiers/")
        ,"etape=15-AN1-DEPOT")
      ,"https:www.assemblee-nationale.fr/dyn/15/dossiers/")
    ,"[[:punct:]]", " ")) %>%
  rename(uid_loi  = scrutin_numero) %>%
  select(uid_loi,nom_loi,date_vote,titre,url_dossier_associe,texte_loi,texte_loi_JO) %>%
  mutate(type_texte = case_when(str_detect(titre,"motion")~ "motion",
                                str_detect(titre,"declaration")~ "declaration",
                                str_detect(titre,"amendement")~ "amendement",
                                str_detect(titre,"article")~ "article",
                                str_detect(titre,"ensemble")~ "ensemble"
                                ))
nom_loi <- unique(data.frame(data_loi$nom_loi,data_loi$texte_loi_JO,data_loi$url_dossier_associe)) %>%
  rename(nom_loi=data_loi.nom_loi,url_dossier_associe=data_loi.url_dossier_associe,texte_loi_JO=data_loi.texte_loi_JO) %>%
  mutate(Statut = case_when(!is.na(texte_loi_JO)~1))

names(nom_loi)[1] <- "nom_loi"

setwd(paste0(path,"/data/data_loi/"))

fwrite(nom_loi,paste0("nom_loi","_",Sys.Date(),".csv"),sep=";",col.names = TRUE)

setwd(paste0(path,"/data/data_final/"))
list.files()
vote_final_v2 <- fread("vote_final_v2.csv")
vote_final_v3 <- readRDS(file ="vote_final_v3.rds")

###### Sous réserve d'avoir nom_loi avec statut de la loi si rejete alors 0 si non 1
setwd(paste0(path,"/data/data_loi/"))
nom_loi <- read.csv("nom_loi.csv",sep=";")

data_loi <-unique(data_loi %>%
                    select(uid_loi,nom_loi,date_vote,titre,type_texte))

data_loi$uid_loi <-as.character(data_loi$uid_loi)
data_loi <- left_join(data_loi,nom_loi,by="nom_loi")

fwrite(data_loi,"data_loi.csv",sep=";",col.names = TRUE)

data_loi$uid_loi <-as.numeric(data_loi$uid_loi )
data_democratie <- left_join(vote_final_v2 ,data_loi,by="uid_loi") %>%
  select(-date_vote.y)%>%
  rename(date_vote=date_vote.x)
data_democratie_v2 <- left_join(vote_final_v3 ,data_loi,by="uid_loi") %>%
  select(-date_vote.y)%>%
  rename(date_vote=date_vote.x)

setwd(paste0(path,"/data/data_democratie/"))
fwrite(data_democratie,"data_democratie.csv",sep=";",col.names = TRUE)
saveRDS(data_democratie_v2, file = "data_democratie_v2.rds")

