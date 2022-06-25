######################################################
# Analyse des données de vote de l'assemblée française
######################################################

#INTEGRATION DES DONNEES VOTE(Json) & Deputés(Csv)

#LIBRAIRIE UTILISEE
library(jsonlite)  # Pour ouvrir fichier json
library(stringr)   # Pour manipuler les caracteres
library(tidyverse) # Pour avoir tout l'univers tidy (coutau-suisse de R)
library(dplyr)
library(data.table)
#Petit rappel des informations techniques, cela ne mange pas de pain

sessionInfo()
packageStatus()
Sys.getenv()
getwd()

path <- "C:/Users/Quentin GOLLENTZ/Documents/PROJET PERSO/bureaudevote/"

#CHARGEMENT DES DONNEES VOTE
# Endroit ou vous mettez les fichiers json en telechargeant sous le lien 
setwd(paste0(path,"/data/data_vote"))

#Endroit ou telecharger les données
url <- "http://data.assemblee-nationale.fr/static/openData/repository/15/loi/scrutins/Scrutins_XV.json.zip"
download.file(url, destfile = basename(url))

#Fichier zip donc dezippage
unzip("Scrutins_XV.json.zip")

#L'archive ne nous sert plus à grand chose
file.remove("Scrutins_XV.json.zip")

#On se place là où les données sont (càd un niveau inférieur)
setwd(paste0(path,"/data/data_vote/json"))

#On recupere la liste des fichiers
liste_vote <- list.files()

#Fonction visant pour chaque fichier de la liste a recuperer les informations
read_data <- function(liste){
  #On lit le fichier de la liste
  vote <- fromJSON(liste, flatten=TRUE)
  
  #On récupère les données de vote CONTRE et on crée un data set
  vote_contre_intermediaire <- bind_rows(vote[["scrutin"]][["ventilationVotes"]][["organe"]][["groupes"]][["groupe"]][["vote.decompteNominatif.contres.votant"]])
  vote_contre_code<- rep(0.0,nrow(vote_contre_intermediaire))
  vote_contre <-if(length(vote_contre_intermediaire)==0){
    tibble(vote_code = integer(),depute_code=character())} else {
      tibble(vote_code= vote_contre_code,depute_code =vote_contre_intermediaire$acteurRef)
    }

  #On récupère les données de vote POUR et on crée un data set
  vote_pour_intermediaire <- bind_rows(vote[["scrutin"]][["ventilationVotes"]][["organe"]][["groupes"]][["groupe"]][["vote.decompteNominatif.pours.votant"]])
  vote_pour_code <- rep(1.0,nrow(vote_pour_intermediaire))
  vote_pour <-if(length(vote_pour_intermediaire)==0){
    tibble(vote_code = integer(),depute_code=character())} else {
      tibble(vote_code= vote_pour_code,depute_code =vote_pour_intermediaire$acteurRef)
    }
  
  #On récupère les données de vote ABSTENTION et on crée un data set
  vote_abstention_intermediaire <- bind_rows(vote[["scrutin"]][["ventilationVotes"]][["organe"]][["groupes"]][["groupe"]][["vote.decompteNominatif.abstentions.votant"]])
  vote_abstention_code <- rep(0.5,nrow(vote_abstention_intermediaire))
  vote_abstention <-if(length(vote_abstention_intermediaire)==0){
    tibble(vote_code = integer(),depute_code=character())} else {
    tibble(vote_code= vote_abstention_code,depute_code =vote_abstention_intermediaire$acteurRef)
  }

  #On lie les trois data.sets
  vote_1 <- bind_rows(vote_contre,vote_pour,vote_abstention)
  
  #On replique pour chaque vote les informations de la loi pour laquelle il vote
  uid_loi <- rep(vote[["scrutin"]][["numero"]],nrow(vote_1))
  date_vote <- rep(vote[["scrutin"]][["dateScrutin"]],nrow(vote_1))
  nb_votant <- rep(vote[["scrutin"]][["syntheseVote"]][["nombreVotants"]],nrow(vote_1))
  nb_pour <- rep(vote[["scrutin"]][["syntheseVote"]][["decompte"]][["pour"]],nrow(vote_1))
  nb_contre <- rep(vote[["scrutin"]][["syntheseVote"]][["decompte"]][["contre"]],nrow(vote_1))
  nb_abstentions <-  rep(vote[["scrutin"]][["syntheseVote"]][["decompte"]][["abstentions"]],nrow(vote_1))                                
  nb_absence<-  rep(577-as.integer(vote[["scrutin"]][["syntheseVote"]][["nombreVotants"]]),nrow(vote_1))                                
  
  
  data <- tibble(uid_loi,nb_votant,nb_pour,nb_contre,nb_abstentions,nb_absence,date_vote,vote_1)
  return(data)
}
#Application de la fonction pour chaque fichier de la liste
vote <- lapply(liste_vote, read_data)

#Creation d'un data.frame
vote <- bind_rows(vote)
#On verifie la coherence 
head(vote,1000)

#CHARGEMENT DES DONNEES DEPUTE
# Endroit ou vous mettez le fichier csv en telechargeant sous le lien 
setwd(paste0(path,"/data/data_depute/"))

#Endroit ou telecharger les données
url <- "https://www.data.gouv.fr/fr/datasets/r/092bd7bb-1543-405b-b53c-932ebb49bb8e"
download.file(url, destfile = basename(url))

#On lit le fichier
depute_plus_file <- list.files()[1]
depute_plus <- read.csv(depute_plus_file)

#On change le nom de la colonne sur laquelle on va joindre
names(depute_plus)[1] <- "depute_code"

#CROISEMENT DES DONNEES VOTE/DEPUTE++ par depute_code pour avoir des données exhaustives
vote_final <- left_join(vote, depute_plus, by = "depute_code")  %>% 
  select(uid_loi,date_vote,nb_votant,nb_pour,nb_contre,nb_abstentions,depute_code,vote_code,
         nom,prenom,naissance,age,departementNom,departementCode,circo,
         experienceDepute,nombreMandats,job,groupeAbrev,mail) %>%
  mutate(vote_code = as.character(vote_code),
         nb_votant = as.integer(nb_votant),
         nb_pour=as.integer(nb_pour),
         nb_contre=as.integer(nb_contre),
         nb_abstentions=as.integer(nb_abstentions),
         nom = as.factor(nom),
         prenom = as.factor(prenom),
         experienceDepute = as.numeric(str_extract(experienceDepute,"\\b([0-9]|[1-9][0-9]|100)\\b")),
         job = as.factor(job),
         groupeAbrev = as.factor(groupeAbrev),
         naissance = as.Date(naissance),
         age = as.integer(age))

#On garde en mémoire le fichier

rm(list=c("depute_plus","vote","depute_plus_file","liste_vote","url","read_data"))
setwd(paste0(path,"/data/data_final"))
fwrite(vote_final,"vote_final.csv",sep=";",col.names =TRUE,)

#######

dep <- unique(vote_final$depute_code)
loi <- unique(vote_final$uid_loi)

vote_final_v2 <- expand.grid(dep,loi)
names(vote_final_v2) <- c("depute_code","uid_loi")
vote_final_v2 <- left_join(vote_final_v2,vote_final,by=c("depute_code","uid_loi"))
vote_final_v2 <-as.data.table(vote_final_v2)


correction_date_vote <- unique(vote_final_v2[, list(selec = !is.na(date_vote),date_vote = date_vote),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_votant <- unique(vote_final_v2[, list(selec = !is.na(nb_votant),nb_votant = nb_votant),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_pour <- unique(vote_final_v2[, list(selec = !is.na(nb_pour),nb_pour = nb_pour),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_contre <- unique(vote_final_v2[, list(selec = !is.na(nb_contre),nb_contre = nb_contre),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_abstentions <- unique(vote_final_v2[, list(selec = !is.na(nb_abstentions),nb_abstentions = nb_abstentions),by="uid_loi"][selec == TRUE][,c(1,3)])


correction_nom <- unique(vote_final_v2[, list(selec = !is.na(nom),nom = nom),by="depute_code"][selec == TRUE][,c(1,3)])
correction_prenom <- unique(vote_final_v2[, list(selec = !is.na(prenom),prenom = prenom),by="depute_code"][selec == TRUE][,c(1,3)])
correction_experienceDepute<- unique(vote_final_v2[, list(selec = !is.na(experienceDepute),experienceDepute = experienceDepute),by="depute_code"][selec == TRUE][,c(1,3)])
correction_job <- unique(vote_final_v2[, list(selec = !is.na(job),job = job),by="depute_code"][selec == TRUE][,c(1,3)])
correction_groupeAbrev <- unique(vote_final_v2[, list(selec = !is.na(groupeAbrev),groupeAbrev = groupeAbrev),by="depute_code"][selec == TRUE][,c(1,3)])
correction_naissance <- unique(vote_final_v2[, list(selec = !is.na(naissance),naissance = naissance),by="depute_code"][selec == TRUE][,c(1,3)])
correction_age <- unique(vote_final_v2[, list(selec = !is.na(age),age = age),by="depute_code"][selec == TRUE][,c(1,3)])
correction_departementNom <- unique(vote_final_v2[, list(selec = !is.na(departementNom),departementNom = departementNom),by="depute_code"][selec == TRUE][,c(1,3)])
correction_circo <- unique(vote_final_v2[, list(selec = !is.na(circo),circo = circo),by="depute_code"][selec == TRUE][,c(1,3)])
correction_mail <- unique(vote_final_v2[, list(selec = !is.na(mail),mail = mail),by="depute_code"][selec == TRUE][,c(1,3)])
correction_nombreMandats <- unique(vote_final_v2[, list(selec = !is.na(nombreMandats),nombreMandats = nombreMandats),by="depute_code"][selec == TRUE][,c(1,3)])
correction_departementCode <- unique(vote_final_v2[, list(selec = !is.na(departementCode),departementCode = departementCode),by="depute_code"][selec == TRUE][,c(1,3)])


vote_final_v2 <- vote_final_v2 %>%
  select(uid_loi,depute_code,vote_code) %>%
  inner_join(correction_date_vote,by="uid_loi")  %>%
  inner_join(correction_nb_votant,by="uid_loi")  %>%
  inner_join(correction_nb_pour,by="uid_loi")  %>%
  inner_join(correction_nb_contre,by="uid_loi")  %>%
  inner_join(correction_nb_abstentions,by="uid_loi")  %>%
  inner_join(correction_nom,by="depute_code")  %>%
  inner_join(correction_prenom,by="depute_code")  %>%
  inner_join(correction_naissance,by="depute_code")  %>%
  inner_join(correction_age,by="depute_code")  %>%
  inner_join(correction_experienceDepute,by="depute_code")  %>%
  inner_join(correction_nombreMandats,by="depute_code")  %>%
  inner_join(correction_job,by="depute_code")  %>%
  inner_join(correction_groupeAbrev,by="depute_code")  %>%
  inner_join(correction_departementNom,by="depute_code")  %>%
  inner_join(correction_departementCode,by="depute_code")  %>%
  inner_join(correction_circo,by="depute_code")  %>%
  inner_join(correction_mail,by="depute_code")


fwrite(vote_final_v2,"vote_final_v2.csv",sep=";",col.names =TRUE)
vote_final_v2 <- fread(paste0(path,"/data/data_final/vote_final_v2.csv"))
############# TROP COUTEUX


setwd(paste0(path,"/data/data_circo"))
download.file("https://www.data.gouv.fr/fr/datasets/r/efa8c2e6-b8f7-4594-ad01-10b46b06b56a", 
              destfile = basename("https://www.data.gouv.fr/fr/datasets/r/efa8c2e6-b8f7-4594-ad01-10b46b06b56a"))


library(maps)
library(geojsonR)
library(geojsonsf)
library(rio)
library(sf)
library(sp)

file_js = geojson_sf(list.files()[1]) %>%
  filter(str_detect(code_dpt,"Z")==FALSE)%>%
  select(num_circ,code_dpt,geometry)%>%
  rename(circo = num_circ,departementCode =code_dpt )%>%
  mutate(circo = as.numeric(circo))

vote_final_v2$circo <- as.numeric(vote_final_v2$circo )
vote_final_v3 <- inner_join(vote_final_v2,file_js,by=c("departementCode","circo"))

test <- st_as_sf(vote_final_v3 %>%
  filter(uid_loi=='1544') %>%
  mutate(geometry = st_sfc(geometry))%>%
  select(vote_code,departementCode,geometry))

plot(test["vote_code"])

setwd(paste0(path,"/data/data_final"))
fwrite(vote_final_v3,"vote_final_v3.csv",sep=";",col.names =TRUE)