######################################################
# Analyse des données de vote de l'assemblée française
######################################################

#INTEGRATION DES DONNEES VOTE(Json) & Deputés(Csv)
library(RPostgreSQL)

db <- 'bdd_democratie'  #provide the name of your db
host_db <- 'BDD_DEMOCRATIE' #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
db_port <- '5432'  # or any other port specified by the DBA
db_user <- "postgres"  
db_password <- 'postgres'
option <- "-c search_path=assemblee_elective"

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password,options=option) 

#LIBRAIRIE UTILISEE
library(jsonlite)  # Pour ouvrir fichier json
library(stringr)   # Pour manipuler les caracteres
library(tidyverse) # Pour avoir tout l'univers tidy (coutau-suisse de R)
library(dplyr)
library(data.table)


library(tidyverse)
library(maps)
library(geojsonR)
library(geojsonsf)
library(rio)
library(sf)
library(sp)

#Petit rappel des informations techniques, cela ne mange pas de pain

sessionInfo()
packageStatus()
Sys.getenv()
getwd()

path <- getwd()

#CHARGEMENT DES DONNEES VOTE
# Endroit ou vous mettez les fichiers json en telechargeant sous le lien 
setwd(paste0(path,"/data/data_vote"))

#Endroit ou telecharger les données
url <- "http://data.assemblee-nationale.fr/static/openData/repository/16/loi/scrutins/Scrutins.json.zip"
download.file(url, destfile = basename(url))

#Fichier zip donc dezippage
unzip("Scrutins.json.zip")

#L'archive ne nous sert plus à grand chose
file.remove("Scrutins.json.zip")

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

dbWriteTable(con,'vote',vote, row.names=FALSE)

#CHARGEMENT DES DONNEES DEPUTE
# Endroit ou vous mettez le fichier csv en telechargeant sous le lien 
setwd(paste0(path,"/data/data_depute/"))

#Endroit ou telecharger les données
url <- "https://www.data.gouv.fr/fr/datasets/r/092bd7bb-1543-405b-b53c-932ebb49bb8e"
download.file(url, destfile = basename(url))

#On lit le fichier
depute_plus_file <- list.files()[1]
depute_plus <- read.csv(depute_plus_file,encoding = "UTF-8")

#On change le nom de la colonne sur laquelle on va joindre
names(depute_plus)[1] <- "depute_code"

dbWriteTable(con,'depute',depute_plus, row.names=FALSE)