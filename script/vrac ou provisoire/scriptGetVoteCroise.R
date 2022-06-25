######################################################
# Analyse des données de vote de l'assemblée française
######################################################

#CREATION D'UNE BASE SERVANT A LA CRETAION D'UN GRAPHE INTER-VOTE DATA 

library(tidyverse) 
library(data.table)

#CHARGEMENT DES DONNEES VOTE FINAL

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/final")
list.files()

vote_final <- read.csv("vote_final.csv" )[,-1]

#Pour avoir une idée
#Nombre de loi
length(unique(vote_final$uid_loi))
#Nombre de votant
length(unique(vote_final$depute_code))

#Le probleme est que l'operation menee nous mene a un data.frame dimension :
# ((nombre de vote moyen * nombre de loi)**2 - nombre_votant)/2 
(length(unique(vote_final$uid_loi))*length(unique(vote_final$depute_code)))**2
#Potentiel nombre de ligne soit 4 000 000 000 000 si tous les députés votait à chaque loi proposé

'Nombre de votant' <- table(vote_final$uid_loi)
'Nombre de lois votées' <- table(vote_final$depute_code)

colors()
#Une loi peut recevoir qu'au maximum 575 député
hist(`Nombre de votant`, 
     main = "Nombre de loi par nombre de votant", 
     breaks = seq(1,length(unique(vote_final$depute_code)),5),
     xlim=c(0,575), 
     ylab = "Nombre de loi",col = "ivory")

hist(`Nombre de lois votées`, 
     main = "Nombre de député ayant voté loi voté par député par nombre de votant",
     breaks = seq(1,length(unique(vote_final$uid_loi)),100),
     xlim=c(0,1500),
     ylab = "Nombre de député",col = "rosybrown")

choix_echantillon_base_nombre_votant <-0.88
echantillon_vote <- as.character(vote_final$uid_loi)[which(table(vote_final$uid_loi)>choix_echantillon_base_nombre_votant*length(unique(vote_final$depute_code)))]
#En prenant les lois où seuls 88/100 des députés ont voté on arrive à un echantillon de loi de 40
#néanmoins le chiffre sera important

vote_final<- vote_final %>% 
  filter(uid_loi %in% echantillon_vote)%>% 
  mutate(vote_code = as.factor(vote_code),
         date_vote = as.Date(date_vote),
         depute_code = as.factor(depute_code),
         nom = as.factor(nom),
         prenom = as.factor(prenom),
         experienceDepute = as.numeric(str_extract(experienceDepute,"\\b([0-9]|[1-9][0-9]|100)\\b")),
         job = as.factor(job),
         groupeAbrev = as.factor(groupeAbrev),
         naissance = as.Date(naissance))



setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/vote_croise")

#Creation data.frame vote commun 
assign(paste0("vote_croise_","",as.character(choix_echantillon_base_nombre_votant,sep="")),
             inner_join(vote_final, vote_final, by=c("uid_loi"))) 

fwrite(get(paste0("vote_croise_","",as.character(choix_echantillon_base_nombre_votant,sep=""))),
       paste0("vote_croise_","",as.character(choix_echantillon_base_nombre_votant),".csv"))


