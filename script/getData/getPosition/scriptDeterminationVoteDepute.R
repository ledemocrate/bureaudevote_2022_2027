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
library(tidyverse) 
library(data.table)
library(igraph)
library(lsa)

path <- getwd()

setwd(paste0(path,"/data/data_democratie/"))
list.files()

vote_final <- read_rds(list.files()[1])  %>%
  filter(str_trim(nom_loi)!="") 

vote_final$vote_code <- as.numeric(vote_final$vote_code)

nom_loi_seq <-unique(vote_final$nom_loi)

#################################
data_result_2 <- data.frame()
for(j in 1:length(nom_loi_seq)){
  print(nom_loi_seq[j])
  nom_loi_choisi <- nom_loi_seq[j]

  vote_final_ech <- vote_final %>%
    filter(nom_loi==nom_loi_choisi) %>%
    select(depute_code,vote_code,uid_loi) %>%
    pivot_wider(names_from = uid_loi,values_from = vote_code,values_fn =mean)
  
  vote_final_ech_mean <- as.data.frame(vote_final_ech[,-1]) %>%
    mutate(across(everything(), as.numeric))
  
  vote_final_ech_mean <-  lapply(vote_final_ech_mean, mean, na.rm = T)     
  vote_final_ech_mean <- round(bind_rows(vote_final_ech_mean))
  
  vote_final_ech_depute <- as.data.frame(vote_final_ech[,1])
  
  for(i in 1:nrow(vote_final_ech)) {
    depute_choisi <- vote_final_ech_depute[i,1]
    position2 <- mean(t(abs(vote_final_ech[i,-1]-vote_final_ech_mean)),na.rm = TRUE)
    intensite2 <- sum(!is.na(vote_final_ech[i,-1]))/dim(vote_final_ech_mean)[2]
    nombre_loi_relatif <-dim(vote_final_ech_mean)[2]
    nombre_loi_vote_relatif <- sum(!is.na(vote_final_ech[i,-1]))
    
    position3 <- 1-mean(t(abs(vote_final_ech[i,-1]-vote_final_ech_mean)),na.rm = TRUE)
  
    resultat_vecteur <- data.frame(nom_loi_choisi,depute_choisi,position3,intensite2,nombre_loi_vote_relatif,nombre_loi_relatif)
    colnames(resultat_vecteur) <-c("nom_loi","depute_code","position","intensite","nombre_vote_relatif_dossier_leg","nombre_texte_relatif_dossier_leg")
    data_result_2 <- rbind(data_result_2,resultat_vecteur)}
}


dbWriteTable(con,'data_position',data_result_2, row.names=FALSE)
