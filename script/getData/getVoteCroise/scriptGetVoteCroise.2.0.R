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
option <- "-c search_path=vote_croise"

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password,options=option) 

#LIBRAIRIE UTILISEE
library(tidyverse) 
library(data.table)
library(igraph)


path <- getwd()

setwd(paste0(path,"/data/data_democratie"))
list.files()

vote_final <- readRDS(file="data_democratie_v3.RDS")  %>%
  select(uid_loi,vote_code,depute_code,nom_loi,nombre_vote_relatif_dossier_leg) %>%
  filter(str_trim(nom_loi)!="") 

vote_final$vote_code <- as.numeric(vote_final$vote_code)
  
loi_seq <- unique(vote_final$nom_loi)

fonction_vote_croise <- function(loi_uid){
  print(loi_uid)
  vote_final_select <- vote_final %>% 
    filter(nom_loi == loi_uid)  
  vote_final_select<-  vote_final_select[!is.na(vote_final_select$vote_code),]
    
  vote_final_select<- inner_join(vote_final_select, vote_final_select, by=c("uid_loi"),multiple = "all")%>%
    mutate(vote_commun = case_when(vote_code.x==vote_code.y~1,
                                   vote_code.x!=vote_code.y~0),
           loi_commun = 1) %>%
    rename(from =depute_code.x,to=depute_code.y,
           nombre_loi_vote_relatif_from=nombre_vote_relatif_dossier_leg.x,
           nombre_loi_vote_relatif_to=nombre_vote_relatif_dossier_leg.y,
           nom_loi = nom_loi.x)%>%
    select(nom_loi,from,to,nombre_loi_vote_relatif_from,nombre_loi_vote_relatif_to,vote_commun,loi_commun)%>%
    group_by(from,to,nom_loi,nombre_loi_vote_relatif_from,nombre_loi_vote_relatif_to)%>%
    summarise(vote_commun = sum(vote_commun),
              loi_commun=sum(loi_commun),
              similarite=sum(vote_commun)/sum(loi_commun),
              connectivite=sum(loi_commun)/(min(nombre_loi_vote_relatif_to,nombre_loi_vote_relatif_from)))  
  
  g <- as_data_frame(simplify(graph_from_data_frame(vote_final_select, 
                                                    directed=FALSE)))
  vote_final_select <- unique(inner_join(g,vote_final_select,by=c("to","from")))
  
  assign(paste0("vote_croise_","",loi_uid,sep=""),
         vote_final_select)
  
  
  dbWriteTable(con,paste0("vote_croise_","",loi_uid),get(paste0("vote_croise_","",loi_uid,sep="")), row.names=FALSE,overwrite =TRUE)
  
  rm(list=c(paste0("vote_croise_","",loi_uid,sep=""),"vote_final_select"))
  
}

lapply(loi_seq,fonction_vote_croise)

