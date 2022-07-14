######################################################
# Analyse des données de vote de l'assemblée française
######################################################

library(tidyverse) 
library(data.table)
library(igraph)


path <- getwd()

setwd(paste0(path,"/data/data_democratie"))
list.files()

vote_final <- readRDS(file="data_democratie_v3.rds")  %>%
  filter(str_trim(nom_loi)!="") %>%
  select(uid_loi,vote_code,depute_code,nom_loi,nombre_vote_relatif_dossier_leg)
vote_final$vote_code <- as.numeric(vote_final$vote_code)
  
loi_seq <- unique(vote_final$nom_loi)

fonction_vote_croise <- function(loi_uid){
  print(loi_uid)
  vote_final_select <- vote_final %>% 
    filter(nom_loi == loi_uid)  
    vote_final_select<-  vote_final_select[!is.na(vote_final_select$vote_code),]
    
  vote_final_select <- inner_join(vote_final_select, vote_final_select, by=c("uid_loi"))%>%
    mutate(vote_commun = case_when(vote_code.x==vote_code.y~1,
                                   vote_code.x!=vote_code.y~0),
           loi_commun = 1) %>%
    rename(from =depute_code.x,to=depute_code.y,
           nombre_loi_vote_relatif_from=nombre_vote_relatif_dossier_leg.x,
           nombre_loi_vote_relatif_to=nombre_vote_relatif_dossier_leg.y)%>%
    select(from,to,nombre_loi_vote_relatif_from,nombre_loi_vote_relatif_to,vote_commun,loi_commun)%>%
    group_by(from,to,nombre_loi_vote_relatif_from,nombre_loi_vote_relatif_to)%>%
    summarise(vote_commun = sum(vote_commun),
              loi_commun=sum(loi_commun),
              similarite=sum(vote_commun)/sum(loi_commun),
              connectivite=sum(loi_commun)/(min(nombre_loi_vote_relatif_to,nombre_loi_vote_relatif_from)))  
  
  g <- as_data_frame(simplify(graph_from_data_frame(vote_final_select, 
                                                    directed=FALSE)))
  vote_final_select <- unique(inner_join(g,vote_final_select,by=c("to","from")))
  
  assign(paste0("vote_croise_","",loi_uid,sep=""),
         vote_final_select)
  
  
  fwrite(get(paste0("vote_croise_","",loi_uid,sep="")),
         paste0("vote_croise_","",loi_uid,".csv"))
  
  rm(list=c(paste0("vote_croise_","",loi_uid,sep=""),"vote_final_select"))
  
}


setwd(paste0(path,"/data/data_vote_croise/"))
lapply(loi_seq,fonction_vote_croise)

