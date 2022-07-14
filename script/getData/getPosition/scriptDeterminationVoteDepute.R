
library(tidyverse) 
library(data.table)
library(igraph)
library(lsa)

path <- getwd()

setwd(paste0(path,"/data/data_democratie/"))
list.files()

vote_final <- fread("data_democratie.csv")  %>%
  filter(str_trim(nom_loi)!="") 

vote_final$vote_code <- as.numeric(vote_final$vote_code)

nom_loi_seq <-unique(vote_final$nom_loi)

#################################
data_result_2 <- data.frame()
for(j in 1:length(nom_loi_seq)){
  print(nom_loi_seq[j])
  nom_loi_choisi <- nom_loi_seq[j]
  statut_loi <- unique(vote_final[vote_final$nom_loi==nom_loi_choisi,]$Statut)
  
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

setwd(paste0(path,"/data/data_position/"))
fwrite(data_result_2,"data_position.csv",sep=";",col.names = TRUE)

setwd(paste0(path,"/data/data_democratie/"))

vote_final <- readRDS(file="data_democratie_v2.rds")
data_democratie <- vote_final %>%
  left_join(data_result_2,by=c("nom_loi","depute_code"))

saveRDS(data_democratie,file="data_democratie_v3.rds")
