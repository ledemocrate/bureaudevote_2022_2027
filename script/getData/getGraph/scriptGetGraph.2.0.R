######################################################
# Analyse des données de vote de l'assemblée française
######################################################

library(tidyverse) 
library(data.table)

path <- "C:/Users/Quentin GOLLENTZ/Documents/PROJET PERSO/bureaudevote/"

setwd(paste0(path,"/data/data_vote_croise"))

liste_vote_croise <- list.files()
liste_vote_croise

fonction_create_graph_data <- function(vote_croise){
  vote_final_read <- fread(vote_croise)
  vote_final_read <- vote_final_read %>%
    mutate(nom_loi)
  return(list(vote_final_read))
}

graph <- lapply(liste_vote_croise,fonction_create_graph_data)
graph <- bind_rows(graph)

head(graph)

edge <- graph %>%
  group_by(from,to)%>%
  summarise(nombre_loi_commune = sum(loi_commun),nombre_vote_commun = sum(vote_commun))%>%
  ungroup()

rm(list=c("graph"))

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_final")
list.files()

vote_final <- read.csv("vote_final.csv" )[,-1]

node <- vote_final %>%
  group_by(nom_prenom,experienceDepute,job,groupeAbrev,naissance) %>%
  count()%>%
  ungroup() %>%
  rename(experience=experienceDepute,groupe_code=groupeAbrev)

rm(list=c("vote_final"))

edge  <- merge(edge,node,by.x="from",by.y="nom_prenom")  
edge  <- merge(edge,node,by.x="to",by.y="nom_prenom") 

K <- 8
K2 <- 2
edge <- edge %>%
  select(from,to,nombre_loi_commune,nombre_vote_commun,n.x,n.y) %>% 
  mutate(indice_taux_connectivité =  nombre_loi_commune/(n.x+n.y- nombre_loi_commune)) %>% 
  mutate(indice_taux_connectivité_transformé =1/(1+exp(-K*(indice_taux_connectivité-mean(indice_taux_connectivité))))**K2) %>%
  mutate(indice_intensité_connectivité = nombre_vote_commun/nombre_loi_commune) %>%
  mutate(indice_connectivité = indice_taux_connectivité*indice_intensité_connectivité) %>%
  mutate(indice_connectivité_transformé = indice_taux_connectivité_transformé*indice_intensité_connectivité) %>%
  rename(nombre_loi_vote.from=n.x,
         nombre_loi_vote.to=n.y)
  

  
node <- node %>%
  select(-n)

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_graph")

fwrite(edge,"edge.csv")
fwrite(node,"node.csv")
