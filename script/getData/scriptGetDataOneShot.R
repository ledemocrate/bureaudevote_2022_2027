###########################################
# Création d'un dataset data_democratie_v3
###########################################

# Créateur : QUENTIN GOLLENTZ
# Date de modification : 15/07/2022

######################
### LIBRAIRIE UTILISEE
######################

# Ensemble de librairie pour la manipulation des données
library(jsonlite)  # Pour ouvrir fichier json
library(stringr)   # Pour manipuler les caracteres
library(tidyverse) # Pour avoir tout l'univers tidy (coutau-suisse de R)
library(dplyr) # Pour manipuler les données
library(data.table) # Pour manipuler les données

# Ensemble de librairie pour les données de type géographique
library(maps)
library(geojsonR)
library(geojsonsf)
library(rio)
library(sf)
library(sp)

# Ensemble de librairie pour lee web scrapping
library(rvest) #Pour le web scrapping
library(purrr) #Pour certaine fonction

# Ensemble de librairie pour les données de type graphe
library(igraph)
library(lsa)

# Fonction éventuellement utilisé pour exclure des valeurs
`%nin%` = Negate(`%in%`)

#Petit rappel des informations techniques, cela ne mange pas de pain
sessionInfo()
packageStatus()
Sys.getenv()
getwd()

# On initialise un répertoire racine
path <-  getwd()

###############################
### RECUPERATION DES DATA VOTES
###############################

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

#Fonction visant pour chaque fichier de la liste a recuperer les informations de vote
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

rm(list=c("liste_vote","url","read_data"))

#################################
### RECUPERATION DES DATA DEPUTES
#################################

#Endroit ou vous mettez le fichier csv en telechargeant sous le lien 
setwd(paste0(path,"/data/data_depute/"))
list.files()

#Endroit ou telecharger les données
url <- "https://www.data.gouv.fr/fr/datasets/r/092bd7bb-1543-405b-b53c-932ebb49bb8e"
download.file(url, destfile = basename(url))

#On fait des mise à jour sur ce fichier car il est dynamique et forunis une image dans un instant T
depute_plus_origine <- read_csv("depute.csv")
depute_plus_candidat <- read_csv(list.files()[1])
depute_plus <- rbind(depute_plus_candidat[depute_plus_candidat$id %nin% depute_plus_origine$id,],depute_plus_origine)
fwrite(depute_plus,"depute.csv")

#On change le nom de la colonne sur laquelle on va joindre
names(depute_plus)[1] <- "depute_code"
depute_plus$depute_code<-as.character(depute_plus$depute_code) 

#Enrichissement des données de votes avec les données 
vote <- inner_join(vote, depute_plus, by = "depute_code")  %>% 
  select(uid_loi,date_vote,nb_votant,nb_pour,nb_contre,nb_abstentions,nb_absence,depute_code,vote_code,
         nom,prenom,naissance,age,departementNom,departementCode,circo,
         experienceDepute,nombreMandats,job,groupeAbrev,mail) %>%
  #Modification du type des champs
  mutate(vote_code = as.character(vote_code),
         nb_votant = as.integer(nb_votant),
         nb_pour=as.integer(nb_pour),
         nb_contre=as.integer(nb_contre),
         nb_abstentions=as.integer(nb_abstentions),
         nb_absence=as.integer(nb_absence),
         nom = as.factor(nom),
         prenom = as.factor(prenom),
         experienceDepute = as.numeric(str_extract(experienceDepute,"\\b([0-9]|[1-9][0-9]|100)\\b")),
         job = as.factor(job),
         groupeAbrev = as.factor(groupeAbrev),
         naissance = as.Date(naissance),
         age = as.integer(age))

rm(list=c("depute_plus_origine","depute_plus_candidat","depute_plus","url"))

##############################
### CREATION DES DATA ABSENCES
##############################

dep <- unique(vote$depute_code)
loi <- unique(vote$uid_loi)

vote_final_v2 <- expand.grid(dep,loi)
names(vote_final_v2) <- c("depute_code","uid_loi")
vote_final_v2 <- left_join(vote_final_v2,vote,by=c("depute_code","uid_loi"))
vote_final_v2 <-as.data.table(vote_final_v2)


correction_date_vote <- unique(vote_final_v2[, list(selec = !is.na(date_vote),date_vote = date_vote),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_votant <- unique(vote_final_v2[, list(selec = !is.na(nb_votant),nb_votant = nb_votant),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_pour <- unique(vote_final_v2[, list(selec = !is.na(nb_pour),nb_pour = nb_pour),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_contre <- unique(vote_final_v2[, list(selec = !is.na(nb_contre),nb_contre = nb_contre),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_abstentions <- unique(vote_final_v2[, list(selec = !is.na(nb_abstentions),nb_abstentions = nb_abstentions),by="uid_loi"][selec == TRUE][,c(1,3)])
correction_nb_absence <- unique(vote_final_v2[, list(selec = !is.na(nb_absence),nb_absence = nb_absence),by="uid_loi"][selec == TRUE][,c(1,3)])


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
  left_join(correction_date_vote,by="uid_loi")  %>%
  left_join(correction_nb_votant,by="uid_loi")  %>%
  left_join(correction_nb_pour,by="uid_loi")  %>%
  left_join(correction_nb_contre,by="uid_loi")  %>%
  left_join(correction_nb_abstentions,by="uid_loi")  %>%
  left_join(correction_nb_absence,by="uid_loi")  %>%
  left_join(correction_nom,by="depute_code")  %>%
  left_join(correction_prenom,by="depute_code")  %>%
  left_join(correction_naissance,by="depute_code")  %>%
  left_join(correction_age,by="depute_code")  %>%
  left_join(correction_experienceDepute,by="depute_code")  %>%
  left_join(correction_nombreMandats,by="depute_code")  %>%
  left_join(correction_job,by="depute_code")  %>%
  left_join(correction_groupeAbrev,by="depute_code")  %>%
  left_join(correction_departementNom,by="depute_code")  %>%
  left_join(correction_departementCode,by="depute_code")  %>%
  left_join(correction_circo,by="depute_code")  %>%
  left_join(correction_mail,by="depute_code")

rm(list=c("correction_date_vote","correction_nb_votant","correction_nb_pour","correction_nb_contre","correction_nb_abstentions","correction_nom",
          "correction_prenom","correction_experienceDepute","correction_job","correction_groupeAbrev","correction_age","correction_naissance",
          "correction_departementNom","correction_circo","correction_mail","correction_nombreMandats","correction_departementCode","vote","loi","dep","correction_nb_absence"))

############################################
### RECUPERATION DES DATA DE CIRCONSCRIPTION
############################################

# Ajout information circonscription
setwd(paste0(path,"/data/data_circo"))
download.file("https://www.data.gouv.fr/fr/datasets/r/efa8c2e6-b8f7-4594-ad01-10b46b06b56a", 
              destfile = basename("https://www.data.gouv.fr/fr/datasets/r/efa8c2e6-b8f7-4594-ad01-10b46b06b56a"))


#Création d'un fichier avec donnée de type POLYGON
file_js = geojson_sf(list.files()[1]) %>%
  select(num_circ,code_dpt,geometry)%>%
  rename(circo = num_circ,departementCode =code_dpt )%>%
  mutate(circo = as.numeric(circo)) %>%
  mutate(departementCode= case_when(departementCode=="ZA"~"971",
                   departementCode=="ZM"~"976",
                   departementCode=="ZB"~"972",
                   departementCode=="ZD"~"974",
                   departementCode=="ZN"~"988",
                   departementCode=="ZS"~"975",
                   departementCode=="ZX"~"977",
                   departementCode=="ZP"~"987",
                   departementCode=="ZW"~"999",
                   departementCode=="2B"~"2b",
                   departementCode=="2A"~"2a",
                   departementCode=="ZC"~"973",
                   TRUE ~ departementCode))%>%
  mutate(departementCode = as.character(departementCode))

vote_final_v2$circo <- as.numeric(vote_final_v2$circo )
vote_final_v2$departementCode <- as.character(vote_final_v2$departementCode )

vote_final_v2 <- left_join(vote_final_v2,file_js,by=c("departementCode","circo"))

rm(list=c("file_js"))

################################
### RECUPERATION DES DATA DE LOI
################################

extract_num <- function(x) {as.numeric(gsub("[^0-9.-]+", "", as.character(x)))} 
#Permet d'éviter le warning deprecated lorsque utilisation de extract_numeric

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

# https://www2.assemblee-nationale.fr/scrutins/liste/(offset)/{sequence_offset}/(legislature)/16/(type)/SOR/(idDossier)/TOUS
# Vérifié manuellement que la sequence_offset prend bien en compte l'ensemble des pages
sequence_offset <- c("")#,c(1:44)*100)

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

#On déduit à partir de l'URL le nom de la loi
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

# Enrichissement de la variable Statut déterminé à la mano 
setwd(paste0(path,"/data/data_loi/"))
nom_loi <- read.csv("nom_loi.csv",sep=";")

data_loi <-unique(data_loi %>%
                    select(uid_loi,nom_loi,date_vote,titre,type_texte))

data_loi$uid_loi <-as.character(data_loi$uid_loi)
data_loi <- inner_join(data_loi,nom_loi,by="nom_loi")

data_loi$uid_loi <-as.character(data_loi$uid_loi )


data_democratie_v2 <- inner_join(vote_final_v2 ,data_loi,by="uid_loi",keep=FALSE) %>%
  select(-date_vote.y)%>%
  rename(date_vote=date_vote.x)

rm(list=c("data_loi","nom_loi","dossier_scrutin","titre_vote","url_texte_loi","url_texte_loi_JO","vote_final_v2",
          "read_data","fonction_url_texte_loi_JO","sequence_offset","function_get_url_dosier_scrutin","fonction_url_texte_loi","extract_num","liste_vote"))

######################################################
### RECUPERATION DES DATA DE POSITION VIS A VIS DE LOI
######################################################

nom_loi_seq <-unique(data_democratie_v2$nom_loi)
  
# Détermointion position député
data_result_2 <- data.frame()

for(j in 1:length(nom_loi_seq)){
    print(nom_loi_seq[j])
    nom_loi_choisi <- nom_loi_seq[j]
    statut_loi <- unique(data_democratie_v2[data_democratie_v2$nom_loi==nom_loi_choisi,]$Statut)
    
    vote_final_ech <- data_democratie_v2 %>%
      mutate(vote_code=as.numeric(vote_code)) %>%
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
      vote_final_ech_mean
      vote_final_ech[i,-1]
      position3 <- 1-mean(t(abs(vote_final_ech[i,-1]-vote_final_ech_mean)),na.rm = TRUE)
      
      resultat_vecteur <- data.frame(nom_loi_choisi,depute_choisi,position3,intensite2,nombre_loi_vote_relatif,nombre_loi_relatif)
      colnames(resultat_vecteur) <-c("nom_loi","depute_code","position","intensite","nombre_vote_relatif_dossier_leg","nombre_texte_relatif_dossier_leg")
      data_result_2 <- rbind(data_result_2,resultat_vecteur)}
  }
  

data_democratie_v3 <- data_democratie_v2 %>%
    inner_join(data_result_2,by=c("nom_loi","depute_code")) 

rm(list=c("nom_loi_seq","data_democratie_v2","data_result_2",
          "resultat_vecteur","nom_loi_choisi","vote_final_ech",
          "vote_final_ech_mean","nombre_loi_vote_relatif",
          "nombre_loi_relatif","position2","position3","depute_choisi","i","j","statut_loi","intensite2","vote_final_ech_depute"))

###########################################
### Suppression des absents non nécessaire
###########################################

data_democratie_v3 <- data_democratie_v3[!is.na(data_democratie_v3$vote_code), ]

######################################
### Création de fichier de vote croise
######################################


vote_final <- data_democratie_v3 %>%
  select(uid_loi,vote_code,depute_code,nom_loi,nombre_vote_relatif_dossier_leg,groupeAbrev)

vote_final$vote_code <- as.numeric(vote_final$vote_code)

loi_seq <- unique(vote_final$nom_loi)

fonction_vote_croise <- function(loi_uid){
  print(loi_uid)
  
  vote_final_select <- vote_final %>% 
    filter(nom_loi == loi_uid)  
  
  vote_final_select <- inner_join(vote_final_select, vote_final_select, by=c("uid_loi")) %>%
    mutate(vote_commun = case_when(vote_code.x==vote_code.y~1,
                                   vote_code.x!=vote_code.y~0),
           loi_commun = 1) %>%
    rename(from =depute_code.x,to=depute_code.y,
           groupe_from =groupeAbrev.x,groupe_to=groupeAbrev.y,
           nombre_loi_vote_relatif_from=nombre_vote_relatif_dossier_leg.x,
           nombre_loi_vote_relatif_to=nombre_vote_relatif_dossier_leg.y) %>%
    select(from,to,groupe_from,groupe_to,nombre_loi_vote_relatif_from,nombre_loi_vote_relatif_to,vote_commun,loi_commun)%>%
    group_by(from,to,groupe_from,groupe_to,nombre_loi_vote_relatif_from,nombre_loi_vote_relatif_to)%>%
    summarise(vote_commun = sum(vote_commun),
              loi_commun=sum(loi_commun),
              similarite=sum(vote_commun)/sum(loi_commun),
              connectivite=sum(loi_commun)/(min(nombre_loi_vote_relatif_to,nombre_loi_vote_relatif_from)))%>%
    filter(from!=to) %>%
    group_by(groupe_from,groupe_to) %>%
    summarise(connectivite=mean(connectivite),similarite=mean(similarite)) %>%
    rename(from=groupe_from,to=groupe_to)
  
  g <- as_data_frame(simplify(graph_from_data_frame(vote_final_select, 
                                                    directed=FALSE)))
  vote_final_select <- unique(inner_join(g,vote_final_select,by=c("from","to"))) 
  
  assign(paste0("vote_croise_","",loi_uid,sep=""),
         vote_final_select)
  
  fwrite(get(paste0("vote_croise_","",loi_uid,sep="")),
         paste0("vote_croise_","",loi_uid,".csv"))
  
  rm(list=c(paste0("vote_croise_","",loi_uid,sep=""),"vote_final_select"))
  
}


setwd(paste0(path,"/data/data_vote_croise/"))
lapply(loi_seq,fonction_vote_croise)

rm(list=c("vote_final","loi_seq","fonction_vote_croise"))

#################################
### Enrichissement date de GROUPE
#################################

groupe_data <- data_democratie_v3 %>%
  select(depute_code,groupeAbrev,nom_loi,position,intensite) %>%
  unique() %>%
  group_by(groupeAbrev,nom_loi) %>%
  summarise(intensite_groupe=mean(intensite),
            position_groupe = mean(position),
            depute_participant = n())

groupe_data_effectif  <-data_democratie_v3 %>%
  select(depute_code,groupeAbrev) %>%
  unique()%>%
  group_by(groupeAbrev)%>%
  summarise(effectif_groupe=n(),
            representation_groupe = n()/577)

groupe <- inner_join(groupe_data,groupe_data_effectif,by="groupeAbrev")%>%
  mutate(participation_groupe=depute_participant/effectif_groupe*intensite_groupe)%>%
  select(groupeAbrev,nom_loi,position_groupe,participation_groupe,representation_groupe)


data_democratie_v3 <- inner_join(data_democratie_v3,groupe,by=c("groupeAbrev","nom_loi"))

rm(list=c("groupe_data","groupe_data_effectif","groupe"))

#################################
### Enrichissement date de GROUPE
#################################

setwd(paste0(path,"/data/data_democratie"))
saveRDS(data_democratie_v3, file = "data_democratie_v3.rds")

setwd(paste0(path,"/script/GetShiny2.0/data/data_democratie"))
saveRDS(data_democratie_v3, file = "data_democratie_v3.rds")
