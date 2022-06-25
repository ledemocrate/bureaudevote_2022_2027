library(tidyverse) 
library(data.table)
library(igraph)
library(lsa)
library(data.table)

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_democratie/")
list.files()

data_democratie <- fread("data_democratie.csv" )

nom_loi_choix_grid <- unique(str_trim(data_democratie$nom_loi))
fonction_caclul_position_loi <- function(nom_loi_choix) {
  nom_loi_choix <- "certification publique performances sociales environnementales entreprises"
  print(nom_loi_choix)
  
  statut_loi <-unique(data_democratie %>%
                       filter(str_trim(nom_loi)!="") %>%
                       filter(nom_loi==nom_loi_choix) %>%
                       select(statut_loi))
  
  
  vote_final_ech <- data_democratie %>%
  filter(str_trim(nom_loi)!="") %>%
  filter(nom_loi==nom_loi_choix) %>%
  select(depute_code,vote_code,uid_loi) %>%
  pivot_wider(names_from = uid_loi,values_from = vote_code)

matrix_final_ech <- as.data.table(vote_final_ech[,c(-1)])
vecteur_majoritaire <-  round(matrix_final_ech[, lapply(.SD, mean, na.rm = TRUE)])

matrix_final_ech <- as.data.frame(bind_rows(matrix_final_ech,vecteur_majoritaire))

row.names(matrix_final_ech) <-c(vote_final_ech$depute_code,"vecteur_majoritaire")

matrix_final_ech <-cbind(row.names(matrix_final_ech),matrix_final_ech)
colnames(matrix_final_ech)<- colnames(vote_final_ech)

df <- melt(matrix_final_ech,id.vars="depute_code")

cosine <- function( x, y=NULL ) {
  
  if ( is.matrix(x) && is.null(y) ) {
    
    co = array(0,c(ncol(x),ncol(x)))
    f = colnames( x )
    dimnames(co) = list(f,f)
    
    for (i in 2:ncol(x)) {
      for (j in 1:(i-1)) {
        co[i,j] = cosine(x[,i], x[,j])
      }
    }
    co = co + t(co)
    diag(co) = 1
    
    return (as.matrix(co))
    
  } else if ( is.vector(x) && is.vector(y) ) {
    return ( crossprod(x,y) / sqrt( crossprod(x)*crossprod(y) ) )
  } else {
    stop("argument mismatch. Either one matrix or two vectors needed as input.")
  }
  
}
cosine2 <- function(x,y) euc(na.omit(cbind(x,y)))

i <- outer(unique(df$depute_code),unique(df$depute_code),FUN=function(i,j) i)
j <- outer(unique(df$depute_code),unique(df$depute_code),FUN=function(i,j) j)

i <- i[!lower.tri(i)]
j <- j[!lower.tri(j)]

comp <- function(ind) {
  res <- cosine2(df$value[df$depute_code==i[ind]],df$value[df$depute_code==j[ind]])[1,2]
  list(from=as.character(i[ind]),to=as.character(j[ind]),CosSim=res)
}

res <- as.data.frame(t(sapply(seq_along(i),FUN="comp")))
print("Les vecteurs sont bien calculés")
res <- res[!is.na(res$CosSim),]
res$CosSim <-as.numeric(res$CosSim)

res_calcul_vote_depute <- res %>%
  filter(to=="vecteur_majoritaire")
res_calcul_vote_depute$CosSim <-as.numeric(res_calcul_vote_depute$CosSim)

if (statut_loi==1) {
  res_calcul_vote_depute <- res_calcul_vote_depute %>%
    mutate(vote_choix =  CosSim) %>%
    rename(depute_code=from) %>%
    select(depute_code,vote_choix)
}


if (statut_loi==0) {
  res_calcul_vote_depute <- res_calcul_vote_depute %>%
    mutate(vote_choix =  1-CosSim) %>%
    rename(depute_code=from) %>%
    select(depute_code,vote_choix)
}

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_vote_depute/")


assign(paste0("vote_depute_","",nom_loi_choix,sep=""),
       res_calcul_vote_depute)

fwrite(get(paste0("vote_depute_","",nom_loi_choix,sep="")),
       paste0("vote_depute_","",nom_loi_choix,".csv"))

res_calcul_vote_croise <- res %>%
  filter(to!="vecteur_majoritaire")

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_vote_croise/")

assign(paste0("vote_croise_","",nom_loi_choix,sep=""),
       res_calcul_vote_croise)

fwrite(get(paste0("vote_croise_","",nom_loi_choix,sep="")),
       paste0("vote_croise_","",nom_loi_choix,".csv"))

rm(list=c(paste0("vote_croise_","",nom_loi_choix,sep=""),
              paste0("vote_depute_","",nom_loi_choix,sep=""),
              "res_calcul_vote_croise","res_calcul_vote_depute","res",
              "comp","i","j","df","matrix_final_ech","statut_loi","vote_final_ech","vecteur_majoritaire"))
}

lapply(nom_loi_choix_grid,fonction_caclul_position_loi)
fonction_caclul_position_loi(nom_loi_choix_grid[6])
nom_loi_choix_grid
