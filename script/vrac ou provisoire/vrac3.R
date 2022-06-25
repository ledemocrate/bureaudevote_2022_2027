
seque <- seq(0,1,0.01)
seque_par_1 <- seque[seque <0.1]




seque <- seq(0,1,0.01)
seque_par_1 <- seque**(log(0.5)/log(0.1))
seque_par_2 <- seque[seque >0.5]**(log(0.5)/log(1-0.1))/2 + 0.5
seque_final <-c(seque_par_1,seque_par_2)
plot(seque_par_2)
plot(seque_par_1)
plot(seque_final)

plot(1/(1+5*exp(seque)))

0.7**1.943358
0.9**6.5
log(0.5)/log(0.7)
0.25/2

# function needed for visualization purposes
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x)))
}


x = 1:53
y = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.1,0.18,0.18,0.18,0.33,0.33,0.33,0.33,0.41,
      0.41,0.41,0.41,0.41,0.41,0.5,0.5,0.5,0.5,0.68,0.58,0.58,0.68,0.83,0.83,0.83,
      0.74,0.74,0.74,0.83,0.83,0.9,0.9,0.9,1,1,1,1,1,1,1)

# fitting code
fitmodel <- nls(y~a/(1 + exp(-b * (x-c))), start=list(a=1,b=.5,c=25))

# visualization code
# get the coefficients using the coef function
params=c(1,1,0.1)

y2 <- sigmoid(params,seque)
plot(y2,type="l")

points(y)
dglogis(5, 5, 1, 2)
library("rmutil")
dglogis(5, 5, 1, 2)
install.packages("grofit")

seque <- seq(0,1,0.01)

plot(1/(1+exp(-30*(seque-0.7))))
sum(1/(1+exp(-0*(seque-0.5)))-seque)**2


curve(logistic(x,a=1.702),-3,3,ylab="Probability of x",
      main="Logistic transform of x",xlab="z score units") 
#logistic with a=1.702 is almost the same as pnorm 
x <- seq(0,1,0.01)
x <- x**(log(0.5)/log(0.1))
zscore(x)
test <- logistic(x,d=0, a=1,c=0, z=1)
curve(logistic(x,d=0, a=1,c=0, z=1))
curve(pnorm(x),add=TRUE,lty="dashed")  
curve(logistic(x),add=TRUE)
text(2,.8, expression(alpha ==1))
text(2,1.0,expression(alpha==1.7))
curve(logistic(x),-4,4,ylab="Probability of x",
      main = "Logistic transform of x in logit units",xlab="logits")
curve(logistic(x,d=-1),add=TRUE)
curve(logistic(x,d=1),add=TRUE)
curve(logistic(x,c=.2),add=TRUE,lty="dashed")
text(1.3,.5,"d=1")
text(.3,.5,"d=0")
text(-1.5,.5,"d=-1")
text(-3,.3,"c=.2")
#demo of graded response model
curve(logistic.grm(x,r=1),-4,4,ylim=c(0,1),main="Five level response scale",
      ylab="Probability of endorsement",xlab="Latent attribute on logit scale")
curve(logistic.grm(x,r=2),add=TRUE)
curve(logistic.grm(x,r=3),add=TRUE)
curve(logistic.grm(x,r=4),add=TRUE)
curve(logistic.grm(x,r=5),add=TRUE)

text(-2.,.5,1)
text(-1.,.4,2)
text(0,.4,3)
text(1.,.4,4)
text(2.,.4,5)
library("psych")
######################################################
# Analyse des données de vote de l'assemblée française
######################################################

library(tidyverse) # Pour avoir tout l'univers tidy
library(tidygraph) #Pour les graphes
library(ggraph) #Pour les graphes
library(igraph) #Pour les graphes
library(statnet)
library(qgraph)
library(RColorBrewer) # Pour graphique
library(lubridate) # Pour graphique
library(data.table) # Pour graphique
library(ggplot2)
library(DescTools)
library(dplyr)

#On recupere le fichier obtenue avec scriptGetData.R
setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/graph/")
list.files()
edge <- read.csv("edge.csv" )
node <- read.csv("node.csv" )

# On va créer une fonction mais pour cela on spécifie d'abord le domaine de définition de cette fonction
# Le cutt-off pour la prise en compte de lien
k_seq <- c(seq(0,1,0.01))
#Les groupes politique
groupe_seq <- unique(as.character(node$groupe_code))

fonction_indice_proximite_groupe <- function(k,groupe) {
  #On s'assure d'avoir des données propre
  #On s'assure d'avoir des données propre
  node_select <-node %>% 
    filter(groupe_code == groupe)
  
  nombre_depute_groupe <- nrow(node_select)
  
  edge_select <- edge %>%
    filter(from %in% node_select$nom_prenom & to %in% node_select$nom_prenom )%>%
    filter(indice>k)
  
  node_select <- node_select %>%
    filter(nom_prenom %in% edge_select$from | nom_prenom %in% edge_select$to )
  
  
  if (nrow(edge_select)==0) {
    indice_final <- data.frame(k=k,indice_noeud = 0 ,indice_lien = 0,indice_groupe =0)
    return(list(indice_final))
  }
  
  if (nrow(edge_select)>0) {
    g <- graph_from_data_frame(edge_select,
                               vertices=node_select, 
                               directed=FALSE) %>% set_vertex_attr("nom_prenom", value = node_select$nom_prenom)
    
    isolated <- which(igraph::degree(g)==0)
    g <-  igraph::delete.vertices(g, isolated)
    
    E(g)$weights <-edge_select$indice
    V(g)$size <- igraph::strength(g)
    V(g)$weights <- igraph::strength(g)
    
    
    indice_final <- data.frame(k=k,
                               indice_noeud = length(V(g))/nombre_depute_groupe,
                               indice_lien = length(E(g))/((length(V(g))*(length(V(g))-1))/2),
                               indice_groupe = length(commu))
    return(list(indice_final))
  }
}

fonction_indice_proximite_groupe(0.84,"FI")
indice_proximite_groupe_prov <- mapply(fonction_indice_proximite_groupe,k_seq,groupe_seq)


indice <-bind_rows(indice_proximite_groupe_prov)

plot(indice$indice_noeud*indice$indice_lien*indice$indice_groupe*k_seq)


indice$indice_groupe <- indice$indice_groupe**(1-k_seq)
plot(indice$indice_groupe)

s3 <- indice$indice_lien
for (i in 2:length(indice$indice_lien)){
  if (indice$indice_lien[i]<=s3[i-1]){
    print("sup")
    s3[i] = s3[i-1]+0.8*(indice$indice_lien[i]-s3[i-1])
  }
  if (indice$indice_lien[i]>s3[i-1]){
    print("inf")
    s3[i]= s3[i-1]
  }
}
s3[which(s3<0)]<-0
plot(s3)
indice$indice_lien <- s3

plot(indice$indice_noeud*indice$indice_lien/indice$indice_groupe)

plot(s2**(1-indice$k))
plot(indice$indice_lien)
nombre_groupe_intra_prov <- unlist(indice_proximite_groupe_prov)[seq(0:length(k_seq)) %% 2 ==0]
nombre_groupe_intra <- nombre_groupe_intra_prov
for (i in 2:length(nombre_groupe_intra_prov)){
  if((nombre_groupe_intra_prov[i]-nombre_groupe_intra_prov[i-1])>=0){
    nombre_groupe_intra[i] <- nombre_groupe_intra[i-1] + (nombre_groupe_intra_prov[i]-nombre_groupe_intra_prov[i-1])
  }
  if((nombre_groupe_intra_prov[i]-nombre_groupe_intra_prov[i-1])<0){
    nombre_groupe_intra[i] <- nombre_groupe_intra[i-1]
  }
}
nombre_groupe_intra
valeur <- indice * 1/nombre_groupe_intra 
valeur
indice_proximite_groupe <- data.frame(k=k_seq,valeur = valeur)

graphique <- indice_proximite_groupe %>% 
  ggplot(aes(x = k, y = valeur )) +
  geom_line()  +
  geom_area()

AUC <- paste("AUC : ", round(AUC(indice_proximite_groupe$k,indice_proximite_groupe$valeur),2))
DDC <- paste("DDC : ", indice_proximite_groupe[which(indice_proximite_groupe$valeur == 0)[1],1]-indice_proximite_groupe[which(indice_proximite_groupe$valeur< 1)[1],1])

graphique + 
  geom_text(mapping = aes(x = 0.6, y = 0.5, label = AUC)) +
  geom_text(mapping = aes(x = 0.6, y = 0.4, label = DDC))  +
  ggtitle("Indice de cohésion du groupe SOC") +
  theme(plot.title = element_text(hjust = 0.5))



devtools::install_github("yanlinlin82/ggvenn")
library(ggvenn)


a <- list(`Set 1` = c(1, 3, 5, 7, 9),
          `Set 2` = c(1, 5, 9, 13),
          `Set 3` = c(1, 2, 8, 9),
          `Set 4` = c(6, 7, 10, 12))
ggvenn(a, c("Set 1", "Set 2"))            # draw two-set venn
ggvenn(a, c("Set 1", "Set 2", "Set 3"))   # draw three-set venn
ggvenn(a)  

# Load library
library(VennDiagram)
grid.newpage()
draw.pairwise.venn(22, 20, 11, category = c("Dog People", "Cat People"), 
                   lty = rep("blank",2), 
                   fill = c("light blue", "pink"), 
                   alpha = rep(0.5, 2), cat.pos = c(0,"Cat People"))

grid.newpage()
draw.pairwise.venn(area1 = 22, area2 = 20, cross.area = 11, category = c("Dog People", 
                                                                         "Cat People"), 
                   lty = rep("blank",2),
                   fill = c("light blue", "pink"),
                   alpha = rep(0.5, 2),
                   cat.pos =c(0,0), 
                   cat.dist = rep(0.025, 2))
# Generate 3 sets of 200 words
set1 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set2 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set3 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = '#14_venn_diagramm.png',
  output=TRUE
)

# Generate 3 sets of 200 words
set1 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set2 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set3 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")

# Prepare a palette of 3 colors with R colorbrewer:
library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = '#14_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)
