library(ggplot2)
library(network)
library(ggnetwork)
library(igraph)
library(ggnet)
library(tidygraph)
library(ggraph)
library(tidyverse)
library(plo)
library(extrafont)

loadfonts(dev="all")
windowsFonts()

nom_loi_select <- "reglement budget 2021"

node<- readRDS(file="C:/Users/Quentin GOLLENTZ/Documents/bureaudevote_2022_2027/data/data_democratie/data_democratie_v3.rds") %>%
  select(groupeAbrev,position_groupe,nom_loi,participation_groupe)%>%
  unique() %>%
  filter(nom_loi == nom_loi_select) %>%
  select(groupeAbrev,position_groupe,participation_groupe)%>%
  mutate(Statut=case_when(position_groupe>0.5~"POUR",
                                  position_groupe<0.5~"CONTRE"))
  
  
edge <- read.csv(paste0("C:/Users/Quentin GOLLENTZ/Documents/bureaudevote_2022_2027/data/data_vote_croise/vote_croise_",nom_loi_select , ".csv")) %>%
  mutate(Lien=case_when(similarite>0.5~"ACCORD",
                          similarite<0.5~"PAS D'ACCORD"))

net=graph_from_data_frame(d=edge,vertices = node ,directed = FALSE)

tg <- tidygraph::as_tbl_graph(net) 
v.size <- V(tg)$participation_groupe*V(tg)$participation_groupe*100
E(tg)$weight <- E(tg)$similarite*100 

tg %>%
  ggraph(layout = "fr") +
  geom_edge_link2(
                lineend = "round",
                strength = .1,
                aes(edge_width = weight,
                    alpha = weight,
                    colour=Lien)) +
  geom_node_point(aes(colour = Statut),size=log(v.size)*2) +
  geom_node_text(aes(label = name), 
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size=sqrt(v.size), 
                 colour="gray10") +
  scale_edge_width(range = c(0, 2.5)) +
  scale_edge_alpha(range = c(0, .3)) +
  theme_graph(background = "white") +
  theme(legend.position = "top") +
  guides(edge_width = "none",
         edge_alpha = "none")+ 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

  