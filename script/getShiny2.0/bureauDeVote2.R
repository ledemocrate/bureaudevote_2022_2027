library(shiny)
library(readtext)
library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(data.table)
library(shinyjs)
library(Microsoft365R)

library(shinyFiles)
library(shinyvalidate)
library(shinythemes)

library(openssl)
library(sodium)
library(markdown)
library(knitr)

library(maps)
library(geojsonR)
library(geojsonsf)
library(rio)
library(sf)
library(sp)

library(plotly)
library(tm)

library(ggplot2)
library(network)
library(ggnetwork)
library(igraph)
library(tidygraph)
library(ggraph)


options(encoding="UTF-8")

`%nin%` = Negate(`%in%`)

outl <- get_personal_outlook()
od <- get_personal_onedrive()

#CHARGEMENT DES DONNEES VOTE
# Endroit ou vous mettez les fichiers json en telechargeant sous le lien 

responses_bis <- read_excel("data/emargement.xlsx")  %>%
  mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
responses <- read_excel("data/vote.xlsx")

data_democratie<- readRDS(file="data/data_democratie/data_democratie_v3.rds") 

node<- data_democratie %>%
  select(groupeAbrev,position_groupe,nom_loi,participation_groupe)%>%
  unique()


groupe_image <- data.frame(
  Parti = as.character(unique(data_democratie$groupeAbrev)),
  Flag = c('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/Logo_France_Insoumise.svg/langfr-280px-Logo_France_Insoumise.svg.png" height="52"></img>',
           '<img src="https://upload.wikimedia.org/wikipedia/fr/thumb/f/fd/Ps-france-2016.svg/langfr-200px-Ps-france-2016.svg.png" height="52"></img>',
           '<img src="https://www.eelv.fr/files/2020/03/LOGO_EELV_BLANC-1024x547.jpg" height="52"></img>',
           '<img src="https://upload.wikimedia.org/wikipedia/fr/7/76/LOGO-GRS-169.jpg" height="52"></img>',
           '<img src="http://www.retroskatestickers.com/gal/independent/img/classic.jpg" height="52"></img>',
           '<img src="https://www.france-politique.fr/logos/em-2017.png" height="52"></img>',
           '<img src="https://www.les4verites.com/wp-content/uploads/2017/11/logo-lr.png" height="52"></img>',
           '<img src="https://upload.wikimedia.org/wikipedia/commons/9/94/MoDem_logo.svg" height="52"></img>',
           '<img src="https://www.joshthedesigner.com/wp-content/uploads/bfi_thumb/Horizon-Logo-Final-1-n3m28zqd4ylaq5twj9ogo25g6qxjmmnypivn6izpow.jpg" height="52"></img>',
           '<img src="https://upload.wikimedia.org/wikipedia/fr/thumb/3/3c/Parti-radical_2021.png/640px-Parti-radical_2021.png" height="52"></img>',
           '<img src="https://logos-marques.com/wp-content/uploads/2020/04/Le-Rassemblement-national-RN-logo.png" height="52"></img>'
  )
)


departement <- data_democratie %>%
  select(departementNom) %>%
  unique()

parti_liste <- data_democratie %>%
  select(groupeAbrev) %>%
  unique()

fichier <-unique(data_democratie$nom_loi)

fieldsEmargement <- c("Mail","Nom","Prenom","Departement","Naissance","Parti")
fieldsMandatoryEmargement <- c("Mail","Nom","Prenom","Departement","Parti")

fieldsVote <- c("Identifiant","Vote")
fieldsMandatoryVote <- c("Identifiant", "Vote","file1")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}
mail_unique <- function() {
  compose_rules(
    ~ if (sum(loadDataEmargement()$Mail==str_trim(.)) > 0) "Cette adresse mail est déjà associée à un identifiant"
  )
}
majorite <- function() {
  compose_rules(
    ~ if (year(Sys.Date())-year(as.Date(.,origin="1970-01-01")) < 18) "Vous n'êtes pas en maturité pour prendre une décision de vote"
  )
}


saveDataEmargement <- function(data) {
  responses_bis <- read_excel("data/emargement.xlsx")  %>%
    mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
  data <- data %>% as.list() %>% data.frame() 
  responses_bis <- rbind(data,responses_bis)
  write_xlsx(responses_bis,"data/emargement.xlsx")}

loadDataEmargement <- function() {
  responses_bis <- read_excel("data/emargement.xlsx")  %>%
    mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
  read_excel("data/emargement.xlsx") %>%
    mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
}

saveDataVote <- function(data) {
  responses <- read_excel("data/vote.xlsx")
  data <- data %>% as.list() %>% data.frame() 
  responses <- rbind(data,responses)
  write_xlsx(responses,"data/vote.xlsx")}

loadDataVote <- function() {
  responses <- read_excel("data/vote.xlsx")
  read_excel("data/vote.xlsx")
}


# CSS to use in the app
button_color_css <- 
  "#DivCompClear, #FinderClear, #EnterTimes{
  /* Change the background color of the update button
  to blue. */
  background: DodgerBlue;
  /* Change the text size to 15 pixels. */
  font-size: 15px;}
"
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "


ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  navbarPage("Bureau de vote en ligne", theme = shinytheme("lumen"),
             tags$style(button_color_css),
             tabPanel("Emargement", fluid = TRUE, icon = icon("id-card"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Identification"),
                          textInput("Mail", labelMandatory("Mail"), ""),
                          textInput("Nom", labelMandatory("Nom"), ""),
                          textInput("Prenom", labelMandatory("Prenom"), ""),
                          dateInput("Naissance","Naissance :",format ="yyyy-mm-dd",value = "1994-07-25"),
                          selectInput("Departement", labelMandatory("Departement"),departement),
                          selectInput("Parti", labelMandatory("Parti"),parti_liste),
                          actionButton("submit_emargement", "Submit", class = "btn-primary"),
                          actionButton("refresh", "Refresh", class = "btn-primary")),
                        mainPanel(                          
                          h3("Presentation"),
                          uiOutput('presentation', width = "600px"),
                          h3("Tableau des députés"),
                          DT::dataTableOutput("responses_bis", width = 600), tags$hr()))),
             tabPanel("Isoloire", fluid = TRUE, icon = icon("person-booth"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Vote"),
                          selectInput("file1", labelMandatory("Choix d'une loi:"),fichier,selected=fichier[2]),
                          textInput("Identifiant", labelMandatory("Identifiant"), ""),
                          selectInput("Vote", labelMandatory("Décision"),
                                      c("Favorable" = TRUE,
                                        "Défavorable" = FALSE,
                                        "Pas d'avis" = NA)),
                          actionButton("submit", "Submit", class = "btn-primary")),
                        mainPanel(
                          h3("Metadata sur la loi"),
                          tableOutput("metadata_loi"),
                          h3("Résultat par amendement"),
                          tableOutput("metadata_amendement"),
                          plotlyOutput('statistique_loi'),
                          h3("Carte"),
                          selectInput("carte","Choix d'une métrique :",c("position","intensite")),
                          selectInput("zone","Choix d'un zone géographique :",unique(data_democratie$departementNom)),
                          plotlyOutput("resultat_carte"),
                          h3("Graph"),
                          plotOutput("resultat_graph"),
                          h3("Contenue de la loi"),
                          actionButton("button", "Voir plus"),
                          hidden(
                            div(id='text_div',
                                uiOutput('markdown'))),
                          h3("Résultat des votes"),
                          DT::dataTableOutput("responses", width = 300), tags$hr())))))


server <- function(input, output,session) {
  
  ######### Emargement partie
  #  Presentation
  formData_Emargement <- reactive({
    data <- c(sapply(fieldsEmargement, function(x) input[[x]]),
              bin2hex(hash(charToRaw(input$Mail))),as.character(Sys.time()))
    names(data)[7] <- "Identifiant"
    names(data)[8] <- "Date"
    data
  })
  
  output$presentation <- renderUI({HTML(markdown::markdownToHTML(knit("data/fichier_presentation/presentation.rmd",quiet = TRUE)))})
  output$responses_bis <- DT::renderDataTable({
    DT::datatable(groupe_image %>%
                    inner_join(loadDataEmargement(),by="Parti") %>%
                    select(Nom,Prenom,Departement,Parti,Naissance,Date,Flag) %>%
                    group_by(Parti,Flag)%>%
                    summarise(`Nombre de représentant`=n()),escape=FALSE)
  })
  
  ## Partie Variable
  #On regarde si les champs nécessaire sont valide
  iv <- InputValidator$new()
  iv$add_rule("Mail", sv_required())
  iv$add_rule("Mail", sv_email())
  iv$add_rule("Mail", mail_unique())
  iv$add_rule("Naissance", majorite())
  iv$add_rule("Naissance", sv_required())
  iv$add_rule("Departement", sv_required())
  
  iv$enable()
  
  observe({
    mandatoryFilledEmargement <-
      vapply(fieldsMandatoryEmargement,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    if(sum(responses_bis$Mail==str_trim(input$Mail)) == 0 && isValidEmail(input$Mail)) {
      mandatoryFilledEmargement[1] <- TRUE
    } else {
      mandatoryFilledEmargement[1] <- FALSE
    }
    if(year(Sys.Date())-year(input$Naissance) > 17) {
      mandatoryFilledEmargement[5] <- TRUE
    } else {
      mandatoryFilledEmargement[5] <- FALSE
    }
    mandatoryFilledEmargement <- all(mandatoryFilledEmargement)
    
    # En fonction des conditions necessaire on active le bouton submit ou pas
    shinyjs::toggleState(id = "submit_emargement", condition = mandatoryFilledEmargement)
  })
  observeEvent(input$submit_emargement, {
    saveDataEmargement(formData_Emargement())
    loadDataEmargement()
    
    msg <-outl$create_email(paste0("Vous trouverez ci-joint le code vous permettant de voter :", bin2hex(hash(charToRaw(input$Mail)))), 
                            subject = "Token d'authentiufication du bureau de vote en ligne", to = input$Mail)
    
    msg$send()
    
    mandatoryFilledEmargement <-
      vapply(fieldsMandatoryEmargement,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    
    if(sum(responses_bis$Mail==str_trim(input$Mail)) == 0 && isValidEmail(input$Mail)) {
      mandatoryFilledEmargement[1] <- TRUE
    } else {
      mandatoryFilledEmargement[1] <- FALSE
    }
    if(year(Sys.Date())-year(input$Naissance) > 17) {
      mandatoryFilledEmargement[5] <- TRUE
    } else {
      mandatoryFilledEmargement[5] <- FALSE
    }
    mandatoryFilledEmargement <- all(mandatoryFilledEmargement)
    
    # En fonction des conditions necessaire on active le bouton submit ou pas
    shinyjs::toggleState(id = "submit_emargement", condition = mandatoryFilledEmargement)
    
    output$responses_bis <- DT::renderDataTable({
      DT::datatable(groupe_image %>%
        inner_join(loadDataEmargement(),by="Parti") %>%
        select(Nom,Prenom,Departement,Parti,Naissance,Date,flag) %>%
        group_by(Parti,flag)%>%
        summarise(`Representation du bureau de vote`=n(Nom)),escape=FALSE)
    })
    
    updateTextInput(session, "Mail", value = "")
    
    
  })

 

  ######### Vote parte
  
  formData <- reactive({
    data <- c(sapply(fieldsVote, function(x) input[[x]]),
              input$file1,as.character(Sys.time()))
    names(data)[3] <- "Loi"
    names(data)[4] <- "Date"
    data
  })
  
  data_resultat_user <- reactive({
    test <- loadDataVote() %>%
      filter(Loi== input$file1)})
  data_resultat_amendement <- reactive({
    test <- data_democratie %>%
      filter(nom_loi== input$file1) %>%
      select(date_vote,uid_loi,titre,type_texte,nb_pour,nb_contre,nb_abstentions)%>%
      mutate(uid_loi=as.integer(uid_loi))%>%
      unique()%>%
      arrange(uid_loi)
    
    sequence <- seq(1,nrow(test))
    
    test <-  test %>%
      mutate(Ordre = sequence)  %>%
      pivot_longer( cols = starts_with("nb"),
                    names_to = "Position",
                    values_to = "Nombre")
    test
  })
  data_resultat_amendement_2 <- reactive({
    test <- data_democratie %>%
      filter(nom_loi== input$file1) %>%
      select(uid_loi,titre)%>%
      mutate(uid_loi=as.integer(uid_loi))%>%
      unique() %>%
      arrange(uid_loi)})
  data_resultat_carte <- reactive({st_as_sf(data_democratie %>%
                                              filter(nom_loi== input$file1)%>%
                                              filter(departementNom==input$zone )%>%
                                              select(vote_code,departementCode,circo,uid_loi,nom_loi,depute_code,input$carte,geometry,nom,prenom,naissance,age,experienceDepute,nombreMandats,job,groupeAbrev,mail)%>%
                                              mutate(geometry = st_sfc(geometry))%>%
                                              select(depute_code,circo,departementCode,geometry,nom_loi,input$carte,nom,prenom,naissance,age,experienceDepute,nombreMandats,job,groupeAbrev,mail))})
  data_resultat_loi <- reactive({data_democratie %>%
      filter(nom_loi== input$file1)%>%
      mutate(Statut = case_when(Statut==0.5~ "En cours",
                                Statut==1.0~ "Actif",
                                Statut==0.0~ "Rejeté"))%>%
      select(nom_loi,Statut,texte_loi_JO,Statut,nombre_texte_relatif_dossier_leg,url_dossier_associe)%>%
      unique()%>%
      rename('Nom de la loi'= nom_loi,'JO' = texte_loi_JO,'Nombre de texte' = nombre_texte_relatif_dossier_leg,'URL'= url_dossier_associe)})
  data_resultat_graph <- reactive({
    
    node_select <- node %>%
      filter(nom_loi == input$file1) %>%
      select(groupeAbrev,position_groupe,participation_groupe)%>%
      mutate(Statut=case_when(position_groupe>0.5~"POUR",
                              position_groupe<0.5~"CONTRE"))
    
    
    edge <- read.csv(paste0("data/data_vote_croise/vote_croise_",input$file1 , ".csv")) %>%
      mutate(Lien=case_when(similarite>0.5~"ACCORD",
                            similarite<0.5~"PAS D'ACCORD"))
    
    net=graph_from_data_frame(d=edge,vertices = node_select ,directed = FALSE)
    
    tg <- tidygraph::as_tbl_graph(net) 
    tg
  })
  
  
  output$resultat_graph <- renderPlot({
    v.size <- V(data_resultat_graph())$participation_groupe*V(data_resultat_graph())$participation_groupe*100
    data_resultat_graph() %>%
      ggraph(layout = "fr") +
      geom_edge_link2(
        lineend = "round",
        strength = .1,
        aes(edge_width = similarite*100,
            alpha = similarite*100,
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
      theme_graph(foreground = 'steelblue', fg_text_colour = 'white')})
  output$responses <- DT::renderDataTable({
    data_resultat_user() %>%
      inner_join(responses_bis, by ="Identifiant") %>%
      select(Nom,Prenom,Vote,Loi,Date.x) %>%
      rename(Date = Date.x)
  })    
  output$metadata_loi <- renderTable({
    data_resultat_loi()
  })   
  output$metadata_amendement <- renderTable({
    data_resultat_amendement_2()
  })  
  output$statistique_loi<-renderPlotly({ggplotly(ggplot(data_resultat_amendement(), aes(fill=Position, y=Nombre, x=Ordre,label=date_vote,label_2=uid_loi,label_4=type_texte)) + 
               scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
               geom_bar(position="stack", stat="identity") +
               ylim(0,577)+
               ggtitle("Distribution des votes relatifs à un texte de loi"),tooltip = c("date_vote","uid_loi","type_texte"))})
  output$resultat_carte <- renderPlotly({ggplotly(ggplot(data_resultat_carte()) +
                                                    geom_sf(aes(fill=get(input$carte), text = paste0(
                                                      input$carte," : ", get(input$carte),"\n",
                                                      "Prénom : ", prenom, "\n",
                                                      "Nom: ", nom, "\n",
                                                      "Prénom : ", prenom, "\n",
                                                      "Naissance : ", naissance, "\n",
                                                      "Age : ", age, "\n",
                                                      "experienceDepute : ", experienceDepute, "\n",
                                                      "nombreMandats : ", nombreMandats, "\n",
                                                      "job : ", job, "\n",
                                                      "groupeAbrev : ", groupeAbrev, "\n",
                                                      "mail : ", mail, "\n"))) +
                                                    scale_fill_gradient(low = "blue4", high = "red4") +
                                                    labs(fill=input$carte),tooltip = c("text") 
  ) })

  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatoryVote,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    if(sum(responses_bis$Identifiant==input$Identifiant) == 1 | input$Identifiant == bin2hex(hash(charToRaw(input$Mail)))) {
      mandatoryFilled[1] <- TRUE
    } else {
      mandatoryFilled[1] <- FALSE
    }
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  observeEvent(input$button, {
    toggle('text_div')
    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit(paste0("data/data_resume_vie_publique_markdown/",input$file1,".rmd"), quiet = TRUE)))
    })})
  observeEvent(input$submit, {
    saveDataVote(formData())})
  observeEvent(input$submit, {
    output$responses <- DT::renderDataTable({
      loadDataVote() %>%
        inner_join(responses_bis, by ="Identifiant") %>%
        select(Nom,Prenom,Vote,Loi,Date.x) %>%
        rename(Date = Date.x)
    })    
    
  })    
  observeEvent(input$file1, {
    
    output$resultat_carte <- renderPlotly({ggplotly(ggplot(data_resultat_carte()) +
                                                      geom_sf(aes(fill=get(input$carte), text = paste0(
                                                        input$carte," : ", get(input$carte),"\n",
                                                        "Prénom : ", prenom, "\n",
                                                        "Nom: ", nom, "\n",
                                                        "Prénom : ", prenom, "\n",
                                                        "Naissance : ", naissance, "\n",
                                                        "Age : ", age, "\n",
                                                        "experienceDepute : ", experienceDepute, "\n",
                                                        "nombreMandats : ", nombreMandats, "\n",
                                                        "job : ", job, "\n",
                                                        "groupeAbrev : ", groupeAbrev, "\n",
                                                        "mail : ", mail, "\n"))) +
                                                      scale_fill_gradient(low = "blue4", high = "red4") +
                                                      labs(fill=input$carte),tooltip = c("text") 
    ) })
  })
  
  session$onSessionEnded(function() {
    stopApp()
    delfiles <- dir(path=getwd() ,pattern="*.md")
    file.remove(file.path(getwd(), delfiles))})
}

shinyApp(ui, server)

