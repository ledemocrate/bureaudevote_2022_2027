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

options(encoding="UTF-8")

outl <- get_personal_outlook()
od <- get_personal_onedrive()

#CHARGEMENT DES DONNEES VOTE
# Endroit ou vous mettez les fichiers json en telechargeant sous le lien 
path <- getwd()

responses_bis <- read_excel("data/emargement.xlsx")  %>%
  mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
responses <- read_excel("data/vote.xlsx")

departement <- read.csv("data/data_departement/departements-france.csv") %>%
  select(nom_departement)

file_js = geojson_sf("data/data_circo/data_circo.json") %>%
  filter(str_detect(code_dpt,"Z")==FALSE)%>%
  select(num_circ,code_dpt,geometry)%>%
  rename(circo = num_circ,departementCode =code_dpt )%>%
  mutate(circo = as.numeric(circo))

data_democratie<- fread("data/data_democratie/data_democratie_2.csv",encoding = "UTF-8") 
data_democratie$circo <- as.numeric(data_democratie$circo)

fichier <-unique(data_democratie$nom_loi)

fieldsEmargement <- c("Mail","Nom","Prenom","Departement","Naissance")
fieldsMandatoryEmargement <- c("Mail","Nom","Prenom","Departement")

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
    ~ if (sum(responses_bis$Mail==str_trim(.)) > 0) "Cette adresse mail est déjà associée à un identifiant"
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
                          actionButton("submit_emargement", "Submit", class = "btn-primary"),
                          actionButton("refresh", "Refresh", class = "btn-primary")),
                        mainPanel(                          
                          h3("Presentation"),
                          uiOutput('presentation'),
                          h3("Tableau des députés"),
                          DT::dataTableOutput("responses_bis", width = 300), tags$hr()))),
             tabPanel("Isoloire", fluid = TRUE, icon = icon("person-booth"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Vote"),
                          selectInput("file1", labelMandatory("Choix d'une loi:"),fichier),
                          textInput("Identifiant", labelMandatory("Identifiant"), ""),
                          selectInput("Vote", labelMandatory("Décision"),
                                      c("Favorable" = TRUE,
                                        "Défavorable" = FALSE,
                                        "Pas d'avis" = NA)),
                          actionButton("submit", "Submit", class = "btn-primary")),
                        mainPanel(
                          h3("Résultat par amendement"),
                          plotlyOutput('statistique_loi'),
                          h3("Carte des position"),
                          plotOutput("resultat_carte"),
                          h3("Carte des intensité"),
                          plotOutput("resultat_carte_intensite"),
                          h3("Contenue de la loi"),
                          actionButton("button", "Voir plus"),
                          hidden(
                            div(id='text_div',
                                uiOutput('markdown'))),
                          h3("Résultat des votes"),
                          DT::dataTableOutput("responses", width = 300), tags$hr())))))


server <- function(input, output,session) {
  
  ######### Emargement partie
  ## Partie Fixe
  #  Presentation
  output$presentation <- renderUI({
    HTML(markdown::markdownToHTML(knit("data/fichier_presentation/presentation.rmd",quiet = TRUE)))})
  #  Tableau
  output$responses_bis <- DT::renderDataTable({
    loadDataEmargement() %>%
      select(Nom,Prenom,Departement,Naissance,Date)})
  
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
  
  formData_Emargement <- reactive({
    data <- c(sapply(fieldsEmargement, function(x) input[[x]]),
              bin2hex(hash(charToRaw(input$Mail))),as.character(Sys.time()))
    names(data)[6] <- "Identifiant"
    names(data)[7] <- "Date"
    data
  })
  
  observeEvent(input$submit_emargement, {
    msg <-outl$create_email(paste0("Vous trouverez ci-joint le code vous permettant de voter :", bin2hex(hash(charToRaw(input$Mail)))), 
                            subject = "Token d'authentiufication du bureau de vote en ligne", to = input$Mail)
    
    msg$send()
    
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit_emargement, {
    saveDataEmargement(formData_Emargement())})
  
  observeEvent(input$submit_emargement, {
  output$responses_bis <- DT::renderDataTable({
     loadDataEmargement() %>%
      select(Nom,Prenom,Departement,Naissance,Date)})
  })     
  
  ######### Vote parte
  ## Partie Fixe
  output$responses <- DT::renderDataTable({
    loadDataVote() %>%
      inner_join(responses_bis, by ="Identifiant") %>%
      select(Nom,Prenom,Vote,Loi,Date.x) %>%
      rename(Date = Date.x)
  })    
  
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
  

  data_resultat_amendement <- reactive({
    test <- data_democratie %>%
      filter(nom_loi== input$file1) %>%
      select(date_vote,uid_loi,titre,type_texte,nb_pour,nb_contre)%>%
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
  
  data_resultat_carte <- reactive({st_as_sf(data_democratie %>%
                                              filter(nom_loi== input$file1)%>%
                                              select(vote_code,departementCode,circo,uid_loi,nom_loi,depute_code,position,intensite)%>%
                                              na.omit() %>%
                                              unique() %>%
                                              inner_join(file_js,by=c("departementCode","circo"))%>%
                                              mutate(geometry = st_sfc(geometry))%>%
                                              select(depute_code,circo,departementCode,geometry,nom_loi,position,intensite))})
 
             
             
  output$statistique_loi<-renderPlotly(
    ggplotly(ggplot(data_resultat_amendement(), aes(fill=Position, y=Nombre, x=Ordre,label=date_vote,label_2=uid_loi,label_3=titre,label_4=type_texte)) + 
               geom_bar(position="stack", stat="identity") +
               ylim(0,570)+
               ggtitle("Distribution des votes relatifs à un texte de loi"),tooltip = c("date_vote","uid_loi","titre","type_texte")))
  
  output$resultat_carte <- renderPlot({plot(data_resultat_carte()["position"])})
  output$resultat_carte_intensite <- renderPlot({plot(data_resultat_carte()["intensite"])})
  
  formData <- reactive({
    data <- c(sapply(fieldsVote, function(x) input[[x]]),
              input$file1,as.character(Sys.time()))
    names(data)[3] <- "Loi"
    names(data)[4] <- "Date"
    data
  })
  
  # When the Submit button is clicked, save the form data
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

  
  session$onSessionEnded(function() {
    stopApp()
    delfiles <- dir(path=getwd() ,pattern="*.md")
    file.remove(file.path(getwd(), delfiles))})
}

shinyApp(ui, server)

