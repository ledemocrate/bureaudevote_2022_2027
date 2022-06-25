library(readtext)
library(tidyverse)
library(lubridate)

library(shinyjs)
library(googledrive)
library(googlesheets4)
library(gmailr)

library(shinyFiles)
library(shinyvalidate)
library(shinythemes)

library(openssl)
library(sodium)
library(markdown)
library(knitr)

library(plotly)


options(gargle_oauth_cache = ".secrets")
drive_auth(path = ".secrets")
gm_auth_configure(path = "data/sendMail.json")
gs4_auth(path = ".secrets")

responses_bis <- read_sheet("https://docs.google.com/spreadsheets/d/1clKV4cJdlKSFodG3zkxS0Y_wKfZaMY_urXc0aEHuEb8/edit?usp=sharing")%>%
  mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))

departement <- read.csv("data/data_departement/departements-france.csv") %>%
  select(nom_departement)

data_democratie<- read.csv("data/data_democratie/data_democratie_2.csv")
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
  data <- data %>% as.list() %>% data.frame()
  sheet_append("https://docs.google.com/spreadsheets/d/1clKV4cJdlKSFodG3zkxS0Y_wKfZaMY_urXc0aEHuEb8/edit?usp=sharing", data)
}

loadDataEmargement <- function() {
  responses_bis <-read_sheet("https://docs.google.com/spreadsheets/d/1clKV4cJdlKSFodG3zkxS0Y_wKfZaMY_urXc0aEHuEb8/edit?usp=sharing")%>%
    mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
}

saveDataVote <- function(data) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append("https://docs.google.com/spreadsheets/d/1CZ_-vixtmkbdnq_TuMPQSyaQ-vzxugYqxWXJnVtl2Hs/edit?usp=sharing", data)
} 

loadDataVote <- function() {
  responses <-read_sheet("https://docs.google.com/spreadsheets/d/1CZ_-vixtmkbdnq_TuMPQSyaQ-vzxugYqxWXJnVtl2Hs/edit?usp=sharing")
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
                          h3("Contenue de la loi"),
                          plotlyOutput('statistique_loi'),
                          actionButton("button", "Voir plus"),
                          hidden(
                            div(id='text_div',
                                uiOutput('markdown'))),
                          h3("Résultat des votes"),
                          DT::dataTableOutput("responses", width = 300), tags$hr())))))


server <- function(input, output,session) {
  
  
  observe({
    # check if all mandatory fields have a value
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
  
  responses_bis <- read_sheet("https://docs.google.com/spreadsheets/d/1clKV4cJdlKSFodG3zkxS0Y_wKfZaMY_urXc0aEHuEb8/edit?usp=sharing")%>%
    mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
  
  observe({
    # check if all mandatory fields have a value
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
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit_emargement", condition = mandatoryFilledEmargement)
  })
  
  iv <- InputValidator$new()
  iv$add_rule("Mail", sv_required())
  iv$add_rule("Mail", sv_email())
  iv$add_rule("Mail", mail_unique())
  iv$add_rule("Naissance", majorite())
  iv$add_rule("Naissance", sv_required())
  iv$add_rule("Departement", sv_required())
  
  iv$enable()
  
  ######### Emargement partie
  output$presentation <- renderUI({
    HTML(markdown::markdownToHTML(knit("data/fichier_presentation/presentation.rmd",quiet = TRUE)))})
  
  formData_Emargement <- reactive({
    data <- c(sapply(fieldsEmargement, function(x) input[[x]]),
              bin2hex(hash(charToRaw(input$Mail))),as.character(Sys.time()))
    names(data)[6] <- "Identifiant"
    names(data)[7] <- "Date"
    data
  })
  
  observeEvent(input$submit_emargement, {
    gm_mime() %>%
      gm_from("goldentz.grahams@gmail.com") %>%
      gm_to(input$Mail) %>%
      gm_text_body(paste0("Vous trouverez ci-joint le code vous permettant de voter :", bin2hex(hash(charToRaw(input$Mail))))) %>%
      gm_send_message()
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit_emargement, {
    saveDataEmargement(formData_Emargement())
    loadDataEmargement()})
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses_bis <- DT::renderDataTable({
    input$refresh
    loadDataEmargement() %>%
      select(Nom,Prenom,Departement,Naissance,Date)
    
  })     
  
  ######### Vote parte
  
  dat <- reactive({
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
  
  output$statistique_loi<-renderPlotly(
    ggplotly(ggplot(dat(), aes(fill=Position, y=Nombre, x=Ordre,label=date_vote,label_2=uid_loi,label_3=titre,label_4=type_texte)) + 
               geom_bar(position="stack", stat="identity") +
               ylim(0,570)+
               ggtitle("Distribution des votes relatifs à un texte de loi"),tooltip = c("date_vote","uid_loi","titre","type_texte")))
  
  observeEvent(input$button, {
    toggle('text_div')
    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit(paste0("data/data_resume_vie_publique/",input$file1,".rmd"), quiet = TRUE)))
    })})
  
  formData <- reactive({
    data <- c(sapply(fieldsVote, function(x) input[[x]]),
              input$file1,as.character(Sys.time()))
    names(data)[3] <- "Loi"
    names(data)[4] <- "Date"
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveDataVote(formData())
  })
  observeEvent(input$Identifiant, {
    loadDataEmargement()
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadDataVote() %>%
      inner_join(responses_bis,by="Identifiant")%>%
      select(Nom,Prenom,Vote,Loi,Date.x) %>%
      rename(Date = Date.x)
  })    
  
  session$onSessionEnded(function() {
    stopApp()})
}

shinyApp(ui, server)

