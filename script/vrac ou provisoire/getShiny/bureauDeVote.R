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

options(gargle_oauth_cache = ".secrets")
drive_auth(path = ".secrets")
gm_auth_configure(path = "sendMail.json")
gs4_auth(path = ".secrets")

responses_bis <- read_sheet("https://docs.google.com/spreadsheets/d/1HFwLfwCCC5Bv7jt5JChnR2wiOO2-cd7PMUMkLXpGbh8/edit?usp=sharing")%>%
  mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))

departement <- read.csv("departements-france.csv") %>%
  select(nom_departement)

fichier <- list.files(paste0(getwd(),"/data_resume_vie_publique"))

fieldsEmargement <- c("Mail","Nom","Prenom","Naissance")
fieldsMandatoryEmargement <- c("Mail","Nom","Prenom")

fieldsVote <- c("Identifiant","Vote")
fieldsMandatoryVote <- c("Identifiant", "Vote","file1")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

mail_unique <- function() {
  compose_rules(
    ~ if (sum(responses_bis$Mail==.) == 1) "Cette adresse mail est déjà associée à un identifiant"
  )
}
majorite <- function() {
  compose_rules(
    ~ if (year(Sys.Date())-year(as.Date(.,origin="1970-01-01")) < 18) "Vous n'êtes pas en maturité pour prendre une décision de vote"
  )
}
identifiant_valide <- function() {
  compose_rules(
    ~ if (sum(responses_bis$Identifiant==.) == 0) "Cet identifiant n'est pas valide"
  )
}


saveDataEmargement <- function(data) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append("https://docs.google.com/spreadsheets/d/1HFwLfwCCC5Bv7jt5JChnR2wiOO2-cd7PMUMkLXpGbh8/edit?usp=sharing", data)
  
  
}

loadDataEmargement <- function() {
  
  read_sheet("https://docs.google.com/spreadsheets/d/1HFwLfwCCC5Bv7jt5JChnR2wiOO2-cd7PMUMkLXpGbh8/edit?usp=sharing")%>%
    mutate(Naissance = as.Date(as.integer(Naissance),origin="1970-01-01"))
  
}

saveDataVote <- function(data) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append("https://docs.google.com/spreadsheets/d/19j0z8Q8s-erOy5eH4zjzPNPvCmd5UkQr5i8f738dfy0/edit?usp=sharing", data)
} 

loadDataVote <- function() {
  
  read_sheet("https://docs.google.com/spreadsheets/d/19j0z8Q8s-erOy5eH4zjzPNPvCmd5UkQr5i8f738dfy0/edit?usp=sharing")
  
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
                          dateInput("Naissance","Naissance :",format ="yyyy-mm-dd"),
                          selectInput("Département", labelMandatory("Département"),departement),
                          actionButton("submit_emargement", "Submit", class = "btn-primary"),
                          actionButton("refresh", "Refresh", class = "btn-primary")),
                        mainPanel(
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
                          h3("Nom de la loi"),
                          verbatimTextOutput("Loi",placeholder = FALSE),
                          h3("Contenue de la loi"),
                          actionButton("button", "Voir plus"),
                          hidden(
                            div(id='text_div',
                                verbatimTextOutput("contents",placeholder = FALSE))),
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
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilledEmargement <-
      vapply(fieldsMandatoryEmargement,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
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
  iv$add_rule("Identifiant", identifiant_valide())
  
  iv$enable()
  
  ######### Emargement partie
  formData_Emargement <- reactive({
    data <- c(sapply(fieldsEmargement, function(x) input[[x]]),
              bin2hex(hash(charToRaw(input$Mail))),as.character(Sys.time()))
    names(data)[5] <- "Identifiant"
    names(data)[6] <- "Date"
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
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses_bis <- DT::renderDataTable({
    input$refresh
    loadDataEmargement()
  })     
  
  ######### Vote parte
  
  output$contents <- renderText(read_file(paste0(getwd(),"/data_resume_vie_publique/",input$file1)))
  
  observeEvent(input$button, {
    toggle('text_div')
    output$Loi <- textOutput(input$file1)
  })
  
  formData <- reactive({
    data <- c(sapply(fieldsVote, function(x) input[[x]]),
              input$file1$name,as.character(Sys.time()))
    names(data)[3] <- "Loi"
    names(data)[4] <- "Date"
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveDataVote(formData())
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadDataVote()
  })    
  
  session$onSessionEnded(function() {
    stopApp()})
}

shinyApp(ui, server)

