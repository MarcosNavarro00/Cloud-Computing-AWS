library(shiny)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(lubridate)
library(dplyr)
library(png)
library(ggplot2)
library(ggjoy)
library(shinyjs)
options(stringsAsFactors = FALSE)

library(DBI)
library(RPostgres)

library(paws.management)
library(httr)
source('R/utils.R')

#options(shiny.port = 8888)
#options(shiny.host = '0.0.0.0')


region <- getRegion()
Sys.setenv(AWS_REGION = region)

ssm_ps <- ssm()

l <- ssm_ps$get_parameters_by_path("/rds")$Parameters
l <- lapply(l, function(x) c(x$Name, x$Value))
names(l) <- lapply(l, function(x) x[1])
l <- lapply(l, function(x) x[2])

db_table_name = l[["/rds/table_name"]]

cognito_client_id <- ssm_ps$get_parameter("/cognito/client_id")$Parameter$Value

conn <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = l[["/rds/database_name"]],
  host = l[["/rds/database_endpoint"]],
  port= l[["/rds/database_port"]],
  user = l[["/rds/database_user"]],
  password = l[["/rds/database_password"]])


if(!dbExistsTable(conn, db_table_name)){
  table_columns <- c('id', 'userid', 'updatedat', 'name', 'content')
  table_columns_types <- c('serial', 'uuid', 'timestamp default current_timestamp', 'varchar(255)', 'jsonb')
  names(table_columns_types) <- table_columns
  dbCreateTable(conn, db_table_name, table_columns_types)
}
# print(dbGetQuery(conn, str_interp("SELECT id, updatedat, name FROM ${db_table_name} WHERE userid = 42")))

ui <- fluidPage(theme = shinytheme("yeti"),
                
                useShinyjs(),
                sidebarLayout(
                  sidebarPanel(width=4,
                               titlePanel("SpotiData - check out your Spotify stats", windowTitle = "SpotiData"),
                              # hr(),
                              # actionButton("zima", "Winter", width = "150px"),
                              # actionButton("wiosna", "Spring", width = "150px"),
                              # br(),
                              # actionButton("lato", "Summer", width = "150px"),
                              # actionButton("jesien", "Autumn", width = "150px"),
                              # br(),
                              # actionButton("resetdat", "Dates reset", width = "304px"),
                               # hr(),
                               # dateRangeInput("daterange1", "Zakres dat:",
                               #                start = "2018-12-01",
                               #                end   = "2020-01-31",
                               #                language = "pl",
                               #                weekstart = 1,
                               #                separator = " do "),
                               hr(),
                               h5('Login:'),
                               textInput("text_login", label=NULL, placeholder = "E-mail", width='304px'),
                               passwordInput("text_passwd", label=NULL, placeholder = "Password", width='304px'),
                               actionButton("log_in", "Sign in", width = '150px'),
                               actionButton("register", "Sign up", width = '150px'),
                               uiOutput('login_info'),
                               hr(),
                               h5('Change password:'),
                               passwordInput("old_passwd", label=NULL, placeholder = "Old password", width='304px'),
                               passwordInput("new_passwd", label=NULL, placeholder = "New password", width='304px'),
                               actionButton("password_change", "Change password", width = '150px', disabled=T),
                               uiOutput('change_passwd'),
                               hr(),
                               h5('Load your data:'),
                               fileInput('files', NULL, multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv", ".json"),
                                         buttonLabel = "Load file",
                                         placeholder = "No file",
                                         width = "220px"),
                               tags$script('
                                     pressedKeyCount = 0;
                                     $(document).on("keydown", function (e) {
                                      Shiny.onInputChange("pressedKey", pressedKeyCount++);
                                      Shiny.onInputChange("key", e.which);
                                      });'),
                               actionButton("save_file", "Save file", width = '150px', disabled=T),
                               hr(),
                               uiOutput('file_list'),
                               hr(),
                               htmlOutput("Opis")
                  ),
                  mainPanel(
                    plotOutput("distPlot",click="click", height = "672px", hover = "hover")
                  )
                )
)

server <- function(input, output, session) {
  
  
  selected_spotidane <- reactiveValues(    #po prostu zbior wartosci reaktywnych - nie sugeruj sie nazwa
    selected = character(),     #nazwa artysty zebrana przez klikniecie na pierwszym wykresie
    x1  = tibble(),      #ramka danych tworzona do przedstawienia pierwszego wykresu
    choices  = tibble(),  #zbior wszystkich artystow, ktorzy byli wyswietlani 
    clicked = numeric(),   #potrzebne do sczytania wspolrzednej y klikniecia mysza na pierwszym wykresie
    click = FALSE, # flaga potrzebna do klikania i odklikiwania artystow - wazna!! decyduje ktory wykres sie wyswietla 
    comeback_possible = FALSE, # do powrotu z drugiego wykresu
    maxvalue = numeric(), #tez do powrotu - okresla polozenie guzika
    begin_date = date("2018-01-01"),   #chyba jasne - paczatkowa data zakresu
    end_date = date("2022-12-31"),   #koncowa
    arrow_index = 0,   #wylicza jaka wartosc nalezy dodać do '1:20' aby byli wyswieltani artysci z zakresu 1+array_index:20+array_index
    keep_range = TRUE,  #czy trzymac zakres skali na pierwszym wykresie
    get_range = TRUE, # czy pobrac zakres skali z pierwszego wykresu
    max_value = numeric(),#potrzebne do okreslenia rozpietosci pierwszego wykresu
    twentieth = numeric(), # do okreslenia na jakiej wysokosci ma byc obrazek
    first_plot = TRUE, #flaga uzyta tylko przy pierwszym wczytywaniu wykresu
    x = FALSE,  #za duzo tych flag, ale dziala - potrzebne bo przy pierwszym wczytaniu plikow zmienia sie daterange, co utrudnia
    window = c(FALSE, TRUE, FALSE), #ktore okno na dole ma sie wyswietlic?
    hover_read = FALSE,   # czy zczytywać dane z hovera - true gdy ma to sens
    hover = c(-0.3, -0.3),  # dane odnosnie myszki
    test = character()
  )
  
  spotidane <-reactiveValues(  #tez nie sugerowac sie nazwa: reaktywne tylko ze inne 
    data = data.frame(),   #ramka danych zawierajaca informacje z wczytanych plikow
    toBind = data.frame()  #tymczasowa ramka wykorzystywana przy wczytywaniu danych
  )
  
  #### obserwatorzy inputow
  observeEvent(input$zima, {
    selected_spotidane$begin_date <- date(format(date("2018-12-22"),"%Y-%m-%d"))
    selected_spotidane$end_date <- date(format(date("2019-03-20"),"%Y-%m-%d"))
    updateDateRangeInput(session, "daterange1", start =  date("2018-12-22"), end = date("2019-03-20"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
    
  })
  observeEvent(input$wiosna, {
    selected_spotidane$begin_date <- date("2019-03-21")
    selected_spotidane$end_date <- date("2019-06-30")
    updateDateRangeInput(session, "daterange1", start =  date("2019-03-21"), end = date("2019-06-21"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  observeEvent(input$lato, {
    selected_spotidane$begin_date <- date(format(date("2019-07-01"),"%Y-%m-%d"))
    selected_spotidane$end_date <- date(format(date("2019-09-30"),"%Y-%m-%d"))
    updateDateRangeInput(session, "daterange1", start =  date("2019-06-22"), end = date("2019-09-22"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  observeEvent(input$jesien, {
    selected_spotidane$begin_date <- date("2019-10-01")
    selected_spotidane$end_date <- date("2019-12-31")
    updateDateRangeInput(session, "daterange1", start =  date("2019-09-23"), end = date("2019-12-21"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  observeEvent(input$resetdat, {
    selected_spotidane$begin_date <- date("2018-12-01") 
    selected_spotidane$end_date <- date("2020-01-31")
    updateDateRangeInput(session, "daterange1", start =  date("2018-12-01"), end = date("2020-01-31"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  
  observeEvent(input$daterange1, {
    selected_spotidane$begin_date <- input$daterange1[1]
    selected_spotidane$end_date <- input$daterange1[2]
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
    
  })
  observeEvent(input$pressedKey, {
    
    if(!selected_spotidane$click){
      if(input$key %in% c(38, 40)){
        tmp <- selected_spotidane$arrow_index + input$key -39
        if(tmp >=0){
          selected_spotidane$arrow_index <- tmp
          selected_spotidane$first_plot <- FALSE
          selected_spotidane$get_range <- FALSE
          selected_spotidane$keep_range <- TRUE
        }
      }
      
    }
    else{
      if(input$key == 39){ 
        if(which(selected_spotidane$window)==1){
          selected_spotidane$window <- c(FALSE, TRUE, FALSE)
        }
        else if(which(selected_spotidane$window)==2){
          selected_spotidane$window <- c(FALSE, FALSE, TRUE)
        }
      }
      else if(input$key == 37){
        if(which(selected_spotidane$window)==3){
          selected_spotidane$window <- c(FALSE, TRUE, FALSE)
        }
        else if(which(selected_spotidane$window)==2){
          selected_spotidane$window <- c(TRUE, FALSE, FALSE)
        }
      }
    }
    
  })
  
  observeEvent(input$hover,{
    if(selected_spotidane$hover_read){
      selected_spotidane$hover[1] <- input$hover$x
      selected_spotidane$hover[2] <- input$hover$y
    }
  })
  
  observeEvent(input$click,{
    if(!selected_spotidane$click) {#w przypadku gdy wyswietlany jest 1. wykres
      selected_spotidane$click = TRUE
      #zapisanie wykonawcy jaki ma byc wyswietlany w drugim oknie - o ile click$y występuje
      selected_spotidane$clicked[1] <-ifelse(!is.null(input$click$y), input$click$y, selected_spotidane$clicked[1])
    }
    else{#gdy drugi wykres jest wyswietlany, zbieramy klikniecie dla przycisku powróć
      if(which(selected_spotidane$window)==1){
        if(input$click$x>0 && input$click$x<3){
          if(input$click$y<selected_spotidane$maxvalue*1.08 && input$click$y>selected_spotidane$maxvalue*1.02){
            selected_spotidane$click = FALSE
            selected_spotidane$window = c(FALSE, TRUE, FALSE) #reset informacji ktory wykres ma byc wyswietlony na drugim poziomie
          }
        }
        
        
      }
      else if(which(selected_spotidane$window)==2){
        
        
      }
      else if(which(selected_spotidane$window)==3){
        
      }
    }
    
    selected_spotidane$first_plot <- FALSE
  })
  ######
  observeEvent(input$save_file,{
    tryCatch({
      # browser()
      file_data = as.character(toJSON(spotidane$data))
      user_id <- session$userData$user_id
      fn <- length(input$files$name)
      if(fn>0){
        dbAppendTable(conn, db_table_name, data.frame(userid=rep(user_id, fn), name=input$files$name, content=file_data))
      
        output$file_list <- renderUI({
            update_file_list(input, tags, session, output, spotidane, selected_spotidane, conn, db_table_name)
      }) 
      }
      
      
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
  })
  
  observeEvent(input$register,{
    output$login_info <- renderUI("")
    login_username <- input$text_login
    login_password <- input$text_passwd
    
    resp <- POST(paste0("https://cognito-idp.", region, ".amazonaws.com/"), add_headers("X-Amz-Target"="AWSCognitoIdentityProviderService.SignUp", "Content-Type"="application/x-amz-json-1.1"),
                 body=list("Username"=login_username, "Password"=login_password, 'UserAttributes'=data.frame(list(Name=c("email"), Value=c(login_username))), "ClientId"=cognito_client_id), encode='json')
    json_resp <- fromJSON(rawToChar(content(resp)))
    if(resp$status_code==200){
      output$login_info <- renderUI(paste0("You are signed up, ", login_username))
    }
    else{
      output$login_info <- renderUI(json_resp$message)
    }
    
  })
  
  observeEvent(input$log_in,{
    output$login_info <- renderUI("")
    login_username <- input$text_login
    login_password <- input$text_passwd
    
    resp <- POST(paste0("https://cognito-idp.", region, ".amazonaws.com/"), add_headers("X-Amz-Target"="AWSCognitoIdentityProviderService.InitiateAuth", "Content-Type"="application/x-amz-json-1.1"),
                 body=list("AuthParameters"=list("USERNAME"=login_username, "PASSWORD"=login_password), "AuthFlow"="USER_PASSWORD_AUTH", "ClientId"=cognito_client_id), encode='json')
    json_resp <- fromJSON(rawToChar(content(resp)))
    if(resp$status_code == 200){
      session$userData$cognito <- json_resp$AuthenticationResult
      
      
      resp_getuser <- POST(paste0("https://cognito-idp.", region, ".amazonaws.com/"), add_headers("X-Amz-Target"="AWSCognitoIdentityProviderService.GetUser", "Content-Type"="application/x-amz-json-1.1"),
                 body=list(AccessToken=json_resp$AuthenticationResult$AccessToken), encode='json')
      json_resp_getuser <- fromJSON(rawToChar(content(resp_getuser)))
      
      if(resp_getuser$status_code == 200){
      
        ua_df <- json_resp_getuser$UserAttributes
        user_id <- ua_df[ua_df['Name']=='sub','Value'][1]
        session$userData$user_id <- user_id
        
        
        output$file_list <- renderUI({
          update_file_list(input, tags, session, output, spotidane, selected_spotidane, conn, db_table_name)
      })
        enable('password_change')
        output$login_info <- renderUI(paste0("You are logged in ", login_username))
        
      }
      else{
        output$login_info <- renderUI('Internal Error')
      }
    }
    else{
      output$login_info <- renderUI(json_resp$message)
    }
    
  })
  
  observeEvent(input$password_change, {
    resp_chpasswd <- POST(paste0("https://cognito-idp.", region, ".amazonaws.com/"),
                         add_headers("X-Amz-Target"="AWSCognitoIdentityProviderService.ChangePassword", "Content-Type"="application/x-amz-json-1.1"),
                 body=list(AccessToken=session$userData$cognito$AccessToken, PreviousPassword=input$old_passwd, ProposedPassword=input$new_passwd), encode='json')
    json_resp_chpasswd <- fromJSON(rawToChar(content(resp_chpasswd)))
    if(resp_chpasswd$status_code==200){
      output$change_passwd <- renderUI("Pasword changed succesfully")
    }
    else{
      output$change_passwd <- renderUI(json_resp_chpasswd$message)
    }
  })
  
  
  ######
  # logika wczytywania plikow
  #
  observeEvent(input$files,{
    tryCatch(
      {
        l <- loadFile(input$files, spotidane, selected_spotidane, session)
        spotidane <- l$spotidane
        selected_spotidane <- l$selected_spotidane
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #####
    #Wczytywanie obu wykresow - ggplot
    
    output$distPlot <- plotrender(spotidane, selected_spotidane)
  })
  output$Opis <- renderUI({
    req(input$files)
    if(!selected_spotidane$click){
      HTML(paste("HINT:", "Use your keyboard's arrows", "   - check what happens!", sep="<br/>"))
    }
    else{
      if(selected_spotidane$window[1]){HTML(paste("HINT:", "To get to know more", "hover over a point", sep="<br/>"))} #dla wykresu Pawła
      else{
        
        HTML(paste("HINT:", "Use your keyboard's arrows", "   - now sideways!", sep="<br/>"))
      }
    }
  })
  
  
}


shinyApp(ui = ui, server = server)




