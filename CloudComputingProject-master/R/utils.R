library(shiny)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(lubridate)
library(dplyr)
library(png)
library(ggplot2)
library(ggjoy)
library(httr)
library(shinyjs)
options(stringsAsFactors = FALSE)

clickme <- readPNG('clickme.png')

getRegion <- function(){
  content(GET("http://169.254.169.254/latest/meta-data/placement/region"))
}

loadFile <- function(f, spotidane, selected_spotidane, session){
  spotidane$data <- data.frame()
  selected_spotidane$click <- FALSE
  selected_spotidane$arrow_index <- 0
  selected_spotidane$first_plot <- TRUE
  selected_spotidane$window = c(FALSE, TRUE, FALSE)
  # browser()
  if('datapath' %in% names(f)){
    for(var in 1:length(f$datapath)){
      if(f$type[var] %in% c("text/csv", "application/csv")){
        spotidane$toBind <-  as.data.frame(as_data_frame(read.csv(f$datapath[var])))
        
        if(all(colnames(spotidane$toBind) == c("endTime", "artistName", "trackName", "msPlayed"))){
          spotidane$toBind$endTime <- as.character(spotidane$toBind$endTime)
          spotidane$toBind$endTime <- substr(spotidane$toBind$endTime, start = 1, stop = nchar(spotidane$toBind$endTime)-3)
          spotidane$data <- rbind(spotidane$data, spotidane$toBind)
        }
      }
      if(f$type[var] %in% c("application/json", "text/json")){
        spotidane$toBind <- as.data.frame(fromJSON(f$datapath[var]))
        if(all(colnames(spotidane$toBind) == c("endTime", "artistName", "trackName", "msPlayed"))){
          spotidane$data <- rbind(spotidane$data, spotidane$toBind)
        }
      }
    }
    if(nrow(spotidane$data)>0){
      spotidane$data$msPlayed <- spotidane$data$msPlayed/(1000*3600)
      spotidane$data$endTime <- as.character(spotidane$data$endTime)
      #spotidane$data$endTime <- fast_strptime(spotidane$data$endTime, "%Y-%m-%d %H:%M",tz="UTC")
      spotidane$data$endTime <- as.POSIXct(spotidane$data$endTime)
    }
    if('AccessToken' %in% names(session$userData$cognito)){
      enable('save_file')
    }
  }
  else if(is.data.frame(f)){
    spotidane$toBind <- as.data.frame(fromJSON(f$content))
    if(all(as.logical(lapply(colnames(spotidane$toBind), function(x) {x %in% c("endTime", "artistName", "trackName", "msPlayed")})))){
      spotidane$data <- rbind(spotidane$data, spotidane$toBind)
    }
    if(nrow(spotidane$data)>0){
      spotidane$data$endTime <- as.character(spotidane$data$endTime)
      spotidane$data$endTime <- as.POSIXct(spotidane$data$endTime)
    }
  }
  list(spotidane=spotidane, selected_spotidane=selected_spotidane)
}

plotrender <- function(spotidane, selected_spotidane){
  renderPlot({
    if(!selected_spotidane$click){
      ### gdy nie bylo jeszcze klikniecia - pokazujemy pierwszy wykres
      # wczytanie danych
      selected_spotidane[["x1"]] <- spotidane$data %>%
        filter(endTime >= selected_spotidane$begin_date)%>%
        filter(endTime <= selected_spotidane$end_date)%>%
        group_by(artistName) %>% summarise(uniquesongs = length(unique(trackName)), time = sum(msPlayed)) %>%
        arrange(desc(time)) %>% slice((1:20) + selected_spotidane$arrow_index) %>%
        mutate(artistName = paste0(row_number() + selected_spotidane$arrow_index, ". ", artistName))
      
      if(selected_spotidane$get_range){
        selected_spotidane$max_value = selected_spotidane[["x1"]][["time"]][1] #do trzymania zakresu osi godzin
        selected_spotidane$twentieth = selected_spotidane[["x1"]][["time"]][20]
      }
      x <- selected_spotidane[["x1"]]
      selected_spotidane$choices <- selected_spotidane[["x1"]]$artistName
      if(nrow(selected_spotidane[["x1"]])==0 || all(is.na(selected_spotidane[["x1"]][["artistName"]]))) {#jesli nie mamy zadnych danych
        plot.new()
        text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
      }
      else{
        p <- ggplot(selected_spotidane[["x1"]], aes(x=reorder(artistName,-time), y=time)) +
          geom_bar(stat="identity", width=.7,fill="#1D428A")+
          theme_minimal()+
          # geom_text(aes(label = ifelse(time==selected_spotidane$max_value,paste0("Liczba przesłuchanych różnych utworów wykonawcy: ", uniquesongs), uniquesongs)),
          #           size = 4.6, hjust = "top", nudge_y = -0.2, color = "white") +
          theme(axis.text.x = element_text(hjust = 1,size=11),
                axis.text.y =element_text(size=15))+
          xlab(element_blank())+
          ylab("Liczba przesłuchanych godzin wykonawcy") +
          labs(title = "Ranking najdłużej słuchanych wykonawców") +
          scale_x_discrete(limits = rev(as.factor(x$artistName)), label = function(t) ifelse(nchar(t)>20,
                                                                                             paste0(substr(t, 1, 20), "..."), t)) +
          coord_flip()
        if(selected_spotidane$first_plot){  #jesli ma sie pojawic obrazek - niech sie pojawi
          z <- p + annotation_raster(clickme, ymin =selected_spotidane$twentieth*1.1,
                                     ymax= selected_spotidane$twentieth*1.1 + selected_spotidane$max_value/6,
                                     xmin =0.7, xmax = 3.7)
        }
        else{z <- p}
        if(selected_spotidane$keep_range){
          z + scale_y_continuous(position = "right", limits = c(0, selected_spotidane$max_value))
        }
        else{
          z + scale_y_continuous(position = "right")
        }
      }
    }
    else{
      #####drugi poziom wykresow (po kliknieciu w pierwszy)
      lvls <- selected_spotidane$choices
      artist <- lvls[21 - round(selected_spotidane$clicked[1])]
      selected_spotidane$selected <- paste(unlist(strsplit(artist, split = " "))[-1], collapse = " ")
      
      if(which(selected_spotidane$window)==2){
        df <- spotidane$data %>%
          filter(endTime >= selected_spotidane$begin_date) %>%
          filter(endTime <= selected_spotidane$end_date) %>%
          filter(artistName == selected_spotidane$selected)
        df$month <- month(df$endTime,label=TRUE)
        df$month <- fct_rev(factor(df$month))
        df$day <- day(df$endTime)
        
        
        if(nrow(df)==0) {
          plot.new()
          text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
        }
        else{
          ggplot(df,aes(x=day,y=month,fill=month))+
            geom_density_ridges(scale = 3, rel_min_height = 0.01) +
            xlab('') +
            ylab('')+
            labs(title = paste0("Częstość słuchania zespołu ", selected_spotidane$selected, " z podziałem na miesiące"), caption = "• 2 •") +
            scale_fill_viridis_d(alpha=0.7,guide='none')+
            scale_y_discrete(expand = c(0.1, 0))+
            theme(legend.position = 'none', axis.title.y = element_blank(),
                  plot.caption = element_text(size = 7))+
            # annotate("text", x = 35, y =  13, label = "• 2 •", size = 7, fontface = "bold") +
            theme_ridges()
        }
        
      }
      ##koniec srodkowego wykresu
      
      ##ten z lewej:
      else if(which(selected_spotidane$window) == 1){
        zwroc_czas <- function(d) {
          weekday = weekdays(d, abbreviate = TRUE)
          hour = format(strptime(d,"%Y-%m-%d %H:%M:%S"),'%H')
          hour = as.numeric(hour) %/% 6 * 6
          paste0(weekday, " ", hour, ":00")
        }
        y <- spotidane$data %>%
          filter(endTime >= selected_spotidane$begin_date) %>%
          filter(endTime <= selected_spotidane$end_date) %>%
          filter(artistName == selected_spotidane$selected) %>%
          mutate(pora = zwroc_czas(endTime)) %>%
          count(pora) %>%
          mutate(grupa = 1)
        
        y2 <- spotidane$data %>%
          filter(artistName == selected_spotidane$selected) %>%
          mutate(pora = zwroc_czas(endTime)) %>%
          count(pora) %>%
          mutate(grupa = 2)
        
        selected_spotidane$maxvalue <- max(y2$n)
        
        if(nrow(y)==0 && nchar(y2)==0) {
          plot.new()
          text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
        }
        else{
          selected_spotidane$hover_read <- TRUE
          dayparts <- paste(rep(weekdays(date("2020-01-20") + 0:6, abbreviate = TRUE), each = 4), c("0:00","6:00", "12:00", "18:00"))
          lackingtimey <-which(!dayparts %in% y[["pora"]])
          lackingtimey2 <- which(!dayparts %in% y2[["pora"]])
          y <- rbind(y, tibble(pora = dayparts[lackingtimey], n = rep(0, length(lackingtimey)), grupa = rep(1, length(lackingtimey))))
          y2 <- rbind(y2, tibble(pora = dayparts[lackingtimey2], n = rep(0, length(lackingtimey2)), grupa = rep(2, length(lackingtimey2))))
          
          
          y <- y[match(dayparts, y$pora),]   #aby ramka danych byla w kolejnosci dni tygodnia - przydatne do tooltipa
          y2 <- y2[match(dayparts, y2$pora),]
          
          toplot <- rbind(y2, y)
          
          p <- ggplot(toplot, aes(x = factor(pora, dayparts), y = n, group = grupa, colour = factor(grupa, 1:2), fill = factor(grupa, 1:2))) +
            geom_point(size = 2) +
            geom_area(data = toplot[toplot$grupa==1,], aes(x = factor(pora, dayparts), y = n, colour = NULL ), alpha = 0.3) +
            geom_area(data = toplot[toplot$grupa==2,], aes(x = factor(pora, dayparts), y = n, colour = NULL), alpha = 0.1) +
            geom_line(colour = c(rep("red", 28), rep("gray", 28))) + #ponizej - guzik do powrotu do pierwszego wykresu
            annotate("text", x = 1.5, y =  selected_spotidane$maxvalue*1.05, label = "bold(Powróć)", parse = TRUE)+
            annotate("segment", x=0, xend = 3, y = selected_spotidane$maxvalue*1.08, yend = selected_spotidane$maxvalue*1.08) +
            annotate("segment", x=0, xend = 3, y = selected_spotidane$maxvalue*1.02, yend = selected_spotidane$maxvalue*1.02) +
            annotate("segment", x=0, xend = 0, y = selected_spotidane$maxvalue*1.08, yend = selected_spotidane$maxvalue*1.02) +
            annotate("segment", x=3, xend = 3, y = selected_spotidane$maxvalue*1.08, yend = selected_spotidane$maxvalue*1.02) +
            theme_minimal()+
            theme(axis.text.x = element_text(hjust = 0.8),
                  legend.position = c(0.85, 0.85),
                  legend.title = element_blank(),
                  plot.caption = element_text(size = 12, hjust = 0.98))+
            labs(title = paste0(selected_spotidane$selected, " - liczba odtworzeń w ciągu tygodnia"),  caption = "1 • •") +
            ylab("Liczba odtworzeń") +
            xlab(element_blank()) +
            scale_x_discrete(breaks = paste(weekdays(date("2020-01-20") + 0:6, abbreviate = TRUE), "12:00"),
                             limits = dayparts,
                             labels = weekdays(date("2020-01-20") + 0:6)) +
            scale_color_manual(values = c("red", "gray"), labels = c("Wybrany zakres dat", "Wszystkie dostępne daty")) +
            scale_fill_manual(values = c("red", "gray"), labels = c("Wybrany zakres dat", "Wszystkie dostępne daty"))
          # annotate("text", x = 25, y =  selected_spotidane$maxvalue*1.05, label = "1 • •", size = 7, fontface = "bold")
          
          
          
          
          ###robienie tooltipa: 
          
          readY <- which(abs(as.vector(toplot[c(round(selected_spotidane$hover[1]),  round(selected_spotidane$hover[1]) + 28), 2]) -
                               selected_spotidane$hover[2]) < selected_spotidane$maxvalue/15)
          
          readX <- ifelse(abs(selected_spotidane$hover[1] - round(selected_spotidane$hover[1]))<0.2, round(selected_spotidane$hover[1]) %% 29, -1)
          if(readX>0 && length(readY)>0){
            rectY <- ifelse(1 %in% readY, toplot[round(selected_spotidane$hover[1]), 2], toplot[round(selected_spotidane$hover[1]) + 28, 2])
            rectY <- as.numeric(rectY)
            if(2 %in% readY){
              # pokazujemy wartosc dla wybranego zakresu
              topartist <- spotidane$data %>%
                filter(endTime >= selected_spotidane$begin_date) %>%
                filter(endTime <= selected_spotidane$end_date) %>%
                filter(artistName == selected_spotidane$selected) %>%
                mutate(pora = zwroc_czas(endTime)) %>%
                filter(pora == as.character(toplot[readX, 1])) %>%
                count(trackName) %>%
                arrange(desc(n)) %>%
                slice(1) %>% pull(trackName)
              selected_spotidane$test <- topartist
              p + annotate("rect", xmin = ifelse(readX>22, 22, readX), ymin = rectY,
                           xmax = ifelse(readX>22, 22, readX)+6, ymax = rectY + selected_spotidane$maxvalue/6, alpha = 0.2) +
                annotate("text", x = ifelse(readX>22, 22, readX)+3, y = rectY + selected_spotidane$maxvalue/12, label = paste("Najczęściej słuchany", "utwór w:",
                                                                                                                              paste0(unlist(strsplit(as.character(toplot[readX, 1]), " "))[1],", ",
                                                                                                                                     unlist(strsplit(unlist(strsplit(as.character(toplot[readX, 1]), " "))[2], ":"))[1], "-",
                                                                                                                                     as.numeric(unlist(strsplit(unlist(strsplit(as.character(toplot[readX, 1]), " "))[2], ":"))[1]) + 6),
                                                                                                                              #unlist(strsplit(unlist(strsplit(as.character(toplot[readX+1, 1]), " "))[2], ":"))[1]),
                                                                                                                              ifelse(nchar(topartist)>20,
                                                                                                                                     paste0(substr(topartist, 1, 18), "..."),
                                                                                                                                     topartist),
                                                                                                                              sep = "\n"))
            }
            else{
              topartist <- spotidane$data %>%
                filter(artistName == selected_spotidane$selected) %>%
                mutate(pora = zwroc_czas(endTime)) %>%
                filter(pora == as.character(toplot[readX, 1])) %>%
                count(trackName) %>%
                arrange(desc(n)) %>%
                slice(1) %>% pull(trackName)
              selected_spotidane$test <- topartist
              p + annotate("rect", xmin = ifelse(readX>22, 22, readX), ymin = rectY,
                           xmax = ifelse(readX>22, 22, readX)+6, ymax = rectY + selected_spotidane$maxvalue/7, alpha = 0.2)+
                annotate("text", x = ifelse(readX>22, 22, readX)+3, y = rectY + selected_spotidane$maxvalue/14, label = paste("Najczęściej słuchany", "utwór w:",
                                                                                                                              paste0(unlist(strsplit(as.character(toplot[readX, 1]), " "))[1],", ",
                                                                                                                                     unlist(strsplit(unlist(strsplit(as.character(toplot[readX, 1]), " "))[2], ":"))[1], "-",
                                                                                                                                     as.numeric(unlist(strsplit(unlist(strsplit(as.character(toplot[readX, 1]), " "))[2], ":"))[1]) + 6),
                                                                                                                              #unlist(strsplit(unlist(strsplit(as.character(toplot[readX+1, 1]), " "))[2], ":"))[1]),                                                                                                                   ifelse(nchar(topartist)>20,
                                                                                                                              ifelse(nchar(topartist)>20,
                                                                                                                                     paste0(substr(topartist, 1, 18), "..."),
                                                                                                                                     topartist),
                                                                                                                              sep = "\n"))
            }
          }
          else{
            p
          }
        }
        
      }
      ###koniec lewego wykresu
      
      
      ##poczatek prawego wykresu Jacy
      else if(which(selected_spotidane$window) == 3){
        df <- spotidane$data %>%
          filter(endTime >= selected_spotidane$begin_date) %>%
          filter(endTime <= selected_spotidane$end_date) %>%
          filter(artistName == selected_spotidane$selected) %>%
          mutate(endTime = as.Date(endTime)) %>%
          group_by(trackName, endTime) %>% summarise(count = length(trackName), time = sum(msPlayed)) %>%
          arrange(desc(time)) %>% ungroup() %>% mutate(trackName = factor(trackName, unique(trackName)))
        temp <- df %>% group_by(trackName) %>% summarise(count = sum(time)) %>% arrange(desc(count)) %>% slice(1:10)
        df <- df %>% filter(trackName %in% temp$trackName)
        
        selected_spotidane$maxvalue[1] <- min(df$endTime)
        selected_spotidane$maxvalue[2] <- max(df$endTime)
        
        if(nrow(df)==0) {
          plot.new()
          text(0.5,0.5,"Wybrany zakres dat nie zwrócił żadnych wyników dla danego pliku")
        }
        else{
          
          ggplot(df, aes(x=endTime, y=trackName, fill = trackName)) +
            geom_joy(scale=2, alpha = 0.8) +
            scale_fill_manual(values=rep(c('#9ecae1', '#3182bd'), length(unique(df$trackName))/2)) +
            scale_y_discrete(expand = c(0.01, 0), labels = function(d){label = ifelse(nchar(d)>17, paste0(substr(d, 1, 17), "..."), d)}) +
            xlab('') +
            theme_joy() +
            labs(title = "Częstość słuchania utworów", caption = "• • 3") +
            theme(legend.position = 'none', axis.title.y = element_blank(),
                  plot.caption = element_text(size = 12)) 
          
          #annotate("text", x = selected_spotidane$end_date, y =  10.5, label = "• • 3", size = 7, fontface = "bold")
        }
      }
      
    }
  })
}# 



update_file_list <- function(input, tags, session, output, spotidane, selected_spotidane, conn, db_table_name){

    user_id <- session$userData$user_id

    file_list_item <- function(x){
          observeEvent(input[[paste0("button", x['id'])]], {
            loadFile(dbGetQuery(conn, str_interp('SELECT content FROM ${db_table_name} WHERE id = ${x["id"]}')), spotidane, selected_spotidane, session)
            output$distPlot <- plotrender(spotidane, selected_spotidane)
          })
    
          observeEvent(input[[paste0("delete", x['id'])]], {
            rs <- dbSendStatement(conn, str_interp('DELETE FROM ${db_table_name} WHERE id = ${x["id"]}'))
            dbClearResult(rs)
            output$file_list <- renderUI({
              update_file_list(input, tags, session, output, spotidane, selected_spotidane, conn, db_table_name)
            })
            # TODO: reload list files
          })
          tags$tr(
            tags$td(
              actionButton(paste0("button", x['id']), str_interp(paste("${x['name']}", "${x['updatedat']} (UTC)", sep="\n")))
            ),
            tags$td(),
            tags$td(
              actionButton(paste0("delete", x['id']), "", icon = icon("trash"))
              )
          )
        }
    
        tags$table(
          apply(dbGetQuery(
            conn, str_interp("SELECT id, updatedat, name FROM ${db_table_name} WHERE userid = '${user_id}'")
          ), 1, function(x) file_list_item(x))
        )
    }

