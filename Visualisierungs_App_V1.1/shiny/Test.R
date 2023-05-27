# Import der benötigten Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(dplyr)
library(plotly)
library(lubridate)
library(data.table)
library(rmarkdown)

#TODO: 
#Plot Ultraschalldaten in Liter?
# abklären wie Energiedaten am besten plotten
# Datetime wird im Plot nicht richtig angezeigt?

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ultraschalldaten", tabName = "ultraschall", icon = icon("dashboard")),
      menuItem("Energiedaten", tabName = "energie", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "ultraschall",
              fluidPage(
                column(
                  width = 12,
                  box(
                    dateRangeInput("daterange_ultraschall", "Zeitraum auswählen:", 
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   format = "dd.mm.yyyy",
                                   language = "de", separator = "bis"),
                    actionButton("submit_ultraschall", "Daten abrufen"),
                    width = 12
                  ),
                  
                  box(
                    plotlyOutput("levelOverTime"),
                    width = 12
                  )
                )
              )
      ),
      tabItem(tabName = "energie",
              fluidPage(
                column(
                  width = 12,
                  box(
                    dateRangeInput("daterange_energie", "Zeitraum auswählen:", 
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   format = "dd.mm.yyyy",
                                   language = "de", separator = "bis"),
                    actionButton("submit_energie", "Daten abrufen"),
                    width = 12
                  ),
                  box(
                    plotlyOutput("energiePlot"),
                    width = 12
                  )
                )

              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Die Absoluten Pfade zu den Ordnern auf dem PC
  ultraschall_folder <- "C:/Users/Lenovo/Documents/Git/Visualisierungs_App_V1.1/Ordnerstruktur/Ultraschalldaten"
  energie_folder <- "C:/Users/Lenovo/Documents/Git/Visualisierungs_App_V1.1/Ordnerstruktur/Energiedaten"
  
 # geteilte Funktionen
  
  # Funktion um alle Dateipfade im angegebenen Zeitraum zu holen
  get_files <- function(start_date, end_date, data_folder) {
    selected_files <- c()
    year_folders <-list.dirs(data_folder, recursive = FALSE) # Liste der Jahresordner
    selected_year_folders <- c()
    
    # Durchsuche die Jahresordner und wähle diejenigen aus, die im angegebenen Zeitraum liegen
    for (year_folder in year_folders) {
      last_four_chars <-substr(year_folder, nchar(year_folder) - 3, nchar(year_folder))
      if (last_four_chars >= (format(start_date, "%Y")) && last_four_chars <= (format(end_date, "%Y"))) {
        selected_year_folders <-c(selected_year_folders, year_folder)  # Jahr zur Liste hinzufügen
      }
      print(selected_year_folders)
    }
    # Durchsuche die Monatsordner in den ausgewählten Jahresordnern und wähle diejenigen aus, die im angegebenen Zeitraum liegen
    for (year_folder in selected_year_folders) {
      #Alle Monatsverzeichnisse, die in den jeweiligen Jahren im Verzeichnis existieren
      month_folders <- list.dirs(year_folder, recursive = FALSE) # Liste der Monatsordner
      selected_month_folders <- c()
      for (month_folder in month_folders) {
        month <- substr(month_folder, nchar(month_folder) - 1, nchar(month_folder))
        if (month >= (format(start_date, "%m")) && month <= format(end_date, "%m")) {
          selected_month_folders <- c(selected_month_folders, month_folder)  # Monat zur Liste hinzufügen
        }
      }
      # Durchsuche die CSV-Dateien in den ausgewählten Monatsordnern und wähle diejenigen aus, die im angegebenen Zeitraum liegen
      for (month_folder in selected_month_folders) {
        file_paths <- list.files(month_folder, pattern = "(?i)\\.csv$", full.names = TRUE)
        for (file in file_paths) {
          date <- substr(file, nchar(file) - 13, nchar(file) - 4)
          if (date >= start_date && date <= end_date) {
            selected_files <- c(selected_files, file)  
          }
        }
      }
    }
    return(selected_files)
  }
  
#Funktionen für Ultraschalldaten
  
  # Funktion zum Abrufen der Ultraschalldaten
  get_ultraschall_data = function(start_date, end_date) {
    ultraschall_data <- list() # Leere Liste zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, ultraschall_folder)
      for (file_path in selected_files) {
        data <-
          read.csv(
            file_path,
            sep = ";",
            header = TRUE,
            stringsAsFactors = FALSE
          )
        ultraschall_data <- c(ultraschall_data, list(data))
      }
    return(do.call(rbind, ultraschall_data))
  }
  
  # Funktion zum Rendern des Level-Over-Time Plots
  renderLevelOverTimePlot <- function(data) {
    p <- plot_ly(
      data,
      type = 'scatter',
      mode = 'lines',
      source = 'trace'
    )
    for(trace in colnames(data)[2:ncol(data)]){
      p <- p %>% plotly::add_trace(x = data$Timestamp, y = data[[trace]], name = trace)
    }
    
    p <- p %>% layout(
      title = sprintf("Füllstand"),
      font = list(size=13),
      xaxis = list(),
      yaxis = list(title = "cm"),
      colorway = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF","#FA6B09FF","#149BEDFF","#A1C720FF","#FEC10BFF","#16A08CFF","#9A703EFF"),
      margin = list(t = 50)
    )
    
    return(p)
  }
  
  # observeEvent-Funktion reagiert auf submit Buttonklick 
  observeEvent(input$submit_ultraschall, {
    start_date <- input$daterange_ultraschall[1]
    end_date <- input$daterange_ultraschall[2]
    
    ultraschall_data <- get_ultraschall_data(start_date, end_date)
    if (length(ultraschall_data) > 0) {
      # Plot für Ultraschalldaten
      output$levelOverTime <- renderPlotly({
        renderLevelOverTimePlot(ultraschall_data)
      })
     
    } else{
      # Keine Dateien gefunden
      shinyalert("Achtung!", "Es wurden keine Ultraschalldaten gefunden!", type = "warning")
    }
  })
  
  # Observer für Änderungen des Startdatums im Tab "ultraschall"
  observeEvent(input$daterange_ultraschall[1], {
    start_date_ultraschall <- input$daterange_ultraschall[1]
    end_date_ultraschall <- input$daterange_ultraschall[2]
    
    # Überprüfen, ob das Startdatum größer als das Enddatum ist
    if (start_date_ultraschall > end_date_ultraschall) {
      # Setzen des Enddatums auf das Startdatum
      updateDateRangeInput(session, "daterange_ultraschall", start = start_date_ultraschall, end = start_date_ultraschall)
    }
  })
  
  
  
  
#Funktionen für Energiedaten:
  
  # Funktion zum Abrufen der Energiedaten
  get_energie_data = function(start_date, end_date) {
    energie_data <- list() # Leere Liste zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, energie_folder)
      for (file_path in selected_files) {
        data <-
          read.csv(
            file_path,
            sep = ";",
            header = TRUE,
            stringsAsFactors = FALSE
          )
        energie_data <- c(energie_data, list(data))
      }
    return(do.call(rbind, energie_data))
  }
  
  renderEnergiePlot <- function(data) {
    p <- plot_ly(
      data,
      type = 'scatter',
      mode = 'lines',
      source = 'trace'
    )
    print(data$DateTime)
    for (trace in c("Energy1")) {
      # /1000 für Kilowattstunden??
      p <- p %>% add_trace(x = data$DateTime, y = ~get(trace)/1000, name = trace)  # Energy-Werte in Kilowattstunden teilen
    }
    
    p <- p %>% layout(
      title = "Energiedaten",
      font = list(size = 13),
      xaxis = list(),
      yaxis = list(title = "kWh"),
      colorway = c("#0C5BB0FF", "#EE0011FF", "#15983DFF"),  # Farben anpassen oder ergänzen
      margin = list(t = 50)
    )
    
    return(p)
  }
  
  # observeEvent-Funktion reagiert auf submit Buttonklick 
  observeEvent(input$submit_energie, {
    start_date <- input$daterange_energie[1]
    end_date <- input$daterange_energie[2]
    
    energie_data <- get_energie_data(start_date, end_date)
    if (length(energie_data) > 0) {
      # Plot für Ultraschalldaten
      output$energiePlot <- renderPlotly({
        renderEnergiePlot(energie_data)
      })
      
    } else{
      # Keine Dateien gefunden
      shinyalert("Achtung!", "Es wurden keine Energiedaten gefunden!", type = "warning")
    }
  })
  
  
  # Observer für Änderungen des Startdatums im Tab "energie"
  observeEvent(input$daterange_energie[1], {
    start_date_energie <- input$daterange_energie[1]
    end_date_energie <- input$daterange_energie[2]
    
    # Überprüfen, ob das Startdatum größer als das Enddatum ist
    if (start_date_energie > end_date_energie) {
      # Setzen des Enddatums auf das Startdatum
      updateDateRangeInput(session, "daterange_energie", start = start_date_energie, end = start_date_energie)
    }
  })
}

shinyApp(ui = ui, server = server)
