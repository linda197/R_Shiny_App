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
# Ultraschalldaten in Liter und mm
# Energiedaten Energy 1-4
# Export Button
#Formel für mm US 


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
  
  # # Funktion um alle Dateipfade im angegebenen Zeitraum zu holen
  # get_files <- function(start_date, end_date, data_folder) {
  #   selected_files <- c()
  #   year_folders <-list.dirs(data_folder, recursive = FALSE) # Liste der Jahresordner
  #   selected_year_folders <- c()
  #   
  #   # Durchsuche die Jahresordner und wähle diejenigen aus, die im angegebenen Zeitraum liegen
  #   for (year_folder in year_folders) {
  #     last_four_chars <-substr(year_folder, nchar(year_folder) - 3, nchar(year_folder))
  #     if (last_four_chars >= (format(start_date, "%Y")) && last_four_chars <= (format(end_date, "%Y"))) {
  #       selected_year_folders <-c(selected_year_folders, year_folder)  # Jahr zur Liste hinzufügen
  #     }
  #   }
  #   # Durchsuche die Monatsordner in den ausgewählten Jahresordnern und wähle diejenigen aus, die im angegebenen Zeitraum liegen
  #   for (year_folder in selected_year_folders) {
  #     print(year_folder)
  #     #Alle Monatsverzeichnisse, die in den jeweiligen Jahren im Verzeichnis existieren
  #     month_folders <- list.dirs(year_folder, recursive = FALSE) # Liste der Monatsordner
  #     selected_month_folders <- c()
  #     for (month_folder in month_folders) {
  #       month <- substr(month_folder, nchar(month_folder) - 1, nchar(month_folder))
  #       if (month >= (format(start_date, "%m")) && month <= format(end_date, "%m")) {
  #         selected_month_folders <- c(selected_month_folders, month_folder)  # Monat zur Liste hinzufügen
  #       }
  #     }
  #     
  #     # Durchsuche die CSV-Dateien in den ausgewählten Monatsordnern und wähle diejenigen aus, die im angegebenen Zeitraum liegen
  #     for (month_folder in selected_month_folders) {
  #       print(month_folder)
  #       file_paths <- list.files(month_folder, pattern = "(?i)\\.csv$", full.names = TRUE)
  #       for (file in file_paths) {
  #         date <- substr(file, nchar(file) - 13, nchar(file) - 4)
  #         if (date >= start_date && date <= end_date) {
  #           selected_files <- c(selected_files, file)  
  #         }
  #       }
  #     }
  #   }
  #   
  #   #print(selected_files)
  #   return(selected_files)
  # }
  
  # Funktion um alle Dateipfade im angegebenen Zeitraum zu holen
  get_files <- function(start_date, end_date, data_folder) {
    selected_files <- c()
    file_paths <- list.files(data_folder, pattern = "(?i)\\.csv$", full.names = TRUE, recursive = TRUE)
    for (file_path in file_paths) {
      date <- as.Date(substr(file_path, nchar(file_path) - 13, nchar(file_path) - 4), format = "%Y-%m-%d")
      if (date >= start_date && date <= end_date) {
        selected_files <- c(selected_files, file_path)
      }
    }
    print(selected_files)
    return(selected_files)
  }
  
  
#Funktionen für Ultraschalldaten
  
  # Funktion zum Abrufen der Ultraschalldaten
  get_ultraschall_data <- function(start_date, end_date) {
    ultraschall_data <- data.frame() # Leerer Datenrahmen zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, ultraschall_folder)
    for (file_path in selected_files) {
      data <- read.csv(file_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
      ultraschall_data <- rbind(ultraschall_data, data)
    }
    ultraschall_data$Timestamp <- as.POSIXct(ultraschall_data$Timestamp, format = "%d.%m.%Y %H:%M")
    return(ultraschall_data)
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
      yaxis = list(title = "mm"),
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
    print(length(ultraschall_data))
    if (nrow(ultraschall_data) > 0) {
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
  get_energie_data <- function(start_date, end_date) {
    energie_data <- data.frame() # Leerer Datenrahmen zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, energie_folder)
    for (file_path in selected_files) {
      data <- read.csv(file_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      energie_data <- rbind(energie_data, data)
    }
    energie_data$DateTime <- as.POSIXct(energie_data$DateTime, format = "%d.%m.%Y %H:%M")
    return(energie_data)
  }
  
  # Funktion zum Rendern des Energie-Over-Time Plots
  renderEnergiePlot <- function(data) {
    p <- plot_ly(
      data,
      type = 'scatter',
      mode = 'lines',
      source = 'trace'
    )
    for (i in 2:5) {
      energy_col <- paste0("Energy", i)
      p <- p %>% plotly::add_trace(x = data$DateTime, y = data[[energy_col]], name = energy_col)
    }
    
    p <- p %>% layout(
      title = "Energie",
      font = list(size = 13),
      xaxis = list(),
      yaxis = list(title = "kWh"),
      colorway = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF"),
      margin = list(t = 50)
    )
    
    return(p)
  }
  
  
  # observeEvent-Funktion reagiert auf submit Buttonklick 
  observeEvent(input$submit_energie, {
    start_date <- input$daterange_energie[1]
    end_date <- input$daterange_energie[2]
    
    energie_data <- get_energie_data(start_date, end_date)
    if (nrow(energie_data) > 0) {
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
