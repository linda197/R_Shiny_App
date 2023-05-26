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

#TODO Funktion schreiben, die es nicht erlaubt das Startdatum älter als Enddatum ist

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
                    dateRangeInput("daterange_ultraschall", "Zeitraum auswählen:", start = Sys.Date() - 7, end = Sys.Date()),
                    actionButton("submit_ultraschall", "Daten abrufen"),
                    width = 12
                  ),
                  box(
                    tableOutput("ultraschall_table"),
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
                    dateRangeInput("daterange_energie", "Zeitraum auswählen:", start = Sys.Date() - 7, end = Sys.Date()),
                    actionButton("submit_energie", "Daten abrufen"),
                    width = 12
                  ),
                  box(
                    tableOutput("energie_table"),
                    width = 12
                  )
                )

              )
      )
    )
  )
)


server <- function(input, output) {
  # Die Absoluten Pfade zu den Ordnern auf dem PC
  ultraschall_folder <- "C:/Users/Lenovo/Documents/Uni/6.Semester/Projekt/Ordnerstruktur/Ultraschalldaten"
  energie_folder <- "C:/Users/Lenovo/Documents/Uni/6.Semester/Projekt/Ordnerstruktur/Energiedaten"
  
 
  
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
  
  # Funktion zum Abrufen der Ultraschalldaten
  get_ultraschall_data = function(start_date, end_date) {
    ultraschall_data <- list() # Leere Liste zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, ultraschall_folder)
    if (length(selected_files) > 0) {
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
    } else{
      # Keine Dateien gefunden
      showNotification("Keine Ultraschalldaten gefunden.", type = "warning")
    }
    print(selected_files)
    return(do.call(rbind, ultraschall_data))
  }
  
  # Funktion zum Abrufen der Energiedaten
  get_energie_data = function(start_date, end_date) {
    energie_data <- list() # Leere Liste zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, energie_folder)
    if (length(selected_files) > 0) {
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
    } else{
      # Keine Dateien gefunden
      showNotification("Keine Energiedaten gefunden.", type = "warning")
    }
    print(selected_files)
    return(do.call(rbind, energie_data))
  }
  
  observeEvent(input$submit_ultraschall, {
    start_date <- input$daterange_ultraschall[1]
    end_date <- input$daterange_ultraschall[2]
    
    ultraschall_data <- get_ultraschall_data(start_date, end_date)
    
    # Führe hier weitere Verarbeitungsschritte mit den abgerufenen Daten durch
    output$ultraschall_table <- renderTable(ultraschall_data)
    # Beispiel: Drucke die Anzahl der abgerufenen Datenstrukturen (CSV-Dateien)
    #print(length(ultraschall_data))
  })
  
  observeEvent(input$submit_energie, {
    start_date <- input$daterange_energie[1]
    end_date <- input$daterange_energie[2]
    
    energie_data <- get_energie_data(start_date, end_date)
    
    # Führe hier weitere Verarbeitungsschritte mit den abgerufenen Daten durch
    output$energie_table <- renderTable(energie_data)
    # Beispiel: Drucke die Anzahl der abgerufenen Datenstrukturen (CSV-Dateien)
  })
}


shinyApp(ui = ui, server = server)
