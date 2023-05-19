library(shiny)

ui <- fluidPage(
  titlePanel("Datum-Auswahl"),
  
  sidebarLayout(
    sidebarPanel(
      # Eingabefelder für den Zeitraum
      dateRangeInput("daterange", "Zeitraum auswählen:",
                     start = Sys.Date() - 7, end = Sys.Date()),
      actionButton("submit", "Daten abrufen")
    ),
    
    mainPanel(
      tableOutput("ultraschall_table")
    )
  )
)

server <- function(input, output) {
    Ultraschall_folder <- "C:/Users/Lenovo/Documents/Uni/6.Semester/Projekt/Ordnerstruktur/Ultraschalldaten"
    ultraschall_data <- list() # Leere Liste zum Speichern der abgerufenen Daten
    
    # Funktion um alle Dateipfade im angegebenen Zeitraum zu holen
    get_files <- function(start_date, end_date, data_folder) {
      selected_files <- c()
      year_folders <- list.dirs(data_folder, recursive = FALSE) # Liste der Jahresordner
      selected_year_folders <- c()
        for (year_folder in year_folders){
          last_four_chars <- substr(year_folder, nchar(year_folder) - 3, nchar(year_folder))
          if (last_four_chars>= (format(start_date, "%Y")) && last_four_chars<= (format(end_date, "%Y"))) {
            selected_year_folders <- c(selected_year_folders, year_folder)  # Jahr zur Liste hinzufügen
        }
      }
      for (year_folder in selected_year_folders) {
        #Alle Monatsverzeichnisse, die in den jeweiligen Jahren im Verzeichnis existieren
        month_folders <- list.dirs(year_folder, recursive = FALSE) # Liste der Monatsordner
        selected_month_folders <- c()
        for (month_folder in month_folders){
          month <- substr(month_folder, nchar(month_folder) - 1, nchar(month_folder))
          if (month>= (format(start_date, "%m")) && month<= format(end_date, "%m")) {
            selected_month_folders <- c(selected_month_folders, month_folder)  # Monat zur Liste hinzufügen
          }
        }
        for (month_folder in selected_month_folders) {
          file_paths <- list.files(month_folder, pattern = "\\.csv$", full.names = TRUE) # Dateipfade der CSV-Dateien
          for (file in file_paths){
            date <- substr(file, nchar(file) - 13, nchar(file)-4)
            if (date>= start_date && date<= end_date) {
              selected_files <- c(selected_files, file)  # Datei zur Liste hinzufügen
            }
          }
        }
      }
      return(selected_files)
    }
    
    #Funktion um CSV Dateien der Ultraschalldaten zu holen
    get_ultraschall_data = function(start_date, end_date) {
      selected_files <- get_files(start_date, end_date, Ultraschall_folder)
      if (length(selected_files) > 0) {
        for (file_path in selected_files) {
          data <- read.csv(file_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
          ultraschall_data <- c(ultraschall_data, list(data))
        }
      } else{
        # Keine Dateien gefunden
        print("Keine Ultraschalldaten gefunden.")
        showNotification("Keine Ultraschalldaten gefunden.", type = "warning")
      }
      print(selected_files)
        return(do.call(rbind, ultraschall_data))
    }
    
    
    
    observeEvent(input$submit, {
      start_date <- input$daterange[1]
      end_date <- input$daterange[2]
      
      ultraschall_data <- get_ultraschall_data(start_date, end_date)
      
      # Führe hier weitere Verarbeitungsschritte mit den abgerufenen Daten durch
      output$ultraschall_table <- renderTable(ultraschall_data)
      # Beispiel: Drucke die Anzahl der abgerufenen Datenstrukturen (CSV-Dateien)
      #print(length(ultraschall_data))
    })
  }
  

shinyApp(ui = ui, server = server)
