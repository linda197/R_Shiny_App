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
library(writexl)
library(shinyjs)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ultraschalldaten", tabName = "ultrasound", icon = icon("dashboard")),
      menuItem("Energiedaten", tabName = "energy", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "ultrasound",
              fluidPage(
                column(
                  width = 12,
                  box(
                    title = "Ultraschalldaten",
                    status = "primary",  
                    solidHeader = TRUE, 
                    dateRangeInput("daterange_ultrasound", "Zeitraum auswählen:", 
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   format = "dd.mm.yyyy",
                                   language = "de", separator = "bis"),
                    actionButton("submit_ultrasound", "Daten abrufen"),
                    downloadButton("export_button_ultrasound", "Daten exportieren"),
                    width = 12
                  ),
                  box(
                    title = "Kalibrierung",
                    status = "primary",  
                    actionButton("run_calibration_button", "Kalibrierung ausführen"),
                    width = 12
                  ),
                  box(
                    plotlyOutput("level_over_time"),
                    width = 12
                  ),
                  box(
                    plotlyOutput("volume_over_time"),
                    width = 12
                  )
                )
              )
      ),
      tabItem(tabName = "energy",
              fluidPage(
                column(
                  width = 12,
                  box(
                    title = "Energiedaten",
                    status = "primary",  
                    solidHeader = TRUE, 
                    dateRangeInput("daterange_energy", "Zeitraum auswählen:", 
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   format = "dd.mm.yyyy",
                                   language = "de", separator = "bis"),
                    actionButton("submit_energy", "Daten abrufen"),
                    downloadButton("export_button_energy", "Daten exportieren"),
                    width = 12
                  ),
                  box(
                    plotlyOutput("energy_over_time"),
                    width = 12
                  )
                ),
              )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # Die Absoluten Pfade zu den Ordnern auf dem PC
  ultrasound_folder <- "C:/Users/Lenovo/Documents/Git/Visualisierungs_App_V1.1/Ordnerstruktur/Ultraschalldaten"
  energy_folder <- "C:/Users/Lenovo/Documents/Git/Visualisierungs_App_V1.1/Ordnerstruktur/Energiedaten"
  
  # Export Button soll bei Initialisierung ausgegraut sein
  shinyjs::disable("export_button_ultrasound")
  shinyjs::disable("export_button_energy")
  
  
  
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
    return(selected_files)
  }
  
###--------------------------------------------------------------------------------------------------------------------------- 
#Funktionen für Ultraschalldaten
  
  # Funktion zum Abrufen der Ultraschalldaten
  get_ultrasound_data <- function(start_date, end_date) {
    ultrasound_data <- data.frame() # Leerer Datenrahmen zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, ultrasound_folder)
    for (file_path in selected_files) {
      data <- read.csv(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      ultrasound_data <- rbind(ultrasound_data, data)
    }
    return(ultrasound_data)
  }
  
  # Funktion zum Rendern des Level-Over-Time Plots
  render_level_over_time_plot <- function(data) {
    p <- plot_ly(
      data,
      type = 'scatter',
      mode = 'lines',
      source = 'trace'
    )
    p <- p %>% plotly::add_trace(x = data$DateTime, y = data$Wasserhoehe_1, name = "Sensor 1")
    p <- p %>% plotly::add_trace(x = data$DateTime, y = data$Wasserhoehe_2, name = "Sensor 2")
    
    p <- p %>% layout(
      title = "Füllstand",
      font = list(size=13),
      xaxis = list(),
      yaxis = list(title = "mm"),
      colorway = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF","#FA6B09FF","#149BEDFF","#A1C720FF","#FEC10BFF","#16A08CFF","#9A703EFF"),
      margin = list(t = 50)
    )
    
    return(p)
  }
  
  
  # Funktion zum Rendern des Volume-Over-Time Plots
  render_volume_over_time_plot <- function(data) {
    p <- plot_ly(
      data,
      type = 'scatter',
      mode = 'lines',
      source = 'trace'
    )
    p <- p %>% plotly::add_trace(x = data$DateTime, y = data$Wassermenge_1, name = "Sensor 1")
    p <- p %>% plotly::add_trace(x = data$DateTime, y = data$Wassermenge_2, name = "Sensor 2")
    
    p <- p %>% layout(
      title = "Wasservolumen",
      font = list(size=13),
      xaxis = list(),
      yaxis = list(title = "Liter"),
      colorway = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF","#FA6B09FF","#149BEDFF","#A1C720FF","#FEC10BFF","#16A08CFF","#9A703EFF"),
      margin = list(t = 50)
    )
    
    return(p)
  }
  
  
  # observeEvent-Funktion reagiert auf submit Button
  observeEvent(input$submit_ultrasound, {
    start_date <- input$daterange_ultrasound[1]
    end_date <- input$daterange_ultrasound[2]
    ultrasound_data <- get_ultrasound_data(start_date, end_date)
    
    # Falls Daten vorhanden sind:
    if (nrow(ultrasound_data) > 0) {
      # Plot für Ultraschalldaten
       ultrasound_data$DateTime <- as.POSIXct(paste(ultrasound_data$Datum, ultrasound_data$Uhrzeit), format = "%Y-%m-%d %H:%M")
       ultrasound_data <- ultrasound_data[, c("DateTime", "Abstand_Sensor_1", "Abstand_Sensor_2", "Wasserhoehe_1", "Wasserhoehe_2", "Wassermenge_1", "Wassermenge_2")]
       output$level_over_time <- renderPlotly({
         render_level_over_time_plot(ultrasound_data)
      })
       output$volume_over_time <- renderPlotly({
         render_volume_over_time_plot(ultrasound_data)
       })
      # Enable export button
      shinyjs::enable("export_button_ultrasound")
      
    } else{
      # Keine Daten gefunden
      shinyalert("Achtung!", "Es wurden keine Ultraschalldaten im ausgewähltem Zeitraum gefunden!", type = "warning")
      shinyjs::disable("export_button_ultrasound")
    }
  })
  
  # Observer für Änderungen des Startdatums
  observeEvent(input$daterange_ultrasound[1], {
    start_date_ultrasound <- input$daterange_ultrasound[1]
    end_date_ultrasound <- input$daterange_ultrasound[2]
    
    # Überprüfe, ob das Startdatum älter als das Enddatum ist
    if (start_date_ultrasound > end_date_ultrasound) {
      # Setzen des Enddatums auf das Startdatum
      updateDateRangeInput(session, "daterange_ultrasound", start = start_date_ultrasound, end = start_date_ultrasound)
    }
  })

  #Download als Excel-Datei
  output$export_button_ultrasound <- downloadHandler(
    filename = function() {
      start_date <- as.character(input$daterange_ultrasound[1])
      end_date <- as.character(input$daterange_ultrasound[2])
      name <- sprintf("%s_%s_Ultraschall_Report", start_date, end_date)
      paste(name, sep = '.', 'xlsx')
    },
    content = function(file) {
      start_date <- input$daterange_ultrasound[1]
      end_date <- input$daterange_ultrasound[2]
      data <- get_ultrasound_data(start_date, end_date)
        write_xlsx(data, path = file)
    }
  )
  
  # Kalibrierung der Ultraschalldaten
  observeEvent(input$run_calibration_button, {
    tryCatch({
      source("C:/Users/Lenovo/Documents/Git/us_calibrate.R")
      showNotification("Die Kalibration wurde erfolgreich ausgeführt", type = "message")
    }, error = function(e) {
      showNotification(paste("Die Kalibrierung konnte nicht ausgeführt werden. Fehlermeldung: ", e$message), type = "error")
    })
  })
  
  
  
###---------------------------------------------------------------------------------------------------------------------------  
#Funktionen für Energiedaten:
  
  # Funktion zum Abrufen der Energiedaten
  get_energy_data <- function(start_date, end_date) {
    energy_data <- data.frame() # Leerer Datenrahmen zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, energy_folder)
    for (file_path in selected_files) {
      data <- read.csv(file_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
      energy_data <- rbind(energy_data, data)
    }
    return(energy_data)
  }
  
  # Funktion zum Rendern des Energie Plots
  render_energy_over_time_plot <- function(data) {
    p <- plot_ly(
      data,
      type = 'scatter',
      mode = 'lines',
      source = 'trace'
    )
    energy_columns <- grep("^Energy", colnames(data), value = TRUE)  # Spalten mit Energy-Werten auswählen
    
    for (energy_col in energy_columns) {
      visible <- ifelse(energy_col == "Energy1", TRUE, "legendonly")  # Festlegen, welcher Trace standardmäßig angezeigt wird
      # Werte in kWh umwandeln => /1000
      p <- p %>% plotly::add_trace(x = data$DateTime, y = data[[energy_col]]/1000, name = energy_col, visible = visible)
    }
    
    p <- p %>% layout(
      title = "Energie",
      font = list(size = 13),
      xaxis = list(),
      yaxis = list(title = "kWh"),
      colorway = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF","#FA6B09FF","#149BEDFF","#A1C720FF","#FEC10BFF","#16A08CFF","#9A703EFF"),
      margin = list(t = 50)
    )
    return(p)
  }
  
  
  # observeEvent-Funktion reagiert auf submit button
  observeEvent(input$submit_energy, {
    start_date <- input$daterange_energy[1]
    end_date <- input$daterange_energy[2]
    
    energy_data <- get_energy_data(start_date, end_date)
    if (nrow(energy_data) > 0) {
      energy_data$DateTime <- as.POSIXct(energy_data$DateTime, format = "%d.%m.%Y %H:%M")
      # Plot für Energiedaten
      output$energy_over_time <- renderPlotly({
        render_energy_over_time_plot(energy_data)
      })
      shinyjs::enable("export_button_energy")
    } else{
      # Keine Dateien gefunden
      shinyalert("Achtung!", "Es wurden keine Energiedaten im ausgewähltem Zeitraum gefunden!", type = "warning")
      shinyjs::disable("export_button_energy")
    }
  })
  
  
  # Observer für Änderungen des Startdatums
  observeEvent(input$daterange_energy[1], {
    start_date_energy <- input$daterange_energy[1]
    end_date_energy <- input$daterange_energy[2]
    
    # Überprüfen, ob das Startdatum größer als das Enddatum ist
    if (start_date_energy > end_date_energy) {
      # Setzen des Enddatums auf das Startdatum
      updateDateRangeInput(session, "daterange_energy", start = start_date_energy, end = start_date_energy)
    }
  })

  #Download als Excel-Datei
  output$export_button_energy <- downloadHandler(
    filename = function() {
      start_date <- as.character(input$daterange_energy[1])
      end_date <- as.character(input$daterange_energy[2])
      name <- sprintf("%s_%s_Energie_Report", start_date, end_date)
      paste(name, sep = '.', 'xlsx')
    },
    content = function(file) {
      start_date <- input$daterange_energy[1]
      end_date <- input$daterange_energy[2]
      data <- get_energy_data(start_date, end_date)
      write_xlsx(data, path = file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
