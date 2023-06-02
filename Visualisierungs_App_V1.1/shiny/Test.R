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
      menuItem("Ultraschalldaten", tabName = "ultraschall", icon = icon("dashboard")),
      menuItem("Energiedaten", tabName = "energie", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "ultraschall",
              fluidPage(
                column(
                  width = 12,
                  box(
                    title = "Ultraschalldaten",
                    status = "primary",  
                    solidHeader = TRUE, 
                    dateRangeInput("daterange_ultraschall", "Zeitraum auswählen:", 
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   format = "dd.mm.yyyy",
                                   language = "de", separator = "bis"),
                    actionButton("submit_ultraschall", "Daten abrufen"),
                    downloadButton("export_button_ultraschall", "Daten exportieren"),
                    width = 12
                  ),
                  
                  box(
                    plotlyOutput("levelOverTime"),
                    width = 12
                  ),
                  box(
                    plotlyOutput("volumeOverTime"),
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
                    title = "Energiedaten",
                    status = "primary",  
                    solidHeader = TRUE, 
                    dateRangeInput("daterange_energie", "Zeitraum auswählen:", 
                                   start = Sys.Date() - 7, end = Sys.Date(),
                                   format = "dd.mm.yyyy",
                                   language = "de", separator = "bis"),
                    actionButton("submit_energie", "Daten abrufen"),
                    downloadButton("export_button_energie", "Daten exportieren"),
                    width = 12
                  ),
                  box(
                    plotlyOutput("energiePlot"),
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
  ultraschall_folder <- "C:/Users/Lenovo/Documents/Git/Visualisierungs_App_V1.1/Ordnerstruktur/Ultraschalldaten"
  energie_folder <- "C:/Users/Lenovo/Documents/Git/Visualisierungs_App_V1.1/Ordnerstruktur/Energiedaten"
  
  # Export Button soll bei Initialisierung ausgegraut sein
  shinyjs::disable("export_button_ultraschall")
  shinyjs::disable("export_button_energie")
  
 # geteilte Funktionen
  
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
  
  
#Funktionen für Ultraschalldaten
  
  # Funktion zum Abrufen der Ultraschalldaten
  get_ultraschall_data <- function(start_date, end_date) {
    ultraschall_data <- data.frame() # Leerer Datenrahmen zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, ultraschall_folder)
    for (file_path in selected_files) {
      data <- read.csv(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      ultraschall_data <- rbind(ultraschall_data, data)
    }
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
  renderVolumeOverTimePlot <- function(data) {
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
  observeEvent(input$submit_ultraschall, {
    start_date <- input$daterange_ultraschall[1]
    end_date <- input$daterange_ultraschall[2]
    ultraschall_data <- get_ultraschall_data(start_date, end_date)
    
    # Falls Daten vorhanden sind:
    if (nrow(ultraschall_data) > 0) {
      # Plot für Ultraschalldaten
       ultraschall_data$DateTime <- as.POSIXct(paste(ultraschall_data$Datum, ultraschall_data$Uhrzeit), format = "%Y-%m-%d %H:%M")
       ultraschall_data <- ultraschall_data[, c("DateTime", "Abstand_Sensor_1", "Abstand_Sensor_2", "Wasserhoehe_1", "Wasserhoehe_2", "Wassermenge_1", "Wassermenge_2")]
       output$levelOverTime <- renderPlotly({
         renderLevelOverTimePlot(ultraschall_data)
      })
       output$volumeOverTime <- renderPlotly({
         renderVolumeOverTimePlot(ultraschall_data)
       })
      # Enable export button
      shinyjs::enable("export_button_ultraschall")
      
    } else{
      # Keine Daten gefunden
      shinyalert("Achtung!", "Es wurden keine Ultraschalldaten im ausgewähltem Zeitraum gefunden!", type = "warning")
      shinyjs::disable("export_button_ultraschall")
    }
  })
  
  # Observer für Änderungen des Startdatums
  observeEvent(input$daterange_ultraschall[1], {
    start_date_ultraschall <- input$daterange_ultraschall[1]
    end_date_ultraschall <- input$daterange_ultraschall[2]
    
    # Überprüfe, ob das Startdatum älter als das Enddatum ist
    if (start_date_ultraschall > end_date_ultraschall) {
      # Setzen des Enddatums auf das Startdatum
      updateDateRangeInput(session, "daterange_ultraschall", start = start_date_ultraschall, end = start_date_ultraschall)
    }
  })

  #Downloadfunktion Excel-Datei
  output$export_button_ultraschall <- downloadHandler(
    filename = function() {
      start_date <- as.character(input$daterange_ultraschall[1])
      end_date <- as.character(input$daterange_ultraschall[2])
      name <- sprintf("%s_%s_Report", start_date, end_date)
      paste(name, sep = '.', 'xlsx')
    },
    content = function(file) {
      start_date <- input$daterange_ultraschall[1]
      end_date <- input$daterange_ultraschall[2]
      data <- get_ultraschall_data(start_date, end_date)
        write_xlsx(data, path = file)
    }
  )
  
  
  
  
#Funktionen für Energiedaten:
  
  # Funktion zum Abrufen der Energiedaten
  get_energie_data <- function(start_date, end_date) {
    energie_data <- data.frame() # Leerer Datenrahmen zum Speichern der abgerufenen Daten
    selected_files <- get_files(start_date, end_date, energie_folder)
    for (file_path in selected_files) {
      data <- read.csv(file_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
      energie_data <- rbind(energie_data, data)
    }
    energie_data$DateTime <- as.POSIXct(energie_data$DateTime, format = "%d.%m.%Y %H:%M")
    return(energie_data)
  }
  
  # Funktion zum Rendern des Energie Plots
  renderEnergiePlot <- function(data) {
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
  observeEvent(input$submit_energie, {
    start_date <- input$daterange_energie[1]
    end_date <- input$daterange_energie[2]
    
    energie_data <- get_energie_data(start_date, end_date)
    if (nrow(energie_data) > 0) {
      # Plot für Energiedaten
      output$energiePlot <- renderPlotly({
        renderEnergiePlot(energie_data)
      })
      shinyjs::enable("export_button_energie")
    } else{
      # Keine Dateien gefunden
      shinyalert("Achtung!", "Es wurden keine Energiedaten im ausgewähltem Zeitraum gefunden!", type = "warning")
      shinyjs::disable("export_button_energie")
    }
  })
  
  
  # Observer für Änderungen des Startdatums
  observeEvent(input$daterange_energie[1], {
    start_date_energie <- input$daterange_energie[1]
    end_date_energie <- input$daterange_energie[2]
    
    # Überprüfen, ob das Startdatum größer als das Enddatum ist
    if (start_date_energie > end_date_energie) {
      # Setzen des Enddatums auf das Startdatum
      updateDateRangeInput(session, "daterange_energie", start = start_date_energie, end = start_date_energie)
    }
  })

  #Downloadfunktion Excel-Datei
  output$export_button_energie <- downloadHandler(
    filename = function() {
      start_date <- as.character(input$daterange_energie[1])
      end_date <- as.character(input$daterange_energie[2])
      name <- sprintf("%s_%s_Energie_Report", start_date, end_date)
      paste(name, sep = '.', 'xlsx')
    },
    content = function(file) {
      start_date <- input$daterange_energie[1]
      end_date <- input$daterange_energie[2]
      data <- get_energie_data(start_date, end_date)
      write_xlsx(data, path = file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
