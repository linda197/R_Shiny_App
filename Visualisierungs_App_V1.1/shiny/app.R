
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

# Erstellung des Userinterfaces
# Unterteilung in Header, Sidebar und Body
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Auswertung", tabName = "vis", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "vis",
              fluidPage(
                useShinyalert(),
                column(
                  # File-Upload für .csv's der  Drainsensoren
                  box(fileInput("csvDrainData","Bitte .csv der Drainsensoren auswählen",accept = ".csv",multiple = TRUE),width = 12),
                  # File-Upload für .csv's der  Ultraschallsensoren
                  box(fileInput("csvUsData","Bitte .csv der Ultraschallsensoren auswählen",accept = ".csv",multiple = TRUE),width = 12),
                  # Dropdown - Menüs für Auswahl des darzustellendes Zeitraums
                  box(uiOutput("dateSelect"),width = 12),
                  # Radiobuttons zur Auswahl der Auswertemethode
                  box(radioGroupButtons(inputId = "method",label = "Auswertungsmethode",choices = c("minütlich","stündlich", "täglich"),justified = TRUE,selected = "stündlich"),width = 12),
                  # Radiobuttons zur Auswahl der Einheiten
                  box(radioGroupButtons(inputId = "unit",label = "Einheit",choices = c("Milliliter", "Liter"),justified = TRUE),width = 12),
                  # Plot für Drainsensordaten
                  box(plotlyOutput(("volOverTime")),width = 12),
                  # Plot für Ultraschallsensordaten
                  box(plotlyOutput(("levelOverTime")),width = 12),
                  box(tags$div(downloadButton("downloadCsvData", "Download CSV"),style="margin-right:12px;",style="display:inline-block;"),downloadButton("downloadHtmlReport", "Download HTML"),width = 12)
                  ,width = 12
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Funktion, um den n-ten Wert von n+1-ten Wert abzuziehen
  calcDiff <- function(x){
    shiftedX <- x %>% mutate_all(lag)
    shiftedX[1,] <- x[1,]
    x[,2:ncol(x)] <- x[,2:ncol(x)] - shiftedX[,2:ncol(shiftedX)]
    x[x<=0] <- 0
    return(x)
  }
  
  # Funktion, um fehlerhaft - formattierte csv - Dateien korrekt einzulesen
  readMalFormCsv <- function(x){
    cleanData <- readChar(x,file.info(x)$size)
    cleanData <- gsub(';\r\n', '\r\n', cleanData)
    cleanData <- gsub(';\n', '\n', cleanData)
    return (cleanData)
  }
  
  # Funktion um zu prüfen, ob Draindaten hochgeladen wurden
  getDrainData = function() {
    data = tryCatch({
      uploadDrainData()
    }, error=function(e){
      NULL
    })
    return(data)
  }
  
  # Funktion um zu prüfen, ob Ultraschalldaten hochgeladen wurden
  getUsData = function() {
    data = tryCatch({
      uploadUsData()
    }, error=function(e){
      NULL
    })
    return(data)
  }
  
  # Reaktive Variable, die die csv-Datein der Drainsensoren enthält
  uploadDrainData <- reactive({
    req(input$csvDrainData)
    cleanData <- lapply(input$csvDrainData$datapath,readMalFormCsv)
    df <- rbindlist(lapply(lapply(cleanData, fread,dec=","),calcDiff), use.names = TRUE, fill = TRUE)
    df[is.na(df)] <- 0
    setDF(df)
    df$Timestamp <- as.POSIXct(df$Timestamp,format = "%d.%M.%Y %H:%M")
    for(cnt in 2:ncol(df)){
      df[,cnt] <- as.numeric(df[,cnt])
    }
    uploadDrainData <- df

  })
  
  # Reaktive Variable, die die csv-Datein der Ultraschallsensoren enthält
  uploadUsData <- reactive({
    req(input$csvUsData)
    df <- rbindlist(lapply(input$csvUsData$datapath, fread,dec=","), use.names = TRUE, fill = TRUE)
    setDF(df)
    for(cnt in 2:ncol(df)){
      df[,cnt] <- as.numeric(df[,cnt])
    }
    uploadUsData <- df
  })
  
  # Reaktive Vorverarbeitung der Rohdaten um sie anschließend zu plotten
  levelPlot <- reactive({
    df <- uploadUsData()
    req(input$beginDay)
    req(input$endDay)
    if(input$beginDay != as.Date(min(df[,1])) | input$beginDay != as.Date(max(df[,1]))){
      shinyalert("Achtung!", "Die Zeiträume der csv - Dateien der Drain und Ultraschall Sensoren stimmen nicht überein!", type = "warning")
    }
    df <- df[date(df$Timestamp)>=input$beginDay & date(df$Timestamp)<=input$endDay,]
    
    df[2:ncol(df)] <- 98.5 - (df[2:ncol(df)] / 10)
    levelPlot <- df
  })
  
  # Reaktive Vorverarbeitung der Rohdaten um sie anschließend zu plotten
  drainPlot <- reactive({
    df <- uploadDrainData()
    req(input$beginDay)
    req(input$endDay)
    if(as.Date(input$beginDay)>as.Date(input$endDay)){
      shinyalert("Achtung!", "Das gewählte Startdatum liegt hinter dem Enddatum!", type = "warning")
    }
    df <- df[date(df$Timestamp)>=input$beginDay & date(df$Timestamp)<=input$endDay,]
    
    if(input$unit =="Liter"){
      df[,2:ncol(df)] <- df[,2:ncol(df)]/1000
    }else{
    }
    
    if(input$method == "stündlich"){
      df$hour <- hour(df$Timestamp)
      df$date <- date(df$Timestamp)
      drainPlot <- df %>% group_by(date,hour) %>% summarise(across(2:(ncol(df)-2), sum))
      drainPlot$Timestamp <- as_datetime(sprintf("%s %s:00:00",as.character(drainPlot$date),as.character(drainPlot$hour))) # create timestamps
      drainPlot <- as.data.frame(drainPlot)
      drainPlot <- drainPlot %>% mutate(hour = NULL,date=NULL) %>% relocate(Timestamp) # clean Dataframe for plotting
    }else if(input$method == "täglich"){
      df$Timestamp <- date(df$Timestamp)
      dataPlot <- df %>% group_by(Timestamp) %>% summarise(across(2:(ncol(df)-1), sum))
    }else if(input$method == "minütlich"){
      dataPlot <- df
    }
  })
  
  # Rendern der Dropdownmenüs zur Datum-Auswahl
  output$dateSelect = renderUI({
    drainData = getDrainData()
    usData = getUsData()
    df = FALSE
    if(!is.null(drainData) & !is.null(usData)) {
      df = c(drainData[,1], usData[,1])
    } else if (!is.null(usData)) {
       df = usData[,1]
    } else if (!is.null(drainData)) {
       df = drainData[,1]
    }
    req(df)
    csvDates <- unique(date(df))
    csvDates <- csvDates[order(csvDates)]
    tagList(
      selectInput("beginDay","Bitte Start-Tag auswählen",choices = csvDates,selected = min(csvDates)),
      selectInput("endDay","Bitte End-Tag auswählen",choices = csvDates,selected = max(csvDates))
    )

  })
  
  # Plot für Draindaten
  output$volOverTime = renderPlotly({
    dataContainer <- drainPlot()
    p <- plot_ly(
      dataContainer,
      type = 'bar',
      source = "trace"
    )
    for(trace in colnames(dataContainer)[2:ncol(dataContainer)]){
      p <- p %>% plotly::add_trace(x = dataContainer$Timestamp, y = dataContainer[[trace]], name = trace)
    }
    p <- p %>%layout(
      title = sprintf("Drain - %s",input$method),
      font = list(size=13),
      xaxis = list(),
      yaxis = list(title = input$unit),
      colorway = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF","#FA6B09FF","#149BEDFF","#A1C720FF","#FEC10BFF","#16A08CFF","#9A703EFF" ),
      margin = list(t = 50)
    )
    p
  })
  
  # Plot für Ultraschalldaten
  output$levelOverTime = renderPlotly({
    dataContainer <- levelPlot()
    p <- plot_ly(
      dataContainer,
      type = 'scatter',
      mode = 'lines',
      source = 'trace'
    )
    for(trace in colnames(dataContainer)[2:ncol(dataContainer)]){
      p <- p %>% plotly::add_trace(x = dataContainer$Timestamp, y = dataContainer[[trace]], name = trace)
    }
    p <- p %>%layout(
      title = sprintf("Füllstand"),
      font = list(size=13),
      xaxis = list(),
      yaxis = list(title = "cm"),
      colorway = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF","#FA6B09FF","#149BEDFF","#A1C720FF","#FEC10BFF","#16A08CFF","#9A703EFF" ),
      margin = list(t = 50)
    )
    p
    
  })
  
  # Downloadfunktion CSV
  output$downloadCsvData <- downloadHandler(
    filename = function() {
      beginDay = as.character(input$beginDay)
      endDay  = as.character(input$endDay)
      name = sprintf("%s_%s_Report",beginDay,endDay)
      paste(name, sep = '.','csv')
    },
    content = function(file) {
      write.csv2(drainPlot(), file, row.names = FALSE)
    }
  )
  
  # Downloadfunktion HTML
  output$downloadHtmlReport <- downloadHandler(
    filename = function() {
      beginDay = as.character(input$beginDay)
      endDay  = as.character(input$endDay)
      name = sprintf("%s_%s_Report",beginDay,endDay)
      paste(name, sep = '.','html')
    },
    content = function(file) {
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      out <- render('report.Rmd', html_document(), 
              params = list(drn = drainPlot(), unit = input$unit, method = input$method, bgDay = input$beginDay, endDay = input$endDay))
      file.rename(out, file)
    }
  )
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
