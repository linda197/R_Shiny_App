library(shiny)

ui <- fluidPage(
  titlePanel("Datum-Auswahl"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date", "Startdatum:", value = Sys.Date()),
      dateInput("end_date", "Enddatum:", value = Sys.Date())
    ),
    
    mainPanel(
      verbatimTextOutput("selected_dates")
    )
  )
)

server <- function(input, output) {
  output$selected_dates <- renderPrint({
    start_date <- input$start_date
    end_date <- input$end_date
    
    list(
      "Startdatum" = start_date,
      "Enddatum" = end_date
    )
  })
}

shinyApp(ui = ui, server = server)
