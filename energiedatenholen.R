# Installiere das Paket 'httr', falls es noch nicht installiert ist
if (!require("httr")) {
  install.packages("httr")
}

library(httr)

# Definiere die URL der CSV-Datei
url <- "150.100.101.122"

# Funktion zum Abrufen der CSV-Datei über das Netzwerk und Speichern
retrieve_csv <- function(url, file_path) {
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- content(response, "text/csv")
    writeLines(content, file_path)
    print("CSV-Datei erfolgreich abgerufen und gespeichert.")
  } else {
    print("Fehler beim Abrufen der CSV-Datei.")
  }
}

# Funktion zum Erstellen des Ordnerpfads basierend auf Jahr, Monat und Tag
create_folder_path <- function(year, month, day) {
  file.path("/Pfad/zum/Hauptordner", year, month, paste0(day, ".csv"))
}

# Funktion zum Berechnen der Wartezeit bis zur nächsten Mitternacht
calculate_sleep_time <- function() {
  current_time <- Sys.time()
  midnight <- as.POSIXlt(as.Date(current_time)) + 86400  # 86400 Sekunden in einem Tag
  sleep_time <- difftime(midnight, current_time, units = "secs")
  as.numeric(sleep_time)
}

# Wartezeit bis zur nächsten Mitternacht berechnen
sleep_time <- calculate_sleep_time()

# Warte bis zur nächsten Mitternacht
Sys.sleep(sleep_time)

# Aktuelles Datum abrufen
current_date <- Sys.Date()

# Ordnerpfad basierend auf Jahr, Monat und Tag erstellen
year <- format(current_date, "%Y")
month <- format(current_date, "%m")
day <- format(current_date, "%d")
file_path <- create_folder_path(year, month, day)

# CSV-Datei abrufen und speichern
retrieve_csv(url, file_path)
