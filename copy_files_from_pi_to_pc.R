library(ssh)
library(dplyr)

# SSH Verbindung zum Benutzer "admin" auf dem Raspberry Pi (die IP Adresse) herstellen
con <- ssh_connect("admin@192.168.0.10")

# Die Absoluten Pfade zu den Ordnern auf dem Pi und dem PC
#piDir <- "/home/admin/Daten"
piDir <- "/media/admin/6847-3231"
pcDir <- "C:/Users/willi/Desktop/Daten"

# Eine Liste aller Dateien im Verzeichnis auf dem Pi
filesOnPi <- ssh_exec_internal(con, paste0("ls ", piDir)) %>%
  .[["stdout"]]  %>%
  rawToChar()    %>%
  strsplit("\n") %>%
  unlist()

# Eine Liste aller Dateien im Verzeichnis auf dem PC 
filesOnPC <- list.files(pcDir)

# Liste von Dateien die ausschließlich auf dem Pi vorkommen
filesToDownload <- setdiff(filesOnPi, filesOnPC)

# Kopiere diese Dateien auf den PC, und lösche sie auf dem Pi
for(file in filesToDownload){
  scp_download(con, paste0(piDir, "/", file), to = pcDir, verbose=TRUE)
  #ssh_exec_internal(con, paste0("sudo rm ", piDir, "/", file))
}

# Trenne die ssh Verbindung
ssh_disconnect(con)
