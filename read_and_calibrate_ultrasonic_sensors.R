# Notwendig um http requests zu senden
library(httr)

# Wo werden alle Daten bezüglich Ultraschall gespeichert?
# Erstelle diesen Ordner. Wenn er schon existiert passiert nichts
home_dir <- "C:/data/hswt/projektarbeit/Ultraschall_Programm/datenspeicher/Ultraschalldaten"
dir.create(home_dir, showWarnings = FALSE, recursive = TRUE)

###---------------------------------------------------------------------------------------------------------------------------

# Lies den n-ten Sensor aus
read_sensor <- function(sensor_n){
  return(GET(paste0("http://10.154.243.22/iolinkmaster/port[",sensor_n,"]/iolinkdevice/pdin/getdata")))
}

###---------------------------------------------------------------------------------------------------------------------------

# Extrahiere den Messwert, und nimm die ersten 4 Zeichen von links, da nur diese den Abstand darstellen
get_distance <- function(sensor_data){
  return(substr(content(sensor_data)$data$value, start=1, stop=4))
}

###---------------------------------------------------------------------------------------------------------------------------

# Lies den letzten Eintrag der Kalibrierungsdatei aus
# Nimm den Nullwert des n-ten Sensors
# Die tatsächliche Wasserhöhe ist die Differenz zwischen Referenzpegel und dem momentanen Wasserpegel
# Die Formel ergibt die Wassermenge in Litern. Sie ergibt sich aus der Excel Datei und einer polynomialen Approximation
get_volume <- function(distance, sensor_n){
  calibration_df <- read.csv(paste0(home_dir, "/us_calibration.csv"))
  last_calibration_value <- calibration_df[dim(calibration_df)[1], ][paste0("Nullwert_Sensor_", sensor_n)]
  
  water_height <- last_calibration_value - distance
  water_volume <- water_height*water_height*0.000115+0.53635*water_height
  
  return(water_volume)
}

###---------------------------------------------------------------------------------------------------------------------------

# Prüfe ob eine Kalibrierungsdatei existiert.
# Aktualisiere die Zeit/Datum und die Messwerte der Sensoren
# Erstelle einen individuellen Pfad (Jahr/Monat). Falls er schon existiert, passiert nichts
# Erstelle die Tagesdatei, falls sie noch nicht existiert.
# Schreibe die Spaltennamen in die Datei
# Hole dir die gemessenen Wassermengen der Sensoren und schreibe sie in die Datei
save_sensor_outputs <- function(){
  
  calibration_file <- file.path(home_dir, "us_calibration.csv")
  
  if(!file.exists(calibration_file)){
    warnings("Kalibrierung der Sensoren wurde noch nicht durchegführt")
  }
  else{
    update_time_and_date()
    query_sensors()
    
    output_path <- file.path(home_dir, year, month)
    output_file <- file.path(output_path, paste0("us_", year, month, day, ".csv"))
    
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    
    if(!file.exists(output_file)){
      file.create(output_file)
      df <- data.frame("Datum","Uhrzeit","Abstand_Sensor_1","Abstand_Sensor_2","Wassermenge_1","Wassermenge_2")
      write.table(df, file = output_file, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
    }
    
    water_volume_1 <- get_volume(distance_sensor_1, 1)
    water_volume_2 <- get_volume(distance_sensor_2, 2)
    
    df <- data.frame (paste(year,month,day,sep="-"), current_time, distance_sensor_1, distance_sensor_2, water_volume_1, water_volume_2)
    write.table(df, file = output_file, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  }
}

###---------------------------------------------------------------------------------------------------------------------------

# Erstelle globale Variablen für Uhrzeit, Jahr, Monat und Tag
update_time_and_date <- function(){
  current_date <<- Sys.Date()
  current_time <<- format(Sys.time(), format = "%H:%M:%S")
  
  year <<- format(current_date, "%Y")
  month <<- format(current_date, "%m")
  day <<- format(current_date, "%d")
}

###---------------------------------------------------------------------------------------------------------------------------

# Lies die aktuellen Messwerte der Sensoren aus und wandel die hex Zahl in eine dec um
# Mache die Werte global verfügbar
query_sensors <- function(){
  distance_sensor_1 <<- strtoi(get_distance(read_sensor(1)), base=16)
  distance_sensor_2 <<- strtoi(get_distance(read_sensor(2)), base=16)
}

###---------------------------------------------------------------------------------------------------------------------------

# Aktualisiere die Zeit/Datum und die Messwerte der Sensoren
# Erstelle eine Kalibrierungsdatei falls sie noch nicht existiert (also beim ersten mal Kalibrieren)
# Schreibe die Spaltennamen in die Datei
# Schreibe die aktuellen Messwerte der Sensoren in die Datei
calibrate_sensors <- function(){
  
  update_time_and_date()
  query_sensors()
  
  calibration_file <- file.path(home_dir, "us_calibration.csv")
  
  if(!file.exists(calibration_file)){
    file.create(calibration_file)
    df <- data.frame ("Datum","Uhrzeit","Nullwert_Sensor_1","Nullwert_Sensor_2")
    write.table(df, file = calibration_file, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  }
  
  df <- data.frame (paste(year,month,day,sep="-"), current_time, distance_sensor_1, distance_sensor_2)
  write.table(df, file = calibration_file, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}


# calibrate_sensors()
save_sensor_outputs()



