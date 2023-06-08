#!/usr/bin/env Rscript
# This script is meant to be called in the background and kept running

# Get needed functions from file
source("us_functions.R")

# A 5 minute interval in seconds
interval = 300

while(TRUE) {
    # Execute function and save elapsed seconds
    elapsed_time = system.time(save_sensor_outputs())[3]
    # Sleep for time left until next interval
    Sys.sleep(interval - elapsed_time) 
}
