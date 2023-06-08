#!/usr/bin/env Rscript
# This script can be called whenever a new calibration is wanted and
# doesn't collide with the cyclical reading of us_read.R

# Get needed functions from file
source("us_functions.R")

calibrate_sensors()