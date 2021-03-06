## ----get-data, include=FALSE, echo=FALSE---------------------------------
library(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
require(ncdf4)

writeLines("This script must be run from tikal as libraries aren't
currently installed elsewhere")

args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(DATA_DIR="/scr/raf/Prod_Data")

if (length(args) == 2) {
   print("Using values from command line")
   Project = args[1] ## project name in caps
   Flight <- args[2] ## rf16Z
} else if (length(args) == 0) {
   print("Using hardcoded values for Project and flight.")
   Project <- "DEEPWAVE"
   Flight <- "rf01"
} else {
   writeLines("Usage: \n\tTo use in RStudio browser window, edit hard-coded flight and project name above and run. \n\t To use at the command line, give project name in caps and flight number with lower case [rtf] as command line arguments, e.g. \n\t\tRscript PitchCorrection.R $project $flight")
   quit()
}

## variables needed for the pitch-correction algorithm, and to correct WIC:
Vars <- c("VNS", "VEW", "GGVNS", "GGVEW", "LAT", "GGALT", "THDG", "PITCH", "ROLL",
          "WIC", "TASX")
## opens file, by default in DataDirectory () which is /scr/raf_data/ on tikal:
fname <- sprintf("%s%s/%s%s.nc", DataDirectory (), Project, Project, Flight)
## copy the file to a duplicate and add the new variables to that duplicate
## (CAUTION:  will overwrite if already present)
fnew <- sprintf("%s%s/%s%sPC.nc", DataDirectory (), Project, Project, Flight)
unlink(fnew)
Z <- file.copy (fname, fnew)

## get a data.frame with the needed variables
DF <- getNetCDF (fname, Vars)
# DF$LAT <- DF$LATC   ## temporary for testing with file that doesn't have LAT
## get the dimensions from the data.frame
Dimensions <- attr(DF, "Dimensions")

## output differently for different data rates, so determine the data rate:
if ("sps25" %in% names (Dimensions)) {
  DataRate <- 25
  Dim <- list(Dimensions[["sps25"]], Dimensions[["Time"]])
} else if ("sps50" %in% names (Dimensions)) {
  DataRate <- 50
  Dim <- list(Dimensions[["sps50"]], Dimensions[["Time"]])
} else {
  DataRate <- 1
  Dim <- Dimensions[["Time"]]
}

## ----appl-pitch-correction, include=FALSE, echo=FALSE--------------------

## this calculates and applies the pitch correction
DP <- CorrectPitch (DF)
DF$PITCHC <- DF$PITCH - DP
DF$WIP <- DF$WIC + DF$TASX * DP * pi / 180.  # check this sign

## ----create-new-netCDF-variables, include=FALSE, echo=FALSE--------------

## Construct the new variables
netCDFfile <- nc_open (fnew, write=TRUE)
varPITCHC <- ncvar_def ("PITCHC", 
                        units="degree", 
                        dim=Dim, 
                        missval=as.single(-32767.),
                        prec='float',
                        longname="PITCH corrected for Schuler oscillation")
varWIP <- ncvar_def ("WIP", 
                     units="m/s", 
                     dim=Dim,
                     missval=as.single(-32767.),
                     prec='float',
                     longname="Vertical wind corrected for Schuler oscillation")
newfile <- ncvar_add (netCDFfile, varWIP) 
newfile <- ncvar_add (newfile, varPITCHC) 

## function to copy attributes from old variable (e.g., PITCH) to new one (e.g., PITCHC)
copy_attributes <- function (atv, v, nfile) {
  for (i in 1:length(atv)) {
    aname <- names(atv[i])
    if (grepl ('name', aname)) {next}  # skips long and standard names
    if (grepl ('units', aname)) {next}
    if (grepl ('Dependencies', aname)) {next}
    if (grepl ('actual_range', aname)) {next}
    if (grepl ('SampledRate', aname)) {next}
    if (is.numeric (atv[[i]])) {
      ncatt_put (nfile, v, attname=aname, attval=as.numeric(atv[[i]]))
    } else {
      ncatt_put (nfile, v, attname=aname, attval=as.character (atv[[i]]))
    }
  }
}

## get old attributes and copy them where appropriate to new variable
## PITCHC:
ATV <- ncatt_get (netCDFfile, "PITCH")
V <- "PITCHC"
copy_attributes (ATV, V, newfile)
ncatt_put (newfile, V, attname='standard_name',
           attval='corrected_pitch_angle')
ncatt_put (newfile, V, attname='Dependencies',
           attval='9 PITCH VNS VEW GGVNS GGVEW LAT GGALT THDG ROLL')
rng <- sprintf ("%6ff, %6ff", min(DF$PITCHC, na.rm=TRUE), max(DF$PITCHC, na.rm=TRUE))
ncatt_put (newfile, V, attname='actual_range', attval=rng)
## WIP
ATV <- ncatt_get (netCDFfile, "WIC")
V <- "WIP"
copy_attributes (ATV, V, newfile)
ncatt_put (newfile, V, attname='standard_name',
           attval='corrected_vertical_wind')
ncatt_put (newfile, V, attname='Dependencies',
           attval='4 WIC PITCH PITCHC TASX')
rng <- sprintf ("%6ff, %6ff", min(DF$WIP, na.rm=TRUE), max(DF$WIP, na.rm=TRUE))
ncatt_put (newfile, V, attname='actual_range', attval=rng)

## load the data into the new variable in the netCDF file
if (DataRate == 1) {
  ncvar_put (newfile, varPITCHC, DF$PITCHC)
  ncvar_put (newfile, varWIP, DF$WIP)
} else {
  ncvar_put (newfile, varPITCHC, DF$PITCHC, count=c(DataRate, nrow(DF)/DataRate))
  ncvar_put (newfile, varWIP, DF$WIP, count=c(DataRate, nrow(DF)/DataRate))
} 

## nothing gets written until this call:
nc_close (newfile)


