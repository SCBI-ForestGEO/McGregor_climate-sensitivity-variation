#compare pdsi values

library(raster)
library(ncdf4)
library(ncdf4.helpers)
library(ncdf.tools)
library(lubridate)

#National drought monitor ####
win_pdsi <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits/data/pdsi_timeseries_winchester_1949-2012.csv")
colnames(win_pdsi) <- c("date", "win_pdsi")
win_pdsi$date <- as.POSIXct(win_pdsi$date, format = "%m/%d/%Y")
win_pdsi$date <- as.Date(win_pdsi$date)

win_scpdsi <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits/data/scpdsi_timeseries_winchester_1949-2012.csv")
colnames(win_scpdsi) <- c("date", "win_scpdsi")
win_scpdsi$date <- as.POSIXct(win_scpdsi$date, format = "%m/%d/%Y")
win_scpdsi$date <- as.Date(win_scpdsi$date)

winchester <- merge(win_pdsi, win_scpdsi)

#CRU ####
# https://crudata.uea.ac.uk/cru/data/drought/
fname <- "I:/Krista Lab/Netcdf PDSI values/scPDSI.cru_ts3.26early.bams2018.GLOBAL.1901.2017.nc" #from CRU
cru <- nc_open("I:/Krista Lab/Netcdf PDSI values/scPDSI.cru_ts3.26early.bams2018.GLOBAL.1901.2017.nc")
time <- ncvar_get(cru, varid = "time")

#CRU data shows time as days since 1900-01-01.
timetrue <- convertDateNcdf2R(time, units = "days", origin = as.POSIXct("1900-01-01"), time.format = c("%Y-%m-%d"))

cru_nc <- brick(fname)
scbi <- data.frame("lon" = 78.1653, "lat" = 38.8871)
cru_scpdsi <- extract(cru_nc, scbi)
cru_scpdsi <- data.frame(cru_scpdsi)
cru_scpdsi[2, ] <- colnames(cru_scpdsi)

library(data.table)
trans <- transpose(cru_scpdsi)
colnames(trans) <- c("cru_scpdsi", "date")
trans$date <- timetrue
trans$date <- as.POSIXct(trans$date, format = "%Y-%m-%d %H:%M:%OS")
trans$date <- as.Date(trans$date)
trans$cru_scpdsi <- as.numeric(trans$cru_scpdsi)
trans <- trans[,c(2,1)]

all_values <- merge(trans, winchester, all=TRUE)

#NOAA ####
# https://www.esrl.noaa.gov/psd/data/gridded/data.pdsi.html#detail
