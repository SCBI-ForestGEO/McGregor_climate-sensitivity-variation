#compare pdsi values

library(raster)
library(ncdf4)
library(ncdf4.helpers)
library(ncdf.tools)
library(lubridate)

#National drought monitor ####
# https://droughtatlas.unl.edu/Data.aspx
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
#noaa pdsi ####
# https://www.esrl.noaa.gov/psd/data/gridded/data.pdsi.html
fname_noaa <- "C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits/data/pdsi.mon.mean.nc" #from NOAA pdsi
noaa_nc <- nc_open("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits/data/pdsi.mon.mean.nc")
time <- ncvar_get(noaa_nc, varid = "time")

#NOAA shows data as hours since 1800-01-01 00:00:0.0
timetrue_noaa <- convertDateNcdf2R(time, units = "hours", origin = as.POSIXct("1800-01-01"), time.format = c("%Y-%m-%d"))

noaa <- brick(fname_noaa)
scbi <- data.frame("lon" = 78.1653, "lat" = 38.8871)
noaa_pdsi <- extract(noaa, scbi)
noaa_pdsi <- data.frame(noaa_pdsi)
noaa_pdsi[2, ] <- colnames(noaa_pdsi)

library(data.table)
noaa_pd <- transpose(noaa_pdsi)
colnames(noaa_pd) <- c("noaa_pdsi", "date")
noaa_pd$date <- gsub("X", "", noaa_pd$date)
#noaa_pd$date <- gsub(".00.00.00", "", noaa_pd$date)

#change date to match with other dataframes
noaa_pd$date <- gsub("09-30", "10-01", noaa_pd$date)
noaa_pd$date <- gsub("10-31", "11-01", noaa_pd$date)
noaa_pd$date <- gsub("11-30", "12-01", noaa_pd$date)
noaa_pd$date <- gsub("12-31", "01-01", noaa_pd$date)
noaa_pd$date <- gsub("01-31", "02-01", noaa_pd$date)
noaa_pd$date <- gsub("02-29", "03-01", noaa_pd$date)
noaa_pd$date <- gsub("02-28", "03-01", noaa_pd$date)
noaa_pd$date <- gsub("03-31", "04-01", noaa_pd$date)
noaa_pd$date <- gsub("04-30", "05-01", noaa_pd$date)
noaa_pd$date <- gsub("05-31", "06-01", noaa_pd$date)
noaa_pd$date <- gsub("06-30", "07-01", noaa_pd$date)
noaa_pd$date <- gsub("07-31", "08-01", noaa_pd$date)
noaa_pd$date <- gsub("08-31", "09-01", noaa_pd$date)

noaa_pd$date <- as.POSIXct(noaa_pd$date, format = "%Y.%m.%d.%H.%M.%OS")
noaa_pd$date <- as.Date(noaa_pd$date)
noaa_pd$noaa_pdsi <- as.numeric(noaa_pd$noaa_pdsi)
noaa_pd$noaa_pdsi <- round(noaa_pd$noaa_pdsi, digits=2)
noaa_pd <- noaa_pd[,c(2,1)]

#noaa scpdsi ####
# https://www.esrl.noaa.gov/psd/data/gridded/data.pdsi.html 
fname_noaa <- "C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits/data/pdsi.mon.mean.selfcalibrated.nc" #from NOAA pdsi
noaa_nc <- nc_open("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/tree-growth-and-traits/data/pdsi.mon.mean.selfcalibrated.nc")
time <- ncvar_get(noaa_nc, varid = "time")

#NOAA shows data as hours since 1800-01-01 00:00:0.0
timetrue_noaa <- convertDateNcdf2R(time, units = "hours", origin = as.POSIXct("1800-01-01"), time.format = c("%Y-%m-%d"))

noaa <- brick(fname_noaa)
scbi <- data.frame("lon" = 78.1653, "lat" = 38.8871)
noaa_scpdsi <- extract(noaa, scbi)
noaa_scpdsi <- data.frame(noaa_scpdsi)
noaa_scpdsi[2, ] <- colnames(noaa_scpdsi)

library(data.table)
noaa_sc <- transpose(noaa_scpdsi)
colnames(noaa_sc) <- c("noaa_scpdsi", "date")
noaa_sc$date <- gsub("X", "", noaa_sc$date)

#change date to match with other dataframes
noaa_sc$date <- gsub("09-30", "10-01", noaa_sc$date)
noaa_sc$date <- gsub("10-31", "11-01", noaa_sc$date)
noaa_sc$date <- gsub("11-30", "12-01", noaa_sc$date)
noaa_sc$date <- gsub("12-31", "01-01", noaa_sc$date)
noaa_sc$date <- gsub("01-31", "02-01", noaa_sc$date)
noaa_sc$date <- gsub("02-29", "03-01", noaa_sc$date)
noaa_sc$date <- gsub("02-28", "03-01", noaa_sc$date)
noaa_sc$date <- gsub("03-31", "04-01", noaa_sc$date)
noaa_sc$date <- gsub("04-30", "05-01", noaa_sc$date)
noaa_sc$date <- gsub("05-31", "06-01", noaa_sc$date)
noaa_sc$date <- gsub("06-30", "07-01", noaa_sc$date)
noaa_sc$date <- gsub("07-31", "08-01", noaa_sc$date)
noaa_sc$date <- gsub("08-31", "09-01", noaa_sc$date)

noaa_sc$date <- as.POSIXct(noaa_sc$date, format = "%Y.%m.%d.%H.%M.%OS")
noaa_sc$date <- as.Date(noaa_sc$date)
noaa_sc$noaa_scpdsi <- as.numeric(noaa_sc$noaa_scpdsi)
noaa_sc$noaa_scpdsi <- round(noaa_sc$noaa_scpdsi, digits=2)
noaa_sc <- noaa_sc[,c(2,1)]

#merge both noaa data
all_noaa <- merge(noaa_pd, noaa_sc, all=TRUE)

#noaa pdsi from northern VA (used in Valentine script) ####
noaa_va <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/climate_sensitivity_cores/data/climate/Formated_NOAA_PDSI_Northern_Virginia_1895_2017.csv")

noaa_va <- noaa_va[,c(1:2,5)]
noaa_va$year <- paste0(noaa_va$year,"/", noaa_va$month, "/01")
noaa_va$year <- as.POSIXct(noaa_va$year, format = "%Y/%m/%d")
noaa_va$year <- as.Date(noaa_va$year)

noaa_va <- noaa_va[,c(1,3)]
noaa_va <- noaa_va[!noaa_va$PDSI < -10, ]
colnames(noaa_va) <- c("date", "noaa_va_pdsi")

all_noaa <- merge(all_noaa, noaa_va, all=TRUE)

#merge everything and graph ####
full_vals <- merge(all_noaa, all_values, all=TRUE)

write.csv(full_vals, "pdsi_value_comparison.csv", row.names=FALSE)

full_vals <- full_vals[full_vals$date > "1899-12-01", ] #everything before 1900 = NA
library(ggplot2)
ggplot(data = full_vals, aes(x=date, color=)) +
  geom_line(aes(y = noaa_pdsi), color = "yellow", size=0.75) +
  geom_line(aes(y = noaa_scpdsi), color = "brown", size=0.75) +
  geom_line(aes(y=noaa_va_pdsi), color = "orange", size=0.75) +
  geom_line(aes(y = cru_scpdsi), color = "black", size=0.75) +
  geom_line(aes(y = win_pdsi), color = "green", size=0.75) +
  geom_line(aes(y = win_scpdsi), color = "dark grey", size=0.75) +
  labs(y="pdsi and scpdsi values") +
  theme(legend.position="right") +
  theme_minimal()
