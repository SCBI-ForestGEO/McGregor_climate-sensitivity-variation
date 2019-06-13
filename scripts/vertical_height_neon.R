#get NEON data for vertically-scaled air temperature

library(neonUtilities)
library(plotly)
library(lubridate)

#traits from NEON
##1. single aspirated air temp: DP1.00002.001, avg = 1min or 30min
##2. 2-D wind speed and direction: DP1.00001.001, avg = 2min or 30min
##3. IR Biological Temp (infrared): DP1.00005.001, avg = 1min or 30min
##4. Relative humidity: DP1.00098.001, avg = 1min or 30min
##5. Shortwave radiation

#this command will load the data into R and collapse into one df (within a list).
#it will not download/store anything on the computer, but working with large dfs will run slowly. Hence, it is a good idea to look at the 30min avg first.

dp <- data.frame("data" = c("SAAT", "wind", "biotemp", "RH"),
                 "id" = c("DP1.00002.001", "DP1.00001.001", "DP1.00005.001", "DP1.00098.001"),
                 "value" = c("tempSingleMean", "windSpeedMean", "bioTempMean", "RHMean"))

dp[] <- lapply(dp, as.character)

#this loop for some reason isn't producing plotly graphs that will work, but everything else runs smoothly
for (i in seq(along=1:4)){
  value <- dp$value[[i]]
  
  neon_tower <- loadByProduct(dpID=dp$id[[i]], 
                site=c("SCBI"),
                package="basic", avg=30, 
                check.size = FALSE, 
                #(use TRUE outside loop to see how big the dowloads are)
                startdate="2018-05",
                enddate="2018-08")
  
  neon_data <- neon_tower[[1]]
  neon_data_sub <- neon_data[colnames(neon_data) %in% c("verticalPosition", "startDateTime", value)]
  # neon_data_sub$startDateTime <- ymd_hms(as.character(neon_data_sub$startDateTime))
  
  neon_data_sub$startDateTime <- as.character(neon_data_sub$startDateTime)
  neon_data_sub$startDateTime <- gsub("T", " ", neon_data_sub$startDateTime)
  neon_data_sub$startDateTime <- as.POSIXct(neon_data_sub$startDateTime, format = "%Y-%m-%d %H:%M:%OS")
  
  y <- list(title = value)
  
  assign(paste0(dp$data[[i]], "_plot"), 
         plot_ly(data = neon_data_sub, x = ~startDateTime, y = ~neon_data_sub[, value], type = "scatter", color = ~verticalPosition, mode = "markers") %>%
           layout(yaxis = y))
  
  neon_data_sub$verticalPosition <- as.character(neon_data_sub$verticalPosition)
  
  assign(paste0(dp$data[[i]], "_graph"), 
         ggplot(data = neon_data_sub) +
           aes_string(x = colnames(neon_data_sub)[2], y = colnames(neon_data_sub)[3]) +
           geom_point(aes(group=verticalPosition, color=verticalPosition)) +
           theme_minimal())
}

SAAT_plot
wind_plot
biotemp_plot
RH_plot


value
gsub(\, "", value)
as.name(value)



neon_data <- neon_air[[1]]
unique(neon_data$verticalPosition)

neon_data_sub <- neon_data[ ,c("verticalPosition", "startDateTime", "tempSingleMean")]
neon_data_sub <- neon_data_sub[neon_data_sub$tempSingleMean > -10, ]

ggplot(data = neon_data_sub) +
  aes(x = startDateTime, y = tempSingleMean) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()+
  facet_wrap(~verticalPosition)


neon_data_sub$verticalPosition <- as.character(neon_data_sub$verticalPosition)
ggplot(data = neon_data_sub) +
  aes(x = startDateTime) +
  geom_line(aes(y=tempSingleMean, group=verticalPosition, color=verticalPosition)) +
  theme_minimal()
