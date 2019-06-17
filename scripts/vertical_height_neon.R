#get NEON data for vertically-scaled air temperature

library(neonUtilities)
library(plotly)
library(lubridate)
library(dplyr)
library(data.table)

#traits from NEON
##1. single aspirated air temp: DP1.00002.001, avg = 1min or 30min
##2. 2-D wind speed and direction: DP1.00001.001, avg = 2min or 30min
##3. IR Biological Temp (infrared): DP1.00005.001, avg = 1min or 30min
##4. Relative humidity: DP1.00098.001, avg = 1min or 30min
##5. Shortwave radiation: DP1.00014.001, avg = 1min or 30min

#the function "loadByProduct" will load the data into R and collapse into one df (within a list).
#it will not download/store anything on the computer, but working with large dfs will run slowly. Hence, it is a good idea to look at the 30min avg first.

dp <- data.frame("data" = c("SAAT", "wind", "biotemp", "RH", "SR"),
                 "id" = c("DP1.00002.001", "DP1.00001.001", "DP1.00005.001", "DP1.00098.001", "DP1.00014.001"),
                 "value" = c("tempSingleMean", "windSpeedMean", "bioTempMean", "RHMean", "difRadMean"))

date <- data.frame("year" = c(rep(2018, 4), rep(2017, 4), rep(2016, 4), rep(2015, 4)),
                    "month" = c(rep(5:8, 4)))


dp[] <- lapply(dp, as.character)

#this loop for some reason isn't producing plotly graphs that will work, but everything else runs smoothly
for (i in seq(along=1:5)){
  dp$value <- as.character(dp$value)
  value <- dp$value[[i]]
  
  neon_tower <- loadByProduct(dpID=dp$id[[i]], 
                site=c("SCBI"),
                package="basic", avg=30, 
                check.size = FALSE, 
                #(use TRUE outside loop to see how big the dowloads are)
                startdate="2018-05",
                enddate="2018-08")
  
  neon_data <- neon_tower[[1]]
  neon_data_sub <- neon_data[colnames(neon_data) %in% c("verticalPosition", "startDateTime", value, "dirRadMean")]
  
  #reformat dates
  # neon_data_sub$startDateTime <- ymd_hms(as.character(neon_data_sub$startDateTime))
  neon_data_sub$startDateTime <- as.character(neon_data_sub$startDateTime)
  neon_data_sub$startDateTime <- gsub("T", " ", neon_data_sub$startDateTime)
  neon_data_sub$startDateTime <- as.POSIXct(neon_data_sub$startDateTime, format = "%Y-%m-%d %H:%M:%OS")
  
  #graph with plotly
  y <- list(title = value)
  
  assign(paste0(dp$data[[i]], "_plot"), 
         plot_ly(data = neon_data_sub, x = ~startDateTime, y = ~neon_data_sub[, value], type = "scatter", color = ~verticalPosition, mode = "markers") %>%
           layout(yaxis = y))
  
  #graph with ggplot
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
SR_plot



#determine threshold for sunny/cloudy day
neon_data_sub$day <- substr(neon_data_sub$startDateTime, 1, nchar(neon_data_sub$startDateTime)-0)

q <- neon_data_sub %>%
      group_by(day) %>%
      summarize(total_difRad = sum(difRadMean), total_dirRad = sum(dirRadMean, na.rm=TRUE))

q1 <- q[c(1,2)]
q1$type <- "difRad"
setnames(q1, old="total_difRad", new="measure")
q2 <- q[c(1,3)]
q2$type <- "dirRad"
setnames(q2, old="total_dirRad", new="measure")
q3 <- rbind(q1,q2)


## make consolidated graph over different months for each variable ####
neon_data_sub$month <- NA
neon_data_sub$month <- ifelse(grepl("05", neon_data_sub$day), "May",
                        ifelse(grepl("06", neon_data_sub$day), "June",
                        ifelse(grepl("07", neon_data_sub$day), "July",
                        ifelse(grepl("08", neon_data_sub$day), "August", 
                               neon_data_sub$month))))
wind <- neon_data_sub %>%
        group_by(month, verticalPosition) %>%
        summarize(total_ws = mean(windSpeedMean, na.rm=TRUE))

wind$month_f <- factor(wind$month, levels=c("May", "June", "July", "August"))
ggplot(wind, aes(verticalPosition)) +
  geom_line(data=wind[wind$month == "May", ], aes(y=total_ws), color="blue", size = 1) +
  geom_line(data=wind[wind$month == "June", ], aes(y=total_ws), color=" dark green", size = 1) +
  geom_line(data=wind[wind$month == "July", ], aes(y=total_ws), color="red", size = 1) +
  geom_line(data=wind[wind$month == "August", ], aes(y=total_ws), color="orange", size = 1) +
  xlab("Height (m)") +
  ylab("Mean windspeed 2018 (m/s)") +
  facet_grid(.~month_f) +
  theme_grey()



# other small things ####
boxplot(q$total_Rad)
ggplot(q) +
  aes(x = day, y = total_Rad) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()

esquisser()




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
