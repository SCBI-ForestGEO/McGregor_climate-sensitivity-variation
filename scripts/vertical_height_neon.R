#get NEON data for vertically-scaled air temperature

library(neonUtilities)
library(plotly)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(gridExtra)

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
                 "value" = c("tempSingleMean", "windSpeedMean", "bioTempMean", "RHMean", "difRadMean"),
                 "xlabs" = c("Mean Air Temperature (C)", "Mean Windspeed (m/s)", "Mean Infrared Biological Temperature (C)", "Relative Humidity", "Mean shortwave downward radiation (W/m2)"))


dp[] <- lapply(dp, as.character)
dp$value <- as.character(dp$value)

date <- data.frame("year" = c(rep(2018, 4), rep(2017, 4), rep(2016, 4), rep(2015, 4)),
                   "month" = c(rep(5:8, 4)))

years <- c("2017", "2018")

#this loop for some reason isn't producing plotly graphs that will work, but everything else runs smoothly

for (i in seq(along=1:4)){ #make 1:5 if using radiation (cloud vs sun threshold)
  neon_data_all <- NULL
  value <- dp$value[[i]]
  
  for (j in seq(along=1:2)){
    neon_tower <- loadByProduct(dpID=dp$id[[i]], 
                                site=c("SCBI"),
                                package="basic", avg=30, 
                                check.size = FALSE, 
                                #(use TRUE outside loop to see how big the dowloads are)
                                startdate=paste0(years[[j]], "-05"),
                                enddate=paste0(years[[j]], "-08"))
    
    neon_data <- neon_tower[[1]]
    neon_data_sub <- neon_data[colnames(neon_data) %in% c("verticalPosition", "startDateTime", value, "dirRadMean", "sunPres")]
    
    #reformat dates
    neon_data_sub$startDateTime <- as.character(neon_data_sub$startDateTime)
    neon_data_sub$startDateTime <- gsub("T", " ", neon_data_sub$startDateTime)
    neon_data_sub$startDateTime <- as.POSIXct(neon_data_sub$startDateTime, format = "%Y-%m-%d %H:%M:%OS")
    
    #get rid of erroneous April datapoints
    neon_data_sub <- neon_data_sub[!grepl("04-30", neon_data_sub$startDateTime), ]
    
    ## make consolidated graph over different months for each variable ####
    neon_data_sub$day <- substr(neon_data_sub$startDateTime, 1, nchar(neon_data_sub$startDateTime)-0)
    
    #want to preserve the dfs, so put them in a list
    # neon_vars[[i]] <- neon_data_sub
    # names(neon_vars)[i] <- paste0("neon_", dp$data[[i]])
    
    neon_data_all <- rbind(neon_data_all, neon_data_sub)
  }
  
  #the 10m air temperature values are completely off, and stop at 19 May 2018. The sensor is broken and hasn't been fixed
  if (value == "tempSingleMean"){
    neon_data_all$tempSingleMean <- ifelse(neon_data_all$verticalPosition == 10 & 
                                          grepl("2018", neon_data_all$day), NA, 
                                           neon_data_all$tempSingleMean)
  }
 
  
  #get mean of values per month per verticalPosition
  data_analy <- neon_data_all %>% 
    group_by(day, verticalPosition) %>% 
    summarize(test_max = max(get(value), na.rm=TRUE),
              test_min = min(get(value), na.rm=TRUE))
  
  data_analy$test_max <- ifelse(grepl("Inf", data_analy$test_max), NA, data_analy$test_max)
  data_analy$test_min <- ifelse(grepl("Inf", data_analy$test_min), NA, data_analy$test_min)
  
  data_analy$month <- NA
  data_analy$month <- 
    ifelse(grepl(paste0(years[[1]], "-05"), data_analy$day) |
             grepl(paste0(years[[2]], "-05"), data_analy$day), "May",
    ifelse(grepl(paste0(years[[1]], "-06"), data_analy$day) |
             grepl(paste0(years[[2]], "-06"), data_analy$day), "June",
    ifelse(grepl(paste0(years[[1]], "-07"), data_analy$day) |
             grepl(paste0(years[[2]], "-07"), data_analy$day), "July", 
                         "August")))
  
  #get rid of random days that aren't full dates
  data_analy <- data_analy[grepl(".{10}", data_analy$day), ]
  
  data_analy <- data_analy %>%
    group_by(month, verticalPosition) %>%
    summarize(mmax = mean(test_max, na.rm=TRUE),
              mmin = mean(test_min, na.rm=TRUE))

  #base ggplot, all months on same graph
  data_analy <- data_analy %>%
    gather(mmax, mmin, key = type, value = measure)
  
  data_analy$month_f <- factor(data_analy$month, levels=c("May", "June", "July", "August"))
  
  assign(paste0(dp$data[[i]], "_plot"),
    data_analy %>%
      arrange(verticalPosition) %>%
      ggplot(aes(x = measure, y = verticalPosition)) +
      geom_path(aes(color = month_f), size = 1) +
      scale_color_manual(values = c("orange", "red", "dark green", "blue"), name = "Month") +
      geom_point(aes(color = month_f)) +
      labs(x = dp$xlabs[[i]], y = "Height (m)") +
      theme_grey() +
      facet_wrap(~type)
  )
}

#arrange all graphs together

png("manuscript/tables_figures/NEON_vertical_profiles.png", width = 1000, height = 1000, pointsize = 18)
graph <- grid.arrange(SAAT_plot, wind_plot, RH_plot, biotemp_plot, nrow=2)

dev.off()

#original graphing format
# assign(paste0(dp$data[[i]], "_plot"),
#        ggplot(data_analy) +
#          geom_line(aes(x = verticalPosition, y = measure, color = month_f), size = 1) +
#          scale_color_manual(values = c("orange", "red", "dark green", "blue"), name = "Month") +
#          geom_point(aes(x = verticalPosition, y = measure, color = month_f)) +
#          labs(x = "Height (m)", y = dp$xlabs[[i]]) +
#          theme_grey() +
#          facet_wrap(~type)
# )







#graph with plotly
y <- list(title = value)

assign(paste0(dp$data[[i]], "_plot"), 
       plot_ly(data = neon_data_sub, x = ~startDateTime, y = ~neon_data_sub[, value], type = "scatter", color = ~verticalPosition, mode = "markers") %>%
         layout(yaxis = y))

SAAT_plot
wind_plot
biotemp_plot
RH_plot
SR_plot


#determine threshold for sunny/cloudy day ####
neon_data_sub$day <- substr(neon_data_sub$startDateTime, 1, nchar(neon_data_sub$startDateTime)-0)

q <- neon_data_sub %>%
      group_by(day) %>%
      summarize(mean_difRad = mean(difRadMean, na.rm=TRUE), mean_dirRad = mean(dirRadMean, na.rm=TRUE))

q <- neon_data_sub %>%
  group_by(day) %>%
  summarize(mean_sun = sum(sunPres, na.rm=TRUE))

q1 <- q[c(1,2)]
q1$type <- "difRad"
setnames(q1, old="mean_difRad", new="measure")
q2 <- q[c(1,3)]
q2$type <- "dirRad"
setnames(q2, old="mean_dirRad", new="measure")
q3 <- rbind(q1,q2)




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
