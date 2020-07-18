######################################################
# Purpose: Create vertical profiles of climate variables for ForestGEO plot using NEON tower data
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created May 2019
######################################################
library(neonUtilities)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(grid)
library(gridExtra)
library(ggplot2)

#traits from NEON
##1. single aspirated air temp: DP1.00002.001, avg = 1min or 30min
##2. 2-D wind speed and direction: DP1.00001.001, avg = 2min or 30min
##3. IR Biological Temp (infrared): DP1.00005.001, avg = 1min or 30min
##4. Relative humidity: DP1.00098.001, avg = 1min or 30min
##5. Shortwave radiation: DP1.00014.001, avg = 1min or 30min

#the function "loadByProduct" will load the data into R and collapse into one df (within a list).
#it will not download/store anything on the computer, but working with large dfs will run slowly. Hence, it is a good idea to look at the 30min avg first.

dp <- data.frame("data" = c("SAAT", "2DWSD", "biotemp", "RH", "SR"),
                 "id" = c("DP1.00002.001", "DP1.00001.001", "DP1.00005.001", "DP1.00098.001", "DP1.00014.001"),
                 "value" = c("tempSingleMean", "windSpeedMean", "bioTempMean", "RHMean", "difRadMean"),
                 "xlabs" = c("Mean Air Temperature [°C]", "Wind speed [m/s]", "Mean Infrared Biological Temperature [°C]", "RH [%]", "Mean shortwave downward radiation [W/m^2]"))

dp <- dp[c(2,4,1,3,5), ]

dp[] <- lapply(dp, as.character)
dp$value <- as.character(dp$value)

date <- data.frame("year" = c(rep(2018, 4), rep(2017, 4), rep(2016, 4), rep(2015, 4)),
                   "month" = c(rep(5:8, 4)))

years <- c("2016", "2017", "2018")

#this loop for some reason isn't producing plotly graphs that will work, but everything else runs smoothly

alldt <- list()
for (i in seq(along=dp$value[1:3])){ #4 is biotemp and 5 is radiation (cloud vs sun threshold)
  neon_data_all <- NULL
  value <- dp$value[[i]]
  
  for (j in seq(along=years)){
    if (value != "RHMean" | j != 1){
      
      #this first if...else statement is due to an error with API. I've been told by 
      #NEON (Claire Lunch) that it has been fixed and will be updated with the CRAN
      #update by end March 2020.
      #To change this back with that new update, simply have the "else" condition be the 
      #code for everything.
      if((j==2) | (i==3 & j==1)){ 
        neon_tower <- loadByProduct(dpID=dp$id[[i]], 
                                    site=c("SCBI"),
                                    package="basic", 
                                    check.size = FALSE, 
                                    #(use TRUE outside loop to see how big the dowloads are)
                                    startdate=paste0(years[[j]], "-05"),
                                    enddate=paste0(years[[j]], "-08"))
        neon_data <- neon_tower[grepl(dp[,"data"][i], names(neon_tower))]
        neon_data <- as.data.frame(neon_data[[1]])
      } else {
        neon_tower <- loadByProduct(dpID=dp$id[[i]], 
                                    site=c("SCBI"),
                                    package="basic", avg=30, 
                                    check.size = FALSE, 
                                    #(use TRUE outside loop to see how big the dowloads are)
                                    startdate=paste0(years[[j]], "-05"),
                                    enddate=paste0(years[[j]], "-08"))
        
        neon_data <- neon_tower[grepl(dp[,"data"][i], names(neon_tower))]
        neon_data <- as.data.frame(neon_data[[1]])
      }
      
      neon_data$verticalPosition <- as.numeric(neon_data$verticalPosition)
      neon_data_sub <- neon_data[colnames(neon_data) %in% c("verticalPosition", "startDateTime", value, "dirRadMean", "sunPres")]
      
      #reformat dates
      neon_data_sub$startDateTime <- as.character(neon_data_sub$startDateTime)
      neon_data_sub$startDateTime <- gsub("T", " ", neon_data_sub$startDateTime)
      neon_data_sub$startDateTime <- as.POSIXct(neon_data_sub$startDateTime, format = "%Y-%m-%d %H:%M:%OS")
      
      #get rid of erroneous April datapoints
      neon_data_sub <- neon_data_sub[!grepl("04-30", neon_data_sub$startDateTime), ]
      
      ## make consolidated graph over different months for each variable ####
      neon_data_sub$day <- substr(neon_data_sub$startDateTime, 1, 10)
      
      test <- as.POSIXct(as.character("2016-07-31 17:20:00"), format= "%Y-%m-%d %H:%M:%OS")
      substr(test, 1, 10)
      
      
      #want to preserve the dfs, so put them in a list
      # neon_vars[[i]] <- neon_data_sub
      # names(neon_vars)[i] <- paste0("neon_", dp$data[[i]])
      
      neon_data_all <- rbind(neon_data_all, neon_data_sub)
      
    } else if(value == "RHMean" & j == 1){ #no RH data for 2016, so have to recreate the df to make sure code runs
      neon_data_sub[, 3] <- NULL
      neon_data_sub[, value] <- NA
      neon_data_sub <- neon_data_sub[, c(1:2,4,3)]
      
      neon_data_sub$verticalPosition <- ifelse(neon_data_sub$verticalPosition == 20, 
                                               60, 
                                               neon_data_sub$verticalPosition)
      neon_data_sub <- neon_data_sub[neon_data_sub$verticalPosition %in% c(0, 60), ]
      neon_data_sub$day <- str_replace(neon_data_sub$day, "2018", "2016")
      
      neon_data_all <- rbind(neon_data_all, neon_data_sub)
    }
  }
  
  #the 10m air temperature values are completely off, and stop at 19 May 2018. The sensor is broken and hasn't been fixed
  if (value == "tempSingleMean"){
    neon_data_all$tempSingleMean <- 
      ifelse(neon_data_all$verticalPosition == 10 & 
               grepl("2018", neon_data_all$day), NA, 
               neon_data_all$tempSingleMean)
  }
  
  #put full df in list to run anova later
  alldt[[i]] <- neon_data_all
  
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
              mmin = mean(test_min, na.rm=TRUE),
              sd_max = sd(test_max, na.rm=TRUE),
              sd_min = sd(test_min, na.rm=TRUE))
              # quant_95 = quantile(test_max, c(0.95), na.rm=TRUE),
              # quant_05 = quantile(test_min, c(0.05), na.rm=TRUE))
  
  #base ggplot, all months on same graph
  data_analy$month_f <- factor(data_analy$month, levels=c("May", "June", "July", "August"))
  
  #CORRECT HEIGHTS (according to tower dimensions, not downloaded data)
  data_analy$verticalPosition <-
    ifelse(data_analy$verticalPosition == 10, 5.8,
    ifelse(data_analy$verticalPosition == 20, 19.2,
    ifelse(data_analy$verticalPosition == 30, 26,
    ifelse(data_analy$verticalPosition == 40, 32.9,
    ifelse(data_analy$verticalPosition == 50, 38,
    ifelse(data_analy$verticalPosition == 60, 51.8,
           data_analy$verticalPosition))))))
  
  data_analy$vertPos_jitter <- NA
  
  if(!i == 2){
    data_analy$vertPos_jitter <- 
      ifelse(data_analy$month == "May", data_analy$verticalPosition + 0.05,
       ifelse(data_analy$month == "June", data_analy$verticalPosition + 0.25,
       ifelse(data_analy$month == "July", data_analy$verticalPosition + 0.5,
                                          data_analy$verticalPosition + 0.75)))
  } else {
    data_analy$vertPos_jitter <- 
      ifelse(data_analy$month == "May" & data_analy$verticalPosition == 0, 
             data_analy$verticalPosition + 0.05,
      ifelse(data_analy$month == "June" & data_analy$verticalPosition == 0,
              data_analy$verticalPosition + 0.25,
      ifelse(data_analy$month == "July" & data_analy$verticalPosition == 0,
             data_analy$verticalPosition + 0.5,
      ifelse(data_analy$month == "August" & data_analy$verticalPosition == 0,
             data_analy$verticalPosition + 0.75,
      
      ifelse(data_analy$month == "May" & data_analy$verticalPosition == 60, 
             data_analy$verticalPosition - 0.75,
      ifelse(data_analy$month == "June" & data_analy$verticalPosition == 60,
                    data_analy$verticalPosition - 0.5,
      ifelse(data_analy$month == "July" & data_analy$verticalPosition == 60,
                           data_analy$verticalPosition - 0.25,
                                  data_analy$verticalPosition - 0.05)))))))
  }
  
  graph <-
    data_analy %>%
      arrange(verticalPosition) %>%
      ggplot() +
      scale_color_manual(values = c("darkorange", "red", "darkgreen", "blue"), 
                         name = "Month") +
      geom_point(aes(x = mmax, y = vertPos_jitter, color = month_f), 
                 shape=19, position = "jitter") +
      geom_point(aes(x = mmin, y = vertPos_jitter, color = month_f), 
                 shape=17, position = "jitter") +
      geom_path(aes(x = mmax, y = vertPos_jitter, color = month_f, 
                    linetype = "Max"), size = 1) +
      geom_path(aes(x = mmin, y = vertPos_jitter, color = month_f, 
                    linetype = "Min"), size = 1) +
      ggplot2::geom_errorbarh(aes(xmin=mmax-sd_min, xmax=mmax+sd_max, y=vertPos_jitter, color = month_f, linetype = "Max", height=0.8)) +
      ggplot2::geom_errorbarh(aes(xmin=mmin-sd_min, xmax=mmin+sd_max, y=vertPos_jitter, color = month_f, linetype = "Min", height=0.8)) +
      labs(x = dp$xlabs[[i]], y = "Height [m]") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
      theme_bw() +
      guides(linetype = guide_legend("Line type"))
  
  if(i != 1){
    graph <- graph + theme(axis.title.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank())
  }
  
  if(i == 2){
    graph <- graph + scale_x_continuous(breaks=c(40,60,80,100))
  }
  
  if(i == 3){
    graph <- 
      graph + 
      labs(x= expression(paste("T"["air"], " [",degree,"C]"))) +
      scale_x_continuous(breaks=c(10,20,30), limits=c(5,35))
  }
  
  # if(i == 4){
  #   graph <- 
  #     graph + 
  #     labs(x = expression(paste("T"["biological"], " [",degree,"C]"))) +
  #     scale_x_continuous(breaks=c(10,20,30), limits=c(5,35))
  # }
  
  if(i==1){
    assign(paste0("wind", "_plot"), graph)
  } else {
    assign(paste0(dp$data[[i]], "_plot"), graph)
  }
}
names(alldt) <- dp$value[1:3]


# #arrange all graphs together and save image ####
# #extract legend
# #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend <- g_legend(SAAT_plot)
# 
# #formatting
# png("manuscript/tables_figures/NEON_vertical_profiles.png", width = 1000, height = 1000, pointsize = 18)
# graph <- grid.arrange(arrangeGrob(SAAT_plot + theme(legend.position = "none"),
#                                   wind_plot + theme(legend.position = "none"),
#                                   RH_plot + theme(legend.position = "none"),
#                                   biotemp_plot + theme(legend.position = "none"), 
#                                   nrow=1), 
#                       mylegend, nrow=1, 
#                       top = textGrob(expression(bold("NEON Vertical Profile 2016-2018"))))
# 
# dev.off()
# 
# library(ggpubr)
# NEON <- ggarrange(SAAT_plot, wind_plot, biotemp_plot, RH_plot, nrow=1, ncol=4, common.legend = TRUE, legend = "right")
# 
# annotate_figure(NEON,
#                 left = text_grob("Height (m)", rot = 90),
#                 fig.lab = "Figure 1", fig.lab.face = "bold"
# )
# 
# 
# SAAT_plot
# wind_plot
# biotemp_plot
# RH_plot
# SR_plot
# 

# #determine threshold for sunny/cloudy day ####
# neon_data_sub$day <- substr(neon_data_sub$startDateTime, 1, nchar(neon_data_sub$startDateTime)-0)
# 
# q <- neon_data_sub %>%
#       group_by(day) %>%
#       summarize(mean_difRad = mean(difRadMean, na.rm=TRUE), mean_dirRad = mean(dirRadMean, na.rm=TRUE))
# 
# q <- neon_data_sub %>%
#   group_by(day) %>%
#   summarize(mean_sun = sum(sunPres, na.rm=TRUE))
# 
# q1 <- q[c(1,2)]
# q1$type <- "difRad"
# setnames(q1, old="mean_difRad", new="measure")
# q2 <- q[c(1,3)]
# q2$type <- "dirRad"
# setnames(q2, old="mean_dirRad", new="measure")
# q3 <- rbind(q1,q2)
