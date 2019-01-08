##read in the met tower data and reformat

#read and reformat met data ####
data_2018 <- read.csv("V:/SIGEO/Met tower/Meteo DATA/TXT_Files/2018_Data/SCB_Metdata_5min_2018.csv", header=FALSE, stringsAsFactors = FALSE)

#remove unnecessary rows
test <- data_2018[-c(1,4),]

#combine descriptor rows (variable and unit) into one, then make them the headers
test <- rbind(paste0(test[1,], sep="_", test[2,]), test[3:nrow(test),], stringsAsFactors=FALSE)

colnames(test) <- test[1,]
test <- test[-1,]

#convert date into usable format
library(lubridate)
test$TIMESTAMP_TS <- mdy_hm(test$TIMESTAMP_TS, tz="EST")

#can also split timestamp into two different columns and format from there
##library(tidyr)
##test <- test %>%
  ##separate(TIMESTAMP_TS, c("date", "time"), " ")

#make graphs from data ####

#I'm attempting to replicate a matlab script in R.
#the original file ("MAIN") in "V:/SIGEO/Met tower/R script/Matlab R scrip" was created in 2015. There is a function called readMat via the R.matlab package, but it can only read a .mat file from certain Matlab versions. As the MAIN file was originally a .m file and the software version isn't known, this won't work.

##either figure out how to recreate the matlab script exactly, or just try to figure out graphs for R because currently with esquisse it's creating some odd outputs

#Matlab script:
clear all
ref <- datenum('12/31/2012')

#SBC=importdata('D:\Projects\Metdata\other stations\SCB\SBC_Metdata_5min_2012.csv');
SBC <- importdata('SBC_Metdata_5min_2013_10_30_15_21_18.csv')

N <- length(SBC.data)
SBC.decday <- datenum(SBC.textdata(5:},1),'mm/dd/yyyy HH:MM')-ref
SBC.Rs <- SBC.data(:,2) #solar radiation
SBC.Rs(:,2) <- SBC.data(:,4)
SBC.T_air <- SBC.data(:,6) #air temp
SBC.T_air(:,2) <- SBC.data(:,8)
SBC.RH <- SBC.data(:,10) #relative humidity
SBC.WS <- SBC.data(:,12) #wind speed
SBC.WD <- SBC.data(:,13) #wind direction
SBC.WM <- SBC.data(:,14)
SBC.PPT <- SBC.data(:,17) #precipitation
SBC.Asp_fan <- SBC.data(:,22)
SBC.Batt_V <- SBC.data(:,23) #battery voltage
#################
figure(1);clf
plot(SBC.decday,SBC.Rs)
xlabel('time (decimal day)','Fontsize',14)
ylabel('Solar radiation (W m^-^2)','Fontsize',14)
figure(2);clf
plot(SBC.decday,SBC.T_air);hold all
xlabel('time (decimal day)','Fontsize',14)
ylabel('temperature (C)','Fontsize',14)
figure(3);clf
plot(SBC.decday,SBC.RH);hold all
xlabel('time (decimal day)','Fontsize',14)
ylabel('Relative humidity (#)','Fontsize',14)
figure(4);clf
plot(SBC.decday,SBC.WS)
xlabel('time (decimal day)','Fontsize',14)
ylabel('wind speed (m/s)','Fontsize',14)
use2 <- find(SBC.WS>1 & SBC.WS<10)
figure(5);clf
rose(SBC.WD(use2)/180*pi,50)
P1 <- nan(365,1)
for (i in 1:365){
  use=find(floor(SBC.decday)==i);if (!isempty(use)){
    P1(i,1) <- sum(SBC.PPT(use));}
}
figure(6);clf
bar(1:365,P1)

figure(7);clf
figure(7);clf
plot(SBC.decday,SBC.Batt_V);hold all
xlabel('time (decimal day)','Fontsize',14)
ylabel('Battery voltage (V)','Fontsize',14)







