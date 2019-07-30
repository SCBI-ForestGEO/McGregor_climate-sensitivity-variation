######################################################
# Purpose: Create conglomerate graphs for McGregor et al 2019
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.3 - First created July 2019
######################################################
library(ggplot2)

#1 NEON vertical height profiles
source('scripts/vertical_height_neon.R', echo=TRUE)
#########################################################################
#2 height by crown position in 2018 ####
library(RCurl) #2
library(tidyr) #2
library(grid) #2
library(gridExtra) #2

#2a. heights for all cored trees ####
trees_all <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE)

scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)

current_ht <- trees_all[!duplicated(trees_all$tree), ]
current_ht$year <- 2018
current_ht <- current_ht[,c(1:4,13:15,17:19)]

current_ht$dbh_old.mm <- scbi.stem3$dbh[match(current_ht$tree, scbi.stem3$tag)]
current_ht$dbh_old.cm <- current_ht$dbh_old.mm/10
current_ht$dbh.ln.cm <- log(current_ht$dbh_old.cm)

#linear log-log regression
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation
current_ht$height.ln.m <- 
  ifelse(current_ht$sp == "caco", (0.348+0.808*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "cagl", (0.681+0.704*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "caovl", (0.621+0.722*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "cato", (0.776+0.701*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "fagr", (0.708+0.662*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "fram", (0.715+0.619*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "juni", (1.22+0.49*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "litu", (1.32+0.524*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "qual", (1.14+0.548*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "qupr", (0.44+0.751*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "quru", (1.17+0.533*current_ht$dbh.ln.cm),
   ifelse(current_ht$sp == "quve", (0.864+0.585*current_ht$dbh.ln.cm),
                                  (0.791+0.645*current_ht$dbh.ln.cm)))))))))))))

current_ht$height.m <- exp(current_ht$height.ln.m)
current_ht <- current_ht[order(current_ht$tree, current_ht$year), ]

#graphing height by crown position (for paper)
current_ht <- current_ht[!is.na(current_ht$position_all), ]
current_ht$position_all <- factor(current_ht$position_all, levels = c("dominant", "co-dominant", "intermediate", "suppressed"))

quantile(current_ht$height.m, c(.95), na.rm=TRUE) #95% quantile = 35.002m

quant <- data.frame(yintercept = 35.0022, Lines = "95th percentile")

heights <-
  ggplot(data = current_ht) +
  aes(x = position_all, y = height.m, group = position_all) +
  # aes(x=position_all, y=height.m, fill=year) +
  geom_boxplot() +
  xlab("crown position") +
  ylab("Height [m]") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
  theme_minimal()

#2b. get height data for all trees >10cm dbh in census ####
scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)
scbi.stem3$dbh <- ifelse(is.na(scbi.stem3$dbh), 0, scbi.stem3$dbh)

scbi18_ht <- scbi.stem3
scbi18_ht <- scbi18_ht[scbi18_ht$dbh>=100, ]
scbi18_ht <- scbi18_ht[,c(2:5,11,14:15)]

scbi18_ht$dbh_old.cm <- scbi18_ht$dbh/10
scbi18_ht$dbh.ln.cm <- log(scbi18_ht$dbh_old.cm)

#linear log-log regression
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation
scbi18_ht$height.ln.m <- 
   ifelse(scbi18_ht$sp == "caco", (0.348+0.808*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "cagl", (0.681+0.704*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "caovl", (0.621+0.722*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "cato", (0.776+0.701*scbi18_ht$dbh.ln.cm),
   ifelse(scbi18_ht$sp == "fagr", (0.708+0.662*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "fram", (0.715+0.619*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "juni", (1.22+0.49*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "litu", (1.32+0.524*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "qual", (1.14+0.548*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "qupr", (0.44+0.751*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "quru", (1.17+0.533*scbi18_ht$dbh.ln.cm),
    ifelse(scbi18_ht$sp == "quve", (0.864+0.585*scbi18_ht$dbh.ln.cm),
                                  (0.791+0.645*scbi18_ht$dbh.ln.cm)))))))))))))

scbi18_ht$height.m <- exp(scbi18_ht$height.ln.m) #used below in #4

#######################################################################################
#4 Get mean leaf traits as function of TWI and height ####
library(ggstance)
library(raster)
library(dplyr)
library(Hmisc)
library(magrittr)

#4a. get TWI values for all trees in census, add trait values ####
scbi.stem3$dbh <- ifelse(is.na(scbi.stem3$dbh), 0, scbi.stem3$dbh)
scbi.stem3 <- scbi.stem3[scbi.stem3$dbh >= 100, ] #>10cm dbh

#get utm coords
source('D:/Dropbox (Smithsonian)/Github_Ian/SCBI-ForestGEO-Data/R_scripts/SIGEO_plot_grid_UTMcoord.R', echo=TRUE)

plot_to_UTM(scbi.stem3)
scbi18 <- sigeo_coords
scbi18 <- scbi18[, c(2:5,20:21)]
scbi18_x <- scbi18[, c(5:6)]
topo <- raster("data/plot_TWI.tif")
twi <- raster::extract(topo, scbi18_x, method="simple")
scbi18$TWI <- twi
scbi18$TWI.ln <- log(scbi18$TWI)

#add in leaf traits (no rp because it's categorical)
leaf_traits <- trees_all[, c(2,7:10)]

for (i in seq(along=2:ncol(leaf_traits))){
   trait <- colnames(leaf_traits[2:ncol(leaf_traits)])
   scbi18[, trait[[i]]] <- leaf_traits[, trait[[i]]][match(scbi18$sp, leaf_traits$sp)]
}



#4b. create height groupings and graphs ####
scbi18$height.m <- scbi18_ht$height.m[match(scbi18$stemID, scbi18_ht$stemID)]
scbi18$height.ln.m <- log(scbi18$height.m)

range(scbi18$height.ln.m)
scbi18$bins <- cut(scbi18$height.m, breaks=c(-Inf,10,15,20,25,30,35,40,45,Inf), 
   labels=c("5-10","10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50"))
scbi18$bins <- cut(scbi18$height.m, breaks=c(-Inf,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30,32.5,35,37.5,40,42.5,45,47.5,51))

scbi18$bins <- cut2(scbi18$height.m, g=19)

# tlp ####
trait_tlp <- scbi18 %>%
   group_by(bins) %>%
   dplyr::summarize(avg = mean(mean_TLP_Mpa, na.rm=TRUE),
             dev = sd(mean_TLP_Mpa, na.rm=TRUE),
             no_data = (sum(is.na(mean_TLP_Mpa))/length(mean_TLP_Mpa)))

trait_tlp$sdmin <- trait_tlp$avg - trait_tlp$dev
trait_tlp$sdmax <- trait_tlp$avg + trait_tlp$dev
trait_tlp$col <- ifelse(trait_tlp$no_data > 0.25, "grey", "black")
trait_tlp$lab <- seq(5,95,by=5)
trait_tlp$y <- gsub("^.*,", "", trait_tlp$bins)
trait_tlp$y <- gsub("[[:punct:]]$", "", trait_tlp$y)
trait_tlp$y <- as.numeric(trait_tlp$y)


#TLP by height bins
# plot_tlp_ht <- 
   ggplot(trait_tlp, aes(x = avg, y = lab)) +
      geom_point(aes(color=col)) +
      scale_color_manual(values=c("black", "grey")) +
      ggplot2::geom_errorbarh(aes(y = lab, xmin = sdmin, xmax = sdmax, height=0.25, color=col)) +
      scale_x_continuous(breaks=c(-2.8,-2.6,-2.4,-2.2,-2.0,-1.8), limits=c(-2.8,-1.8)) +
      scale_y_continuous(breaks=seq(5,95,by=5)) +
      geom_path(aes(x=avg, y=lab, color=col, group=1)) +
      ylab("Height [percentiles]") +
      xlab("Mean TLP (MPa)") +
      theme_minimal()
   
   ggplot(trait_tlp, aes(x = avg, y = y)) +
      geom_point(aes(color=col)) +
      scale_color_manual(values=c("black", "grey")) +
      ggplot2::geom_errorbarh(aes(y = y, xmin = sdmin, xmax = sdmax, height=0.25, color=col)) +
      scale_x_continuous(breaks=c(-2.8,-2.6,-2.4,-2.2,-2.0,-1.8), limits=c(-2.8,-1.8)) +
      scale_y_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
      geom_path(aes(x=avg, y=y, color=col, group=1)) +
      ylab("Height [percentiles]") +
      xlab("Mean TLP (MPa)") +
      theme_minimal()

# pla ####
trait_pla <- scbi18 %>%
   group_by(bins) %>%
   dplyr::summarize(avg = mean(PLA_dry_percent, na.rm=TRUE),
             dev = sd(PLA_dry_percent, na.rm=TRUE),
             no_data = (sum(is.na(PLA_dry_percent))/length(PLA_dry_percent)))

trait_pla$sdmin <- trait_pla$avg - trait_pla$dev
trait_pla$sdmax <- trait_pla$avg + trait_pla$dev

#PLA by height bins
graph_pla <-
   ggplot(trait_pla, aes(x = avg, y = bins)) +
   geom_point() +
   scale_x_continuous(breaks = c(8,10,12,14,16,18,20,22,24,26), limits=c(8,26)) +
   geom_text(aes(label=round(no_data*100, 2)), vjust=-0.5) +
   ggplot2::geom_errorbarh(aes(y = bins, xmin = sdmin, xmax = sdmax, height=0.25)) +
   ylab("log(Height) [m]") +
   xlab("Percent Leaf Area [%]") +
   theme_minimal() +
   theme(axis.title.y = element_blank(), axis.text.y=element_blank())

#4c. create TWI groupings and graphs ####
scbi18$bins_TWI <- cut(scbi18$TWI, breaks=c(-Inf,4,6,8,10,12,14,Inf), 
      labels=c("2-4","4-6", "6-8", "8-10", "10-12", "12-14", "14-16"))
scbi18$bins_TWI <- cut2(scbi18$TWI, g=20)

#tlp_twi ####
trait_tlp_twi <- scbi18 %>%
   group_by(bins_TWI) %>%
   dplyr::summarize(avg = mean(mean_TLP_Mpa, na.rm=TRUE),
             dev = sd(mean_TLP_Mpa, na.rm=TRUE),
             no_data = (sum(is.na(mean_TLP_Mpa))/length(mean_TLP_Mpa)))

trait_tlp_twi$sdmin <- trait_tlp_twi$avg - trait_tlp_twi$dev
trait_tlp_twi$sdmax <- trait_tlp_twi$avg + trait_tlp_twi$dev

# plot_tlp_twi <- 
ggplot(trait_tlp_twi[!is.na(trait_tlp_twi$bins_TWI), ], aes(x = avg, y = bins_TWI)) +
   geom_point() +
   geom_text(aes(label=round(no_data*100, 2)), vjust=-0.5) +
   ggplot2::geom_errorbarh(aes(y = bins_TWI, xmin = sdmin, xmax = sdmax, height=0.25)) +
   scale_x_continuous(breaks=c(-2.8,-2.6,-2.4,-2.2,-2.0,-1.8), limits=c(-2.8,-1.8)) +
   ylab("TWI") +
   xlab("Mean TLP (MPa)") +
   theme_minimal()

ggplot(trait_tlp_twi[!is.na(trait_tlp_twi$bins_TWI), ], aes(y = avg, x = bins_TWI)) +
   geom_point() +
   # geom_text(aes(label=round(no_data*100, 2)), vjust=-0.5) +
   ggplot2::geom_errorbar(aes(x = bins_TWI, ymin = sdmin, ymax = sdmax, height=0.25)) +
   scale_y_continuous(breaks=c(-2.8,-2.6,-2.4,-2.2,-2.0,-1.8), limits=c(-2.8,-1.8)) +
   xlab("TWI") +
   ylab("Mean TLP (MPa)") +
   theme_minimal()

#pla_twi ####
trait_pla_twi <- scbi18 %>%
   group_by(bins_TWI) %>%
   summarize(avg = mean(PLA_dry_percent, na.rm=TRUE),
            dev = sd(PLA_dry_percent, na.rm=TRUE),
             no_data = (sum(is.na(PLA_dry_percent))/length(PLA_dry_percent)))
   
trait_pla_twi$sdmin <- trait_pla_twi$avg - trait_pla_twi$dev
trait_pla_twi$sdmax <- trait_pla_twi$avg + trait_pla_twi$dev
   
# plot_pla_twi <- 
ggplot(trait_pla_twi[!is.na(trait_pla_twi$bins_TWI), ], aes(x = avg, y = bins_TWI)) +
   geom_point() +
   geom_text(aes(label=round(no_data*100, 2)), vjust=-0.5) +
   ggplot2::geom_errorbarh(aes(y = bins_TWI, xmin = sdmin, xmax = sdmax, height=0.25)) +
   scale_x_continuous(breaks = c(8,10,12,14,16,18,20,22,24,26), limits=c(8,26)) +
   ylab("TWI") +
   xlab("PLA_dry_percent") +
   theme_minimal()


#######################################################################################
#5 Add the graphs together ####
quantile(current_ht$height.m, c(.99), na.rm=TRUE) #95% quantile = 35.002m
top <- current_ht[current_ht$position_all == "dominant", ]
mean(top$height.m, na.rm=TRUE)

quant <- data.frame(yintercept = 35.0022, Lines = "95th percentile")

#add this part to each graph:
geom_hline(aes(yintercept = yintercept, linetype = "dashed"), quant)

library(ggpubr)
NEON <- ggarrange(wind_plot, RH_plot, SAAT_plot, biotemp_plot,  nrow=1, ncol=4, common.legend = TRUE, legend = "right")

traits <- ggarrange(graph_tlp, graph_pla, nrow=1, ncol=4)

ggarrange(NEON, heights, traits, nrow=2, ncol=4)

grid.arrange(ggplotGrob(NEON), ggplotGrob(heights), ggplotGrob(traits), ncol=1)
