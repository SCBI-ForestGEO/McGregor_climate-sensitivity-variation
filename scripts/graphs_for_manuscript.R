######################################################
# Purpose: Create conglomerate graphs for McGregor et al 2019
# Developed by: Ian McGregor - mcgregori@si.edu
# R version 3.5.3 - First created July 2019
######################################################
library(ggplot2)

#1. NEON vertical height profiles
source('scripts/vertical_height_neon.R', echo=TRUE)
NEON_list <- list(wind_plot, RH_plot, SAAT_plot, biotemp_plot)
#########################################################################
#2 height by crown position in 2018 ####
library(RCurl) #2
library(tidyr) #2
library(grid) #2
library(gridExtra) #2

##2a. heights for all cored trees ####
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
current_ht$position_all_abb <- substring(current_ht$position_all, 0, 1)
current_ht$position_all_abb <- toupper(current_ht$position_all_abb)
current_ht$position_all_abb <- factor(current_ht$position_all_abb, levels = c("D", "C", "I", "S"))


heights <-
  ggplot(data = current_ht, aes(x = position_all_abb, y = height.m, group = position_all)) +
  # aes(x=position_all, y=height.m, fill=year) +
  geom_boxplot() +
  xlab("Crown position") +
  ylab("Height [m]") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
  theme_minimal() +
  theme(axis.text = element_text(size=12)) +
  theme(axis.title = element_text(size=14))

##2b. get height data for all trees >10cm dbh in census ####
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
#3 Get PLA and TLP as function of TWI and height ####
library(ggstance)
library(raster)
library(dplyr)
library(Hmisc)
library(magrittr)

##3a. get TWI values for all trees in census, add trait values ####
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

##3b. create height groupings and graphs (Figure S1) ####
scbi18$height.m <- scbi18_ht$height.m[match(scbi18$stemID, scbi18_ht$stemID)]


scbi18$bins <- cut2(scbi18$height.m, g=19)
scbi18$bins_TWI <- cut2(scbi18$TWI, g=20)

type = c("tlp_ht", "pla_ht", "tlp_twi", "pla_twi")
var = c("mean_TLP_Mpa", "PLA_dry_percent", "mean_TLP_Mpa", "PLA_dry_percent")

breaks_tlp = c(-2.8,-2.6,-2.4,-2.2,-2.0,-1.8)
breaks_pla = c(8,12,16,20,24)

limits_tlp = c(-2.8,-1.8)
limits_pla = c(8,26)

graph_names <- paste0(main$type, "_plot")

for (i in seq(along=1:4)){
   if(!grepl("twi", type[[i]])){
      df <- scbi18 %>%
         group_by(bins) %>%
         dplyr::summarize(avg = mean(eval(parse(text = var[i])), na.rm=TRUE),
                          dev = sd(eval(parse(text = var[i])), na.rm=TRUE),
                          no_data = (sum(is.na(eval(parse(text = var[i]))))/length(eval(parse(text = var[i])))))
      
      df$sdmin <- df$avg - df$dev
      df$sdmax <- df$avg + df$dev
      df$col <- ifelse(df$no_data > 0.25, "grey", "black")
      df$lab <- seq(5,95,by=5) #percentiles
      df$num <- gsub("^.*,", "", df$bins) #upper bound of bin
      df$num <- gsub("[[:punct:]]$", "", df$num)
      df$num <- as.numeric(df$num)
      
   } else if (grepl("twi", type[[i]])){
      df <- scbi18 %>%
         group_by(bins_TWI) %>%
         dplyr::summarize(avg = mean(eval(parse(text = var[i])), na.rm=TRUE),
                          dev = sd(eval(parse(text = var[i])), na.rm=TRUE),
                          no_data = (sum(is.na(eval(parse(text = var[i]))))/length(eval(parse(text = var[i])))))
      
      df$sdmin <- df$avg - df$dev
      df$sdmax <- df$avg + df$dev
      df$col <- ifelse(df$no_data > 0.25, "grey", "black")
      df$num <- gsub("^.*,", "", df$bins_TWI) #upper bound of bin
      df$num <- gsub("[[:punct:]]$", "", df$num)
      df$num <- as.numeric(df$num)
   }
   
   #create plots
   if(!grepl("twi", type[[i]])){
      q <- ggplot(df, aes(x = avg, y = num)) +
         geom_point(aes(color=col)) +
         scale_color_manual(values=c("black", "grey")) +
         ggplot2::geom_errorbarh(aes(y = num, xmin = sdmin, xmax = sdmax, height=0.5, color=col)) +
         scale_y_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
         geom_path(aes(x=avg, y=num, color=col, group=1)) +
         ylab("Height [m]") +
         theme_minimal() +
         theme(legend.position = "none",
               axis.text = element_text(size=12),
               axis.title = element_text(size=14))
      
      if(i==1){
         q <- q + 
            xlab("Mean turgor loss point [MPa]") +
            scale_x_continuous(breaks=breaks_tlp, limits=limits_tlp)
      } else if(i==2){
         q <- q + 
            xlab("Percent leaf area [%]") +
            scale_x_continuous(breaks=breaks_pla, limits=limits_pla)
      }
   } else if(grepl("twi", type[[i]])){
      q <- 
         ggplot(df[!is.na(df$bins_TWI), ], aes(y = avg, x = num)) +
         geom_point(aes(color=col)) +
         scale_color_manual(values=c("black", "grey")) +
         ggplot2::geom_errorbar(aes(x = num, ymin = sdmin, ymax = sdmax, width=0.25, color=col)) +
         geom_path(aes(x=num, y=avg, color=col, group=1)) +
         scale_x_continuous(breaks=c(2,6,10,14), limits=c(0,16)) +
         xlab("Topographic wetness index") +
         theme_minimal() +
         theme(legend.position = "none",
               axis.text = element_text(size=12),
               axis.title = element_text(size=14))
      
      if(i==3){
         q <- q + 
            ylab("Mean turgor loss point [MPa]") +
            scale_y_continuous(breaks=breaks_tlp, limits=limits_tlp)
      } else if(i==4){
         q <- q + 
            ylab("Percent leaf area [%]") +
            scale_y_continuous(breaks=breaks_pla, limits=limits_pla)
      }
   }
   assign(paste0("plot_", type[i]), q)
}

##3c. Make map of plot using TWI and cored tree locations ####
species <- read.csv("data/core_list_for_neil.csv", stringsAsFactors = FALSE)

cored_points <- SpatialPointsDataFrame(data.frame(species$NAD83_X, species$NAD83_Y), data=species)

plot.new()

png("manuscript/tables_figures/Figure3.png", width=5, height=7, units="in", res=72)
plot(topo, axes=FALSE, box=FALSE, 
     legend.args = list(text="Topographic Wetness Index", side=4, font=2, line=2.5, cex=0.8))
plot(cored_points, pch=20, add=TRUE)
dev.off()

#######################################################################################
#4 Export the graphs ####
library(ggpubr)

quantile(current_ht$height.m, c(.95), na.rm=TRUE) #95% quantile = 35.002m
quant <- data.frame(yintercept = 35.0022, Lines = "95th percentile")

#add this part to each graph:
geom_hline(aes(yintercept = yintercept), linetype = "dashed", quant)

##4a. Figure S1 (TLP and PLA with height and TWI) ####
plot_pla_twi
plot_tlp_twi

traits <- ggarrange(plot_tlp_twi, plot_pla_twi, nrow=1, ncol=2)
ggsave("manuscript/tables_figures/FigureS1.png", width=5, height=7, units="in", traits)

##4b. height profiles ####
NEON_names <- c("wind_plot", "RH_plot", "SAAT_plot", "biotemp_plot")
NEON_order <- c("a", "b", "c", "d")
NEON_order_x <- c(0.5, 35, 7.5, 7.5)
NEON_order_y <- c(57.5, 52.5, 57.5, 57.5)
for (i in seq(along=1:4)){
   assign(NEON_names[[i]],
          NEON_list[[i]] +
             geom_hline(aes(yintercept = yintercept), linetype = "dotted", quant) +
             theme(axis.text = element_text(size=12),
                   axis.title = element_text(size=14),
                   legend.text = element_text(size=12),
                   legend.title = element_text(size=12, face="bold")) +
             annotate(geom="text", x=NEON_order_x[[i]], y=NEON_order_y[[i]], 
                      label = NEON_order[[i]], fontface="bold", size=7)
          )
}

NEON <- ggarrange(wind_plot, RH_plot, SAAT_plot, biotemp_plot,  nrow=1, ncol=4, common.legend = TRUE, legend = "top", align="h")

###format the other height graphs
heights <- 
   heights + 
   geom_hline(aes(yintercept = yintercept), linetype = "dotted", quant) +
   annotate(geom="text", x=0.7, y=57.5, label = "e", fontface="bold", size=7)

plot_pla_ht <- 
   plot_pla_ht + 
   theme(axis.text.y=element_blank(), 
         axis.title.y=element_blank()) +
   geom_hline(aes(yintercept = yintercept), linetype = "dotted", quant) +
   annotate(geom="text", x=9, y=57.5, label = "f", fontface="bold", size=7)

plot_tlp_ht <- 
   plot_tlp_ht + 
   theme(axis.text.y=element_blank(), 
         axis.title.y=element_blank()) +
   geom_hline(aes(yintercept = yintercept), linetype = "dotted", quant) +
   annotate(geom="text", x=-2.75, y=57.5, label = "g", fontface="bold", size=7)


heights_other <- ggarrange(heights, plot_pla_ht, plot_tlp_ht, nrow=1, ncol=3)

###put plots together
png("manuscript/tables_figures/Figure2.png", width=11, height=11, units="in", res=150)
ggarrange(NEON, heights_other, nrow=2, ncol=1)
dev.off()
