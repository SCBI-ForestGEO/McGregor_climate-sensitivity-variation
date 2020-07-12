######################################################
# Purpose: Create conglomerate graphs for McGregor et al 2019
# Developed by: Ian McGregor, contact Anderson-Teixeira (teixeirak@si.edu)
# R version 3.5.3 - First created July 2019
######################################################
library(ggplot2)

#1 Figure 1: Distribution of resistance values and time series
## necessary packages ####
library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(cowplot)

##1a make density plot ####
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv")
trees_all_sub$year <- as.character(trees_all_sub$year)

#In each drought, roughly 30% of the cored trees
#Find n trees that have resistance <= 0.7 per year: #% in 1966, #2% in 1977, and #% in 1999. #Do same thing for those that have resistance >1 for each year
library(data.table)
test <- as.data.table(trees_all_sub)

test_reduct <- test[resist.value<=0.7,.(reduct = length(resist.value)), by=.(year)]
by_year <- test[, .(total=length(resist.value)), by=.(year)]
test_reduct$total_re <- by_year$total
test_reduct$perc_re <- test_reduct$reduct/test_reduct$total_re

test_growth <- test[resist.value>1,.(growth = length(resist.value)), by=.(year)]
test_growth$total_gr <- by_year$total
test_growth$perc_gr <- test_growth$growth/test_growth$total_gr

test_full <- merge(test_reduct, test_growth)

test[,.(ran = range(resist.value, na.rm=TRUE)), by=.(year)]
test[,.(mean = mean(resist.value, na.rm=TRUE)), by=.(year)]

Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
}

# apply(d, 2, Mode)
# test[,.(mode = Mode(resist.value)), by=.(year)]

#save as png so that way you're arranging only png
png("manuscript/tables_figures/publication/Figure1b_density_plot.png", res = 300, width = 150, height = 50, units = "mm", pointsize = 10)
ggplot(trees_all_sub) +
   aes(x = resist.value, fill = year) +
   geom_density(adjust = 1L, alpha=.5) +
   geom_vline(xintercept=1) +
   scale_fill_hue(labels=c("1966", "1977", "1999")) +
   labs(x="resistance value") +
   theme_minimal() +
   annotate(geom="text", x=0.1, y=1.5, 
            label = "(b)", fontface="bold", size=7)
dev.off()

plot1 <- readPNG("manuscript/tables_figures/publication/Figure1a_Time_series_for_each_species.png")
plot2 <- readPNG("manuscript/tables_figures/publication/Figure1b_density_plot.png")

png("manuscript/tables_figures/publication/Figure1.png", res=300, height=200, width=150, units="mm", pointsize=10)
g <- plot_grid(rasterGrob(plot1), rasterGrob(plot2), align = "v", nrow = 2, rel_heights = c(3/4, 1/4), axis = "b")
g + annotate(geom="text", x=0.13, y=0.99, 
             label = "(a)", fontface="bold", size=3) +
   annotate(geom="text", x=0.13, y=0.23, 
             label = "(b)", fontface="bold", size=3)
dev.off()

##1b Figure 2: Distribution by species ####
library(data.table)
rt <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE)
rt$year <- as.character(rt$year)

#all droughts per species
png("manuscript/tables_figures/publication/Figure2_rt_across_sp.png", width=960)
ggplot(rt) +
   aes(x = sp, y = resist.value) +
   geom_boxplot(aes(fill = year), alpha=0.5) +
   scale_color_discrete() +
   ylab("Rt") +
   xlab("Species") +
   theme_minimal() +
   theme(axis.text = element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.text=element_text(size=12),
         legend.title=element_text(size=12))
dev.off()

rt <- as.data.table(rt)
avg_rt <- rt[, .(avg_rt = round(mean(resist.value),2)), by=.(sp,year)]
setnames(avg_rt, old=c("sp", "avg_rt"), new=c("Species", "Mean Rt"))
write.csv(avg_rt ,"manuscript/tables_figures/publication/mean_rt_by_sp.csv", 
          row.names = FALSE)

##########################################################################
## Figure ??? anova results ####
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=TRUE

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

bleh <- lapply(model_df, function(x) aov(resist.value ~ sp, data=x))


anova_out <- lapply(c(1:4), function(x){
   output <- TukeyHSD(bleh[[x]])
   ne <- as.data.frame(output$sp)
   out <- data.frame(occ = rownames(ne),
                     txt = ne$`p adj`)
   colnames(out) <- c("occ", names(model_df)[x])
   out$occ <- as.character(out$occ)
                     
   
   return(out)
})

full <- Reduce(function(...) merge(...,by="occ"), anova_out)

ggplot(full) +
   aes(x=occ, y=trees_all_sub) +
   geom_point(aes(color="all")) +
   geom_point(aes(y=x1966, color="1966")) +
   geom_point(aes(y=x1977, color="1977")) +
   geom_point(aes(y=x1999, color="1999")) +
   geom_jitter() +
   scale_colour_manual(breaks = c("all", "1966", "1977", "1999"),
                       values = c("black", "red", "green", "blue")) +
   ylab("P value") +
   xlab("") +
   geom_abline(slope=0, intercept=0.05) +
   theme(axis.text.x = element_text(angle = 90))




anova_out <- NULL
for(i in 1:4){
   output <- TukeyHSD(bleh[[2]])
   ne <- as.data.frame(output$sp)
   out <- data.frame(drought = names(model_df[2]),
                     occ = rownames(ne),
                     pval = ne$`p adj`)
}

summary(bleh[[1]]) #all years
summary(bleh[[2]]) #1966
summary(bleh[[3]]) #1977
summary(bleh[[4]]) #1999






#########################################################################
#2. Figure 3: NEON vertical height profiles with 
## necessary packages and define 95% quantile height ####
library(ggpubr)
library(extrafont)
library(rasterVis)
library(RCurl)
library(stringi)

loadfonts(device="win") #to get TNR

quantile(current_ht$height.m, c(.95), na.rm=TRUE) #95% quantile = 38.34479m
quant <- data.frame(yintercept = 38.34479, Lines = "95th percentile")

##2a. Get NEON plots ####
source('scripts/vertical_height_neon.R', echo=TRUE)
NEON_list <- list(wind_plot, RH_plot, SAAT_plot) #,"biotemp_plot")
names(NEON_list) <- c("wind_plot", "RH_plot", "SAAT_plot") #,"biotemp_plot

##2b. Format the NEON plots ####
NEON_order <- c("(a)", "(b)", "(c)")
NEON_order_x <- c(0.5, 35, 7.5)
NEON_order_y <- c(57.5, 57.5, 57.5)
for (i in seq(along=1:3)){
   NEON_list[[i]] <-
      NEON_list[[i]] +
      theme_bw(base_size = 16) + 
      # theme_bw(base_family = "serif") + #for TNR font
      geom_hline(aes(yintercept = yintercept), linetype = "longdash", quant) +
      annotate(geom="text", x=NEON_order_x[[i]], y=NEON_order_y[[i]], 
               label = NEON_order[[i]], fontface="bold", size=7)
   if(i==1){
      NEON_list[[i]] <-
         NEON_list[[i]] +
         theme(legend.title = element_blank(),
               legend.box = "vertical",
               legend.position = c(0.8, 0.25),
               legend.background = element_rect(fill=alpha("white", 0.01)),
               legend.text=element_text(size=12),
               legend.key.size=unit(4,"mm"),
               legend.spacing=unit(-0.5,"cm")
         )
   }
   
   if(!i==1){
      NEON_list[[i]] <-
         NEON_list[[i]] +
         theme(legend.position = "none")
   }
   
   if(i==2){
      NEON_list[[i]] <- 
         NEON_list[[i]] +
         theme(axis.title.y = element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())
   }
}

NEON <- ggarrange(NEON_list$wind_plot, NEON_list$RH_plot, NEON_list$SAAT_plot,  nrow=2, ncol=2, align="h")

##2c. Get the boxplots for 2018 heights (this is also prep for #3) ####
trees_all <- read.csv("manuscript/tables_figures/trees_all.csv", stringsAsFactors = FALSE) #these graphs are meant to be for all cored trees, not just the ones being used in analysis

scbi.stem3 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"), stringsAsFactors = FALSE)

scbi.stem3$dbh <- as.numeric(scbi.stem3$dbh)

current_ht <- trees_all[!duplicated(trees_all$tree), ]
current_ht$year <- 2018
current_ht <- current_ht[,c(1:4,16:18)]

current_ht$dbh_old.mm <- scbi.stem3$dbh[match(current_ht$tree, scbi.stem3$tag)]
current_ht$dbh_old.cm <- current_ht$dbh_old.mm/10
current_ht$year_dbh <- ifelse(!is.na(current_ht$dbh_old.cm), 2018, NA)

#get original dbh if they died before 2018 (only need 2013)
scbi.stem2 <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem2.csv"), stringsAsFactors = FALSE)

current_ht$dbh_old.mm <- ifelse(is.na(current_ht$dbh_old.mm),
                                scbi.stem2$dbh[match(current_ht$tree, scbi.stem2$tag)],
                                current_ht$dbh_old.mm)
current_ht$dbh_old.cm <- ifelse(is.na(current_ht$dbh_old.cm),
                                current_ht$dbh_old.mm/10,
                                current_ht$dbh_old.cm)
current_ht$year_dbh <- ifelse(is.na(current_ht$year_dbh), 2013, current_ht$year_dbh)

#get the log dbh and get heights
current_ht$dbh.ln.cm <- log(current_ht$dbh_old.cm)

#linear log-log regression
#the full equation is using all points for which we have data to create the equation, despite that for several species we don't have enough data to get a sp-specific equation

#this csv is created from #4 of canopy_heights.R
height_regr <- read.csv("manuscript/tables_figures/publication/tableS2_height_regression.csv", stringsAsFactors = FALSE)

current_ht$height.ln.m <- NA
for(w in seq(along=height_regr$sp)){
   sp_foc <- height_regr$sp[[w]]
   ht_eq <- height_regr[height_regr$sp == sp_foc, ]
   num <- gsub("\\*x", "", ht_eq$Equations)
   num1 <- as.numeric(stri_extract_first_regex(num, "[[:digit:]].[[:digit:]]+"))
   num2 <- as.numeric(stri_extract_last_regex(num, "[[:digit:]].[[:digit:]]+"))
   
   if(sp_foc %in% current_ht[,"sp"]){
      current_ht$height.ln.m <- 
         ifelse(current_ht$sp == sp_foc,
                num1 + num2*current_ht$dbh.ln.cm,
                current_ht$height.ln.m)
   } else {
      current_ht$height.ln.m <- 
         ifelse(current_ht$sp != sp_foc,
                num1 + num2*current_ht$dbh.ln.cm,
                current_ht$height.ln.m)
   }
}

current_ht$height.m <- exp(current_ht$height.ln.m)
current_ht <- current_ht[order(current_ht$tree, current_ht$year), ]

#graphing height by crown position (for paper)
current_ht <- current_ht[!is.na(current_ht$position_all), ]
current_ht$position_all_abb <- substring(current_ht$position_all, 0, 1)
current_ht$position_all_abb <- toupper(current_ht$position_all_abb)
current_ht$position_all_abb <- factor(current_ht$position_all_abb, levels = c("D", "C", "I", "S"))

heights_box <-
   ggplot(data = current_ht, aes(x = position_all_abb, y = height.m, group = position_all)) +
   # aes(x=position_all, y=height.m, fill=year) +
   geom_boxplot() +
   xlab("Crown position") +
   ylab("Height [m]") +
   scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits=c(0,60)) +
   theme_minimal() +
   theme(axis.text = element_text(size=12)) +
   theme(axis.title = element_text(size=14))
##2d. Format the height boxplot and add to NEON, export ####
heights_box <-
   heights_box + 
   theme_bw(base_size = 16) + 
   # theme_bw(base_family = "serif") + #for TNR font
   geom_hline(aes(yintercept = yintercept), linetype = "longdash", quant) +
   annotate(geom="text", x=0.8, y=57.5, 
            label = "d", fontface="bold", size=7) +
   theme(axis.title.y = element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())

###put plots together
png("manuscript/tables_figures/publication/Figure3_NEON_vars_height_profile.png", width=11, height=11, units="in", res=300)
ggarrange(NEON_list$wind_plot, NEON_list$RH_plot, NEON_list$SAAT_plot, heights_box,
          nrow=2, ncol=2, align="h")
dev.off()

#########################################################################
#3. Figure S1: Export map of plot using TWI and cored tree locations
## necessary packages ####
library(raster)
library(rasterVis)

##3a. create map ####
topo <- raster("data/physical/plot_TWI.tif")

species <- read.csv("data/core_files/core_list.csv", stringsAsFactors = FALSE)
species <- species[!species$sp %in% c("frni", "pist"), ]
species$sp_fact <- as.factor(species$sp)

color_pallete_function <- colorRampPalette(
   colors = c("pink","lightskyblue", "turquoise", "springgreen", "white", "khaki", "gold", "brown"),
   space = "Lab" # Option used when colors do not represent a quantitative scale
)
species_colors <- color_pallete_function(nlevels(species$sp_fact))

cored_points <- SpatialPointsDataFrame(data.frame(species$NAD83_X, species$NAD83_Y), data=species)

plot.new()

png("manuscript/tables_figures/publication/figureS1_location_cored_trees.png", width=5, height=7, units="in", res=300)
levelplot(topo, margin=FALSE, scales=list(draw=FALSE), 
          key=list(space="right",
                   text=list(lab=levels(cored_points@data$sp_fact)),
                   points=list(pch=20, fill=species_colors, col=species_colors)),
          colorkey=list(space="left", width=0.75, height=0.75)) +
   layer(sp.points(cored_points, pch=20, col = species_colors[species$sp_fact]))
dev.off()
########################################################################
#4. Figure S3: height by crown position in 2018
## necessary packages ####
library(RCurl) #2
library(tidyr) #2
library(grid) #2
library(gridExtra) #2
library(data.table)
library(stringi)
library(ggplot2)

##4a. heights for all cored trees ####
##4b. Figure S3: combine heights to make plot with all years ####

trees_all$position_all_abb <- ifelse(trees_all$position_all == "dominant", "D",
                                     ifelse(trees_all$position_all == "co-dominant", "C",
                                            ifelse(trees_all$position_all == "suppressed", "S", "I")))
trees_all_plot <- trees_all[,c("tree", "year", "position_all_abb", "height.m", "dbh_old.cm")]

current_ht$position_all_abb <- ifelse(current_ht$position_all == "dominant", "D",
                                     ifelse(current_ht$position_all == "co-dominant", "C",
                                            ifelse(current_ht$position_all == "suppressed", "S", "I")))
current_ht_sub <- current_ht[,c("tree", "year", "position_all_abb", "height.m", "dbh_old.cm")]


#make plot
heights_allplot <- rbind(trees_all_plot, current_ht_sub)
heights_allplot$position_all_abb <- factor(heights_allplot$position_all_abb, levels=c("D","C","I","S"))
heights_allplot$year <- as.character(heights_allplot$year)
heights_allplot <- heights_allplot[order(heights_allplot$tree, heights_allplot$year), ]

png("manuscript/tables_figures/publication/figureS3_height_plot_analysis.png")
ggplot(na.omit(heights_allplot), aes(position_all_abb, height.m)) +
   geom_boxplot(aes(fill=year)) +
   xlab("Crown position") +
   ylab("Height [m]") +
   ggtitle("Height comparison with only trees in analysis")
dev.off()


heights_allplot$tree <- as.character(heights_allplot$tree)
heights_allplot$position_all_abb <- as.character(heights_allplot$position_all_abb)

########################################################################
#5. Visualizing regression output
library(visreg)
library(lme4)
library(ggpubr)

## https://pbreheny.github.io/visreg/cross.html

trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=FALSE
# trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub_arimaratio.csv", stringsAsFactors = FALSE); arima_vals=TRUE
top_models <- read.csv("manuscript/tables_figures/top_models_dAIC_reform.csv", stringsAsFactors = FALSE)
top_models <- top_models[top_models$Delta_AICc==0, ]

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

test <- c(1:4)
glmm_all <- lapply(test, function(x){
   fit1 <- glmer(top_models[,"Modnames"][x], 
                 data = model_df[[x]], REML=FALSE, 
                 control = lmerControl(optimizer ="Nelder_Mead"))
   return(fit1)
})
names(glmm_all) <- c("trees_all_sub", "x1966", "x1977", "x1999")


for(i in 1:4){
   glmm_all[[i]]@call$data <- model_df[[i]]
   visreg(fit1, xvar="PLA_dry_percent", by="year", ylab="Rt", overlay=TRUE)
}


# main top model only (all droughts combined)
mod <- top_models[,"Modnames"][1]
fit1 <- glmer(mod, 
              data = trees_all_sub, REML=FALSE, 
              control = lmerControl(optimizer ="Nelder_Mead"))

fit1@call$data <- model_df[[1]]

vars <- c("height.ln.m", "TWI.ln", "PLA_dry_percent", "mean_TLP_Mpa")
lab <- c("ln[H]", "ln[TWI]", "PLA", "TLP")
gglist <- list()
for(i in 1:4){
   q <- visreg(fit1, xvar=vars[i], by="year", line=list(col="black"),
               ylab="Rt", xlab=lab[i], 
               overlay=TRUE, gg=TRUE)
   
   if(i %in% c(1,3)){
      q <- q + theme(legend.position="none")
   } else if(i %in% c(2,4)){
      q <- q + 
         ylab("") +
         theme(axis.text.y = element_blank())
   }
   
   gglist[[i]] <- q
}

png("manuscript/tables_figures/publication/top_model_visual.png", width=960, height=960)

ggarrange(gglist[[1]], gglist[[2]], gglist[[3]], gglist[[4]], 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
library(cowplot)
ggdraw() +
   draw_plot(gglist[[1]], x = 0, y = .5, width = .5, height = .5) +
   draw_plot(gglist[[2]], x = .4, y = .5, width = .5, height = .5) +
   draw_plot(gglist[[3]], x = 0, y = 0, width = .5, height = 0.5) +
   draw_plot(gglist[[4]], x = .4, y = 0, width = .5, height = 0.5) +
   draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"), 
                   size = 12,
                   x = c(0, 0.4, 0, 0.4), 
                   y = c(0.95, 0.95, 0.45, 0.45))

library(ggplot2)
library(ggpubr)
trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub.csv", stringsAsFactors = FALSE); arima_vals=FALSE
# trees_all_sub <- read.csv("manuscript/tables_figures/trees_all_sub_arimaratio.csv", stringsAsFactors = FALSE); arima_vals=TRUE
top_models <- read.csv("manuscript/tables_figures/top_models_dAIC_reform.csv", stringsAsFactors = FALSE)
top_models <- top_models[top_models$Delta_AICc==0, ]

trees_all_sub$position_num <- 
   ifelse(trees_all_sub$position_all=="dominant", 1,
   ifelse(trees_all_sub$position_all=="co-dominant", 2,
   ifelse(trees_all_sub$position_all=="intermediate", 3,4)))

x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]

model_df <- list(trees_all_sub, x1966, x1977, x1999)
names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")

vars <- c("height.ln.m", "position_num", "TWI.ln", "PLA_dry_percent", "mean_TLP_Mpa")
lab <- c("ln[H]", "CP", "ln[TWI]", "PLA", "TLP")
gglist <- list()
colors <- c("All" = "black", 
            "1966" = "red", 
            "1977" = "green", "1999" = "blue")
for(i in 1:5){
   gglist[[i]] <- local({
      i <- i
      q <- 
         ggplot(trees_all_sub) +
         aes(x=get(vars[i]), y=resist.value) +
         ylab("Rt") +
         xlab(lab[i]) +
         coord_cartesian(ylim=c(0.5,1.15))
      
      if(i==2){
         q <- q + scale_x_continuous(breaks=c(1,2,3,4),
            labels=c("D", "C", "I", "S"))
      }
      
      if(i %in% c(1:3)){ #1999
         q <- 
            q + 
            geom_smooth(data=x1999, 
                            aes(get(vars[i]), resist.value, color="blue"),
                            method=lm , color=NA, fill="blue", 
                            alpha=0.2, se=TRUE) +
            geom_line(data=x1999, 
                      aes(get(vars[i]), resist.value, color="blue"),
                      stat = "smooth", method = lm,
                      color = "blue", size = 1.5, alpha = 0.2)
      } 
      
      if(i %in% c(2,3,5)){#1977
         q <- 
            q +
            geom_smooth(data=x1977, 
                        aes(get(vars[i]), resist.value, color="green"),
                        method=lm , color=NA, fill="green", alpha=0.2,
                        se=TRUE) +
            geom_line(data=x1977, 
                      aes(get(vars[i]), resist.value, color="green"),
                      stat = "smooth", method = lm,
                      color = "green", size = 1.5, alpha = 0.2)
      }
      
      if(i %in% c(1,4)){#1966
         q <- 
            q +
            geom_smooth(data=x1966, 
                        aes(get(vars[i]), resist.value, color="red"),
                        method=lm , color=NA, fill="red", alpha=0.2,
                        se=TRUE) +
            geom_line(data=x1966, 
                      aes(get(vars[i]), resist.value, color="red"),
                      stat = "smooth", method = lm,
                      color = "red", size = 1.5, alpha = 0.2)
      }
      
      if(i %in% c(1,3,4,5)){#all
         q <- q +
            geom_smooth(method=lm, color="black", fill="black", alpha=0.2,
                        se=TRUE, aes(color="black")) +
            scale_color_identity(
               name = "Droughts",
               breaks = c("black", "red", "green", "blue"),
               labels = c("All", "1966", "1977", "1999"),
               guide = "legend")
         # geom_line(stat = "smooth", method = lm,
         #           color = "black", size = 1.5, alpha = 0.2)
      }
      
      q <- q  
      
      print(q)
   })
}

png("manuscript/tables_figures/publication/test_fig.png", width=480, height=960)
ggarrange(gglist[[1]], gglist[[2]], gglist[[3]], 
          gglist[[4]], gglist[[5]], 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
          ncol = 2, nrow = 3,
          common.legend=TRUE)
dev.off()


trees_all_sub$year <- "all"
full <- rbind(trees_all_sub, x1966, x1977, x1999)
full$color <- 
   ifelse(full$year=="all", "black",
   ifelse(full$year=="1966", "red",
   ifelse(full$year=="1977", "green","blue")))

ggplot(full) +
   aes(height.ln.m, resist.value) +
   geom_line(aes(color=year), size=1.2, alpha=0.4) +
   facet_wrap(~year) +
   scale_color_manual(name="Droughts",
      labels=c("All", "1966", "1977", "1999"),
      values = c("black", "red", "green", "blue"))

#
library(png)
library(grid)
library(gridExtra)
fullplot <- readPNG("manuscript/tables_figures/publication/test_fig.png")
legend <- readPNG("manuscript/tables_figures/legend.png")

fullplot <- rasterGrob(fullplot)
legend <- rasterGrob(legend)

library(cowplot)
png("manuscript/tables_figures/publication/regr_visuals.png",
    width=600, height=960)
ggdraw() +
   draw_plot(fullplot, x = 0, y = 0, width = 1, height = 1) +
   draw_plot(legend, x = .5, y = .15, width = .5, height = .15)
dev.off()


#






########################################################################
# appendix ####
allhei <- as.data.table(heights_allplot)
#height growth showing massive negative growth from 1999 to 2018
test <- allhei[year %in% c("1999", "2018"), .(shrunk = height.m[1] - height.m[2]), 
            by=.(tree, position_all_abb)
            ][, .(perc= sum(shrunk >0, na.rm=TRUE)/sum(shrunk>0 | shrunk <=0, na.rm=TRUE)), 
                  by=.(position_all_abb)]

#avg growth between each of the scenario years
allhei$group <- cut(allhei$year, breaks=3)
test66 <- allhei[year %in% c("1966", "1977"), .(gro = height.m[2] - height.m[1]), by=tree
            ][, .(avg_gro = mean(gro, na.rm=TRUE))]
test77 <- allhei[year %in% c("1977", "1999"), .(gro = height.m[2] - height.m[1]), by=tree
              ][, .(avg_gro = mean(gro, na.rm=TRUE))]
test99 <- allhei[year %in% c("1999", "2018"), .(gro = height.m[2] - height.m[1]), by=tree
              ][, .(avg_gro = mean(gro, na.rm=TRUE))]
avg_growth <- data.frame(g66_77 = test66$avg_gro,
                         g77_99 = test77$avg_gro,
                         g99_18 = test99$avg_gro)













