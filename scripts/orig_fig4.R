trees_all_sub <- read.csv(
  paste0("manuscript/tables_figures/trees_all_sub_", metric, ".csv"),
  stringsAsFactors = FALSE); arima_vals=FALSE
  top_models <- read.csv(
    paste0("manuscript/tables_figures/top_models_dAIC_", metric, "_CPout.csv"),
    stringsAsFactors = FALSE)
  top_models <- top_models[top_models$Delta_AICc==0, ]
  
  ##height / TWI no have consistent coeff for either Rs or Rc. Take out for
  ##this figure
  if(metric %in% c("recovery", "resilience")){
    top_models$Modnames <- 
      gsub("height.ln.m\\+|height.ln.m|\\*TWI.ln\\+", "", 
           top_models$Modnames)
  } 
  
  x1966 <- trees_all_sub[trees_all_sub$year == 1966, ]
  x1977 <- trees_all_sub[trees_all_sub$year == 1977, ]
  x1999 <- trees_all_sub[trees_all_sub$year == 1999, ]
  
  model_df <- list(trees_all_sub, x1966, x1977, x1999)
  names(model_df) <- c("trees_all_sub", "x1966", "x1977", "x1999")
  
  fitall <- lmer(top_models[,"Modnames"][1], 
                 data = model_df[[1]], REML=TRUE, 
                 control = lmerControl(optimizer ="Nelder_Mead"))
  fit66 <- lmer(top_models[,"Modnames"][2], 
                data = model_df[[2]], REML=TRUE, 
                control = lmerControl(optimizer ="Nelder_Mead"))
  fit77 <- lmer(top_models[,"Modnames"][3], 
                data = model_df[[3]], REML=TRUE, 
                control = lmerControl(optimizer ="Nelder_Mead"))
  fit99 <- lmer(top_models[,"Modnames"][4], 
                data = model_df[[4]], REML=TRUE, 
                control = lmerControl(optimizer ="Nelder_Mead"))
  
  library(bootpredictlme4)
  predict(fitall, re.form=NA, se.fit=TRUE, nsim=1000)
  
  #plot fits for each drought scenario models separately (and overlay with visreg)
  
  
  if(metric=="resistance"){
    ylab <- "Rt"
    plotlist <- list()
    for(u in 1:4){
      if(u==1){ #height
        var <- "height.ln.m"
        labs <- rev(c("All", "1966"))
        colvals <- rev(c("black", "#FF9999"))
        xlab <- "ln[H]"
        
        v <- visregList(visreg(fit66, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      } else if(u==2){ #TWI
        var <- "TWI.ln"
        labs <- rev(c("ALL", "1977", "1999"))
        colvals <- rev(c("black", "#009900", "#6699CC"))
        xlab <- "ln[TWI]"
        
        v <- visregList(visreg(fit99, var, plot=FALSE),
                        visreg(fit77, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      } else if(u==3){ #PLA
        var <- "PLA_dry_percent"
        labs <- rev(c("All", "1966"))
        colvals <- rev(c("black", "#FF9999"))
        xlab <- expression(PLA[dry])
        
        v <- visregList(
          visreg(fit66, var, plot=FALSE),
          visreg(fitall, var, plot=FALSE),
          labels=labs, 
          collapse=TRUE)
      } else if(u==4){# TLP
        var <- "mean_TLP_Mpa"
        labs <- rev(c("All", "1977"))
        colvals <- rev(c("black", "#009900"))
        xlab <- expression(pi[TLP])
        
        v <- visregList(visreg(fit77, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      }
      
      q <- plot(v, overlay=TRUE, gg=TRUE, 
                partial=FALSE, rug=FALSE,
                fill.par=list(alpha=0.2)) +
        # guides(fill=FALSE) +
        scale_color_manual(
          values = colvals,
          labels= labs,
          name="Droughts") +
        scale_fill_manual(values=colvals) +
        # guides(color=guide_legend(override.aes=list(fill=NA))) +
        ylab(ylab) + xlab(xlab) +
        ylim(0.5, 1.2) + geom_hline(yintercept=1, lty=2) +
        theme_minimal() +
        theme(legend.position="none",
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))
      
      plotlist[[u]] <- q
    }
    names(plotlist) <- c("height", "TWI", "PLA", "TLP")
  }
  if(metric=="recovery"){
    ylab <- "Rc"
    plotlist <- list()
    for(u in 1:2){
      if(u==1){ #ring porosity
        var <- "rp"
        labs <- rev(c("All", "1966", "1999"))
        colvals <- rev(c("black", "#FF9999", "#6699CC"))
        xlab <- "RP"
        
        v <- visregList(visreg(fit99, var, plot=FALSE),
                        visreg(fit66, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      } else if(u==2){ #TLP
        var <- "mean_TLP_Mpa"
        labs <- rev(c("ALL", "1977", "1999"))
        colvals <- rev(c("black", "#009900", "#6699CC"))
        xlab <- expression(pi(TLP))
        
        v <- visregList(visreg(fit99, var, plot=FALSE),
                        visreg(fit77, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      }
      
      q <- plot(v, overlay=TRUE, gg=TRUE, 
                partial=FALSE, rug=FALSE,
                fill.par=list(alpha=0.2)) +
        # guides(fill=FALSE) +
        scale_color_manual(
          values = colvals,
          labels = labs,
          name="Droughts") +
        scale_fill_manual(values=colvals) +
        # guides(color=guide_legend(override.aes=list(fill=NA))) +
        labs(y=ylab) +
        labs(x=xlab) +
        ylim(0.5, 2) + 
        geom_hline(yintercept=1, lty=2) +
        theme_minimal() +
        theme(legend.position="none",
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))
      
      plotlist[[u]] <- q
    }
    names(plotlist) <- c("RP", "TLP")
  }
  if(metric=="resilience"){
    ylab <- "Rs"
    plotlist <- list()
    for(u in 1:3){
      if(u==1){ #ring porosity
        var <- "rp"
        labs <- rev(c("All", "1966", "1977", "1999"))
        colvals <- rev(c("black", "#FF9999", "#009900", "#6699CC"))
        xlab <- "RP"
        
        v <- visregList(visreg(fit99, var, plot=FALSE),
                        visreg(fit77, var, plot=FALSE),
                        visreg(fit66, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      } else if(u==2){ #TLP
        var <- "mean_TLP_Mpa"
        labs <- rev(c("ALL", "1977", "1999"))
        colvals <- rev(c("black", "#009900", "#6699CC"))
        xlab <- expression(pi(TLP))
        
        v <- visregList(visreg(fit99, var, plot=FALSE),
                        visreg(fit77, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      } else if(u==3){ #PLA
        var <- "PLA_dry_percent"
        labs <- rev(c("All", "1966", "1977", "1999"))
        colvals <- rev(c("black", "#FF9999", "#009900", "#6699CC"))
        xlab <- "PLA"
        
        v <- visregList(visreg(fit99, var, plot=FALSE),
                        visreg(fit77, var, plot=FALSE),
                        visreg(fit66, var, plot=FALSE),
                        visreg(fitall, var, plot=FALSE),
                        labels=labs,
                        collapse=TRUE)
      }
      
      q <- plot(v, overlay=TRUE, gg=TRUE, 
                partial=FALSE, rug=FALSE,
                fill.par=list(alpha=0.2)) +
        # guides(fill=FALSE) +
        scale_color_manual(
          values = colvals,
          labels = labs,
          name="Droughts") +
        scale_fill_manual(values=colvals) +
        # guides(color=guide_legend(override.aes=list(fill=NA))) +
        labs(y=ylab) +
        labs(x=xlab) +
        ylim(0.5, 2) + 
        geom_hline(yintercept=1, lty=2) +
        theme_minimal() +
        theme(legend.position="none",
              axis.text = element_text(size=12),
              axis.title = element_text(size=14))
      
      plotlist[[u]] <- q
    }
    names(plotlist) <- c("RP", "TLP", "PLA")
  }
  
  ## make legend
  library(grid)
  library(gridExtra)
  trees_all_sub$year <- "all"
  full <- rbind(trees_all_sub, x1966, x1977, x1999)
  full$color <- 
    ifelse(full$year=="all", "black",
           ifelse(full$year=="1966", "#FF9999",
                  ifelse(full$year=="1977", "#009900","#6699CC")))
  cols <- unique(full$color)
  
  w <- ggplot(full) +
    aes(height.ln.m, metric.value) +
    geom_line(aes(color=year), size=1.2, alpha=0.8) +
    facet_wrap(~year) +
    scale_color_manual(name="Droughts",
                       labels=c("All", "1966", "1977", "1999"),
                       values = cols) +
    theme(legend.text = element_text(size=12),
          legend.title=element_text(size=14))
  legend <- cowplot::get_legend(w)
  
  ## put all together
  png(paste0("manuscript/tables_figures/publication/Figure4_model_vis_",
             metric, ".png"), height=480, width=600)
  if(metric=="resistance"){
    arr <- ggarrange(plotlist[["height"]], plotlist[["TWI"]], 
                     plotlist[["PLA"]], plotlist[["TLP"]],
                     labels = c("(a)", "(b)", "(c)", "(d)"),
                     label.x=0.15,
                     label.y=1,
                     ncol = 2, nrow = 2,
                     common.legend=FALSE)
    
    grid.arrange(arr, legend, ncol=2, widths=c(4,1)) 
  } else if(metric=="recovery"){
    arr <- ggarrange(plotlist[["RP"]], plotlist[["TLP"]],
                     labels = c("(a)", "(b)"),
                     label.x=0.15,
                     label.y=1,
                     # ncol = 2, 
                     nrow = 1,
                     common.legend=FALSE)
    
    grid.arrange(arr, legend, widths=c(6,1))
  } else if(metric=="resilience"){
    arr <- ggarrange(plotlist[["RP"]], plotlist[["TLP"]], plotlist[["PLA"]],
                     labels = c("(a)", "(b)", "(c)"),
                     label.x=0.15,
                     label.y=1,
                     ncol = 3, nrow = 1,
                     common.legend=FALSE)
    
    grid.arrange(arr, legend, widths=c(6,1))
  }
  
  dev.off()