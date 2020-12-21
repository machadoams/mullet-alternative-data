# Setup 

# Load packages and custom functions
#source("./R/setup.R")

# Run analyses
#source("./R/analyses.R")

# FIGURE 2 ====

# Plot correlations between datasets ----
gPairs <- list()  
rlabs <- list()

# create dataframe with year and catch data
tmpData <- dataMullet %>% 
  dplyr::filter(year %in% presYear) %>% 
  dplyr::select('Media reports', 'Self-reporting', 'Industrial fleet', 'Beach seiner community', 'year') 

for(i in 1:length(tmpx)){
  
  # subset collumns to plot
  tmp.v1 <- unique(as.character(adfR[which(adfR$Var1 == tmpx[i]), "Var1"]))
  tmp.v2 <- unique(as.character(adfR[which(adfR$Var2 == tmpy[i]), "Var2"]))
  
  # get r and p value string
  rlabs[[i]] <- bquote(atop(
    italic(r) == .(adfR[which(adfR$Var1 == tmp.v1 & adfR$Var2 == tmp.v2), "r"]),
    italic(p) == .(adfR[which(adfR$Var1 == tmp.v1 & adfR$Var2 == tmp.v2), "Pval"])))
  
  # Create color according to p value
  col.beta <- ifelse(as.numeric(adfR[which(adfR$Var1 == tmp.v1 & adfR$Var2 == tmp.v2),
                                     "Pval"]) < 0.05, "#F4B400", "grey40")
  col.point <- ifelse(as.numeric(adfR[which(adfR$Var1 == tmp.v1 & adfR$Var2 == tmp.v2),
                                      "Pval"]) < 0.05, "black", "grey50")
  
  # Panels to remove axis titles and values
  no.xaxis <- c("Industrial fleet-Self-reporting",
                "Industrial fleet-Media reports",
                "Self-reporting-Media reports")
  
  no.yaxis <- c("Self-reporting-Media reports",
                "Self-reporting-Beach seiner community",
                "Media reports-Beach seiner community")
  
  # name list according to subset
  names(rlabs)[[i]] <- paste0(tmp.v1,"-", tmp.v2)
  
  # subset dataframe to pass to ggplot2
  tmp.df <- data.frame(tmp.v1 = tmpData[, tmp.v1],
                       tmp.v2 = tmpData[, tmp.v2],
                       year = tmpData$year)
  
  # create a list to receive plots
  gPairs[[i]] <- ggplot(data = tmp.df, 
                        aes(x = tmp.v1, y = tmp.v2, label = year)) +
    # add line connecting observations 
    geom_path(linetype = 2, size = 0.25) +
    # add trending line
    geom_smooth(method = "lm", se = FALSE, colour = col.beta) +
    # add points
    geom_point(color = col.point, size = 2) +
    # add text with stats
    annotate("text", fontface = "plain", label = rlabs[[i]],
             x = 0.75 * (max(tmp.df$tmp.v1)),
             y = 0.15 * (max(tmp.df$tmp.v2))) +
    # add labels with years
    geom_text_repel(direction = 'both', colour = col.point, size = 2.5) +
    # chage plot aesthetics
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3), limits = c(0, NA)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3), limits = c(0, NA)) +
    labs(x = tmp.v1, y = tmp.v2) +
    theme(
      axis.text = element_text(size = rel(1), colour = "black"),
      axis.title = element_text(size = rel(1), colour = "black"),
      axis.ticks = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.background = element_rect(fill = NA),
      panel.grid = element_blank(),
      legend.position = "none",
      aspect.ratio = 1,
      panel.spacing = unit(0,"null")
      ) 
  
  if(names(rlabs)[[i]] %in% no.xaxis){
    gPairs[[i]] <- gPairs[[i]] + 
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            plot.margin = margin(t = 0.2, b = 0.2))
  }
  
  if(names(rlabs)[[i]] %in% no.yaxis){
    gPairs[[i]] <- gPairs[[i]] + 
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(b = 0.2, l = 0.2))
  }
  
  # name list according to subset
  names(gPairs)[[i]] <- paste0(tmp.v1,"-", tmp.v2)
  
}

# Combine panels ----
plotCorYield <- cowplot::plot_grid(
  gPairs$`Industrial fleet-Self-reporting`, NULL, NULL,
  gPairs$`Industrial fleet-Media reports`, gPairs$`Self-reporting-Media reports`, NULL,
  gPairs$`Industrial fleet-Beach seiner community`, gPairs$`Self-reporting-Beach seiner community`, gPairs$`Media reports-Beach seiner community`,
  labels = "", ncol = 3, align = "hv"
)


# export pdf
ggsave2(plot = plotCorYield,
        file = "./figures/Fig2_Correlations.pdf",
        height = 7, width = 7)
