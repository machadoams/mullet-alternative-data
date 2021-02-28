# Setup 

# Load packages and custom functions
#source("./R/setup.R")

# Run analyses
#source("./R/analyses.R")

# FIGURE 3 ====

# color by source
colMullet <- c('Beach seiner community' = "#56B4E9", # light blue (estaleirinho),
               'Self-reporting' = "#E69F00", # orange (fepesc)
               'Media reports' = "#0072B2", # dark grey (newspaper)
               'Industrial fleet' = "#D55E00") # black (industrial)

# Suitability (TimeIntegratedArea) 
maxObsSST <- TimeIntegratedArea %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(maxobs = sum(value)) %>% 
  dplyr::select(maxobs) %>% 
  max()


# Normalize data for secondary axis plot
subCatch <- list()
postCatch <- list()
normalized_catch <- list()
ymax <- list()
gplotYield <- list()

# set x axis interval
yrBreaks <- seq(2000, 2017, 3)

# set col names to match
rname <- function(x){setNames(x, nm = c("year", "FishCaughtTon"))}

dataMulletYearYield <- list(
  "Industrial fleet" = dataMullet[, c("year", "Industrial fleet")],
  "Self-reporting" = dataMullet[, c("year", "Self-reporting")],
  "Media reports" = dataMullet[, c("year", "Media reports")],
  "Beach seiner community" = dataMullet[, c("year", "Beach seiner community")]
)

for(i in names(colMullet)){
  
  # subset dataframes
  subCatch[[i]] <- rname(dataMulletYearYield[[i]])  %>% 
    dplyr::filter(year < 2017,
                  FishCaughtTon > 0)  
    
    
  subCatch[[i]][, "source"] <- i
  
  
  # calc normalizer for dual axis plots
  if(i == "Industrial fleet"){
    
    postCatch[[i]] <- dplyr::left_join(subCatch[["Industrial fleet"]],
                                       y = dplyr::left_join(industrial.cpue, fleet.size), 
                                       by = "year") %>% 
      dplyr::filter(year > 2012)
    
    subCatch[[i]] <- dplyr::left_join(subCatch[["Industrial fleet"]],
                                      y = dplyr::left_join(industrial.cpue, fleet.size), 
                                      by = "year") %>% 
      dplyr::filter(year <= 2012)
    
    normalized_catch[[i]] <- roundUp(max(c(subCatch[[i]]$cpue,
                                           postCatch[[i]]$cpue)), to = 10) / maxObsSST
    
    ymax[i] <- roundUp(max(c(max(subCatch[[i]]$cpue), max(postCatch[[i]]$cpue))))
    
  } else {
    
    tmp <- max(bsts.pred.models[[i]]$interval["97.5%",]) 
    if(tmp > 1000)
      tmp.round <- 1000
    if(tmp < 1000)
      tmp.round <- 100
    if(tmp < 100)
      tmp.round <- 50
    
    normalized_catch[i] <- roundUp(tmp, to = tmp.round) / maxObsSST
    
    ymax[i] <- roundUp((max(c(max(subCatch[[i]]$FishCaughtTon), 
                             max(postCatch[[i]]$FishCaughtTon),
                             max(bsts.pred.models[[i]]$interval["97.5%",])))),
                        to = tmp.round)
    
  }
}

subTimeIntegratedArea <- TimeIntegratedArea %>% 
  dplyr::filter(year %in% 1999:2017)


#__ Industrial fleet ----
gplotYield$industrial <- subCatch[["Industrial fleet"]] %>% 
  ggplot(aes(x = year, y = cpue)) +
  
  # sst background
  geom_bar(inherit.aes = FALSE,
           stat = "identity", width = 1, fill = "grey85",
           data = subTimeIntegratedArea[subTimeIntegratedArea$variable == "best",],
           aes(x = year, y = value * normalized_catch$`Industrial fleet`)) +
  
  # dashed lines 2003-2012
  geom_vline(aes(xintercept = 2013), colour = "grey70", size = 1, linetype = 'dashed') +
  # set dual axis
  scale_y_continuous(limits = c(0, ymax$`Beach seiner community`),
                     expand = expand_scale(mult=c(0, .1), add=c(0,0)),
                     sec.axis = sec_axis("Days in optimal SST\n",
                                         trans = ~ . / normalized_catch[["Industrial fleet"]], 
                                         breaks = seq(0, 90, by = 30))) +
  # line of catch data
  geom_line(aes(y = cpue, x = year, linetype = "n",
                colour = source), size = 0.8, show.legend=FALSE) +
  geom_point(shape = 21, fill = colMullet[["Industrial fleet"]], color = "black", 
             aes(size = fleet_size)) + # set size by fleet size
  
  # catch data after 2012
  geom_line(inherit.aes = FALSE, data = postCatch$`Industrial fleet`, linetype = "dashed", 
            aes(y = cpue, x = year, alpha = 0.5, colour = source), show.legend=FALSE) +
  geom_point(inherit.aes = FALSE, data = postCatch$`Industrial fleet`, 
             aes(y = cpue, x = year, alpha = 0.5, colour = source, size = fleet_size),
             shape = 21, fill = colMullet[["Industrial fleet"]], color = "black") +
  scale_alpha(guide = FALSE) +
  scale_size(breaks = c(25, 75, 125), range = c(1,6)) +
  
  # set x axis
  scale_x_continuous(expand = c(0, 0), breaks = yrBreaks) +
  
  # set color of lines according to source
  scale_color_manual(values = colMullet[["Industrial fleet"]]) +
  
  # set color of sst background
  scale_fill_manual(values = c("#9E9E9E")) +
 
  # other theme graphic params
  theme(axis.text.y = element_text(size = rel(1), colour = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = rel(1), colour = "black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = NA),
        legend.position = c(0.9, 0.75),
        legend.title = element_text(color = "black", size = rel(1)),
        legend.text = element_text(color = "black", size = rel(1)),
        legend.justification = "center",
        legend.key = element_blank(),
        legend.box.background = element_rect(fill = 'grey95'),
        legend.background = element_blank()) +
  
  # set xlab and ylab
  labs(size = "Fleet size",
       y = "CPUE (ton/fishing trips)\n", 
       x = "") 

#__ Self-reporting ----

# get predictions and ci
pred.self_reporting <- data.frame(
  ci.low = bsts.pred.models$`Self-reporting`$interval[1,],
  ci.up = bsts.pred.models$`Self-reporting`$interval[2,],
  mean = bsts.pred.models$`Self-reporting`$mean,
  median = bsts.pred.models$`Self-reporting`$median,
  year = sst.test$year)

# limit CI's lower than 0 (check supplementary material for values)
pred.self_reporting$ci.low[pred.self_reporting$ci.low < 0] <- 0

# plot Self-reporting
gplotYield[["Self-reporting"]] <- gplotCatch(dataset = "Self-reporting", 
                                             pred.dataset = pred.self_reporting, 
                                             ylab = "Yield (ton)\n",
                                             xaxis = FALSE,
                                             pred.color.l = "black",
                                             pred.color.p = "black",
                                             ci.color = colMullet['Self-reporting'],
                                             ci.alpha = 0.3)  


#__ Media reports ----

# get predictions and ci
pred.media_reports <- data.frame(
  ci.low = bsts.pred.models$`Media reports`$interval[1,],
  ci.up = bsts.pred.models$`Media reports`$interval[2,],
  mean = bsts.pred.models$`Media reports`$mean,
  median = bsts.pred.models$`Media reports`$median,
  year = sst.test$year)

# limit CI's lower than 0 (check supplementary material for values) 
pred.media_reports$ci.low[pred.media_reports$ci.low < 0] <- 0

# plot Media reports
gplotYield[["Media reports"]] <- gplotCatch(dataset = "Media reports", 
                                            pred.dataset = pred.media_reports,
                                            ylab = "Yield (ton)",
                                            xaxis = FALSE,
                                            pred.color.l = "black",
                                            pred.color.p = "black",
                                            ci.color = colMullet['Media reports'],
                                            ci.alpha = 0.3)  


#__ Beach seiner community ----

# get predictions and ci
pred.beach_seiners <- data.frame(
  ci.low = bsts.pred.models$`Beach seiner community`$interval[1,],
  ci.up = bsts.pred.models$`Beach seiner community`$interval[2,],
  mean = bsts.pred.models$`Beach seiner community`$mean,
  median = bsts.pred.models$`Beach seiner community`$median,
  year = sst.test$year) 

# limit CI's lower than 0 (check supplementary material for values) 
pred.beach_seiners$ci.low[pred.beach_seiners$ci.low < 0] <- 0

# plot Beach seiner community
gplotYield[["Beach seiner community"]] <- gplotCatch(dataset = "Beach seiner community", 
                                                     pred.dataset = pred.beach_seiners, 
                                                     ylab = "Yield (ton)",
                                                     xaxis = TRUE,
                                                     pred.color.l = "black",
                                                     pred.color.p = "black",
                                                     ci.color = colMullet['Beach seiner community'],
                                                     ci.alpha = 0.3)  

#___ Combine plot -----
plotYearYield <- cowplot::plot_grid(
  gplotYield$industrial, 
  gplotYield$`Self-reporting`, 
  gplotYield$`Media reports`, 
  gplotYield$`Beach seiner community`,
  ncol = 1, 
  align = "hv",
  label_fontface = "plain",
  labels = c("(a)", "(b)", "(c)", "(d)"))

# export pdf
ggsave2(plot = plotYearYield,
        file = "./figures/Fig4_yield_bsts_r2.pdf",
        height = 11, width = 6.5)
