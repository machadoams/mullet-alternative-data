# SETUP 

#### 1. Install and load packages ####
if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(corrplot)){install.packages("corrplot");library(corrplot)}
if(!require(cowplot)){install.packages("cowplot");library(cowplot)}
if(!require(zoo)){install.packages("zoo")};library(zoo)
if(!require(bsts)){install.packages("bsts")};library(bsts)
if(!require(rgdal)){install.packages("rgdal")};library(rgdal)
if(!require(ggsn)){install.packages("ggsn")};library(ggsn)
if(!require(rerddap)){install.packages("rerddap")};library(rerddap)
if(!require(ncdf4)){install.packages("ncdf4")};library(ncdf4)
if(!require(raster)){install.packages("raster")};library(raster)
if(!require(RColorBrewer)){install.packages("RColorBrewer")};library(RColorBrewer)
if(!require(see)){install.packages("see")};library(see)

#### 2. Custom functions ####

# Gplot catch ----
source("./R/gplotCatch.R")

# Round values ----
roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

# Scale latitude/longitude
scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}

scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}    


#### 3. Custom graphics ####

cols <- brewer.pal(7, "Dark2")

# 3.1. Theme for faceted plots ----

theme_faceted <- theme(
  strip.background = element_blank(), 
  strip.placement = "outside",
  #strip.text = element_blank(),
  axis.text = element_text(size = rel(1), colour = "black"),
  axis.title = element_text(size = rel(1), colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.x = element_line(colour = "black"),
  axis.title.x=element_blank(),
  legend.position = "none",
  panel.spacing = unit(1, "lines"),
  plot.margin = unit(c(0, 0, 0.2, 0.1), "cm")
)

# 3.2. Theme for multipanel grid/grob ----

theme_base <- theme(panel.border = element_rect(color = "black", fill = "NA"),
                    panel.background = element_rect(fill = NA),
                    panel.grid = element_blank(),
                    legend.position = "right",
                    axis.text = element_text(colour = "black", size = rel(1)),
                    axis.title = element_text(colour = "black", size = rel(1)),
                    axis.ticks = element_line(colour = "black"))
