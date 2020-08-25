# Based on: https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html

# Load packages and custom functions
source("./R/setup.R")


# The information for the NOAA OISST data
info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
     url = "https://www.ncei.noaa.gov/erddap/")

# This function expects the user to provide it with two values 
# that match the time format of the target OISST dataset
OISST_sub <- function(times){
  oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
                       url = "https://www.ncei.noaa.gov/erddap/", 
                       time = times, 
                       depth = c(0, 0),
                       latitude = c(-28.875, -25.875),
                       longitude = c(310.125, 312.125),
                       fields = "sst")
}

OISST1 <- OISST_sub(c("2000-01-01T00:00:00Z", "2005-12-31T00:00:00Z"))
OISST2 <- OISST_sub(c("2006-01-01T00:00:00Z", "2010-12-31T00:00:00Z"))
OISST3 <- OISST_sub(c("2011-01-01T00:00:00Z", "2017-12-31T00:00:00Z"))

# bind all data
sst <- rbind(OISST1$data, OISST2$data, OISST3$data)
sst <- sst[!is.nan(sst$sst), ]

# subset data to mullet season
sst$time <- as.Date(sst$time, "%Y-%m-%d")
sst.mullet.season <- sst[months(sst$time) %in% c("maio", "junho", "julho"),] # may, june, july

# export csv 
write.csv(sst.mullet.season, "./data/sst.csv", row.names=F)
