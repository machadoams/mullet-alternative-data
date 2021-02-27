# 1. Setup ====

# Load packages and custom functions
source("./R/setup.R")

# 2. Load data ====

# Load mullet dataset 
dataMullet <- read.csv("./data/dataMullet.csv", 
                       header = TRUE, stringsAsFactors = FALSE) %>% 
  
  # Renaming datasets
  dplyr::select(year,
                'Media reports' = newspaper,
                'Self-reporting' = fepesc,
                'Industrial fleet' = UNIVALI,
                'Beach seiner community' = estaleirinho,
                cpue,
                fleet_size)

# Load SST data 
sstMulletSeason <- read.csv("./data/sst.csv")

# check SST data 
str(sstMulletSeason)

# 3. Calculate SST Time Integrated variable ====

# function to summarise optimal SST mullet season
bestSST <- function(x){sum(x >= 19 & x <= 21)}
underSST <-function(x){sum(x < 19)}
overSST <- function(x){sum(x > 21)}

# Calculate Time Integrated Variable 
TimeIntegratedArea <- sstMulletSeason %>% 
  dplyr::select(time, sst) %>%
  dplyr::group_by(time) %>% 
  dplyr::summarise(sst = mean(sst)) %>% 
  dplyr::group_by(year = format.Date(time, "%Y")) %>%
  dplyr::summarise(best = bestSST(sst),
                   under = underSST(sst),
                   over = overSST(sst)) %>% 
  reshape2::melt(id.vars = "year") %>% 
  dplyr::mutate(year = as.numeric(year),
                variable = factor(variable, levels = c("under", "best", "over"))) %>% 
  dplyr::filter(year < 2017) %>% 
  as.data.frame()

# Calculate SST monthly average (cf. Lemos et al 2016)
sstMonthly <- sstMulletSeason %>% 
  dplyr::select(time, sst) %>%
  dplyr::group_by(time) %>% 
  dplyr::group_by(year = format.Date(time, "%Y"),
                  month = format.Date(time, "%m")) %>%
  dplyr::summarise(sst = mean(sst)) %>% 
  as.data.frame()

# Check SST: Plot SST monthly average and high optimal condition (19-21 Cº, cf. Lemos et al. 2016)
ggplot(data = sstMonthly, 
       aes(x = as.numeric(year), y = sst, group = month, colour = month)) +
  geom_rect(ymin = 19, ymax = 21, xmin = 2000, xmax = 2017, 
            colour = NA, fill = "grey90") +
  geom_point() +
  geom_path() + 
  theme_minimal()


# 4. Correlating datasets =====

# match time interval between datasets from 2003 to 2012
# UNIVALI dataset after 2013 had a different effort (http://propesqweb.acad.univali.br)

presYear <- 2004:2012

# order x and y axis
tmpx <- c("Industrial fleet", "Industrial fleet", "Industrial fleet", "Self-reporting", "Self-reporting", "Media reports")
tmpy <- c("Self-reporting", "Media reports", "Beach seiner community", "Media reports", "Beach seiner community", "Beach seiner community")

dfCor <- dataMullet %>% 
  dplyr::filter(year %in% presYear) %>% 
  dplyr::select('Media reports', 'Self-reporting', 'Industrial fleet', 'Beach seiner community')

adfR <- reshape2::melt(cor(dfCor), value.name = "r") %>% 
  dplyr::mutate(
    r = format(r, digits = 2),
    Pval = format(reshape2::melt(cor.mtest(dfCor)$p)$value, digits = 3)) %>% 
  dplyr::filter(Var1 %in% tmpx & Var2 %in% tmpy) %>% 
  dplyr::filter(Var1 != Var2) 

# Check correlations with p-value lower than 0.05 
print(adfR[which(adfR$Pval < 0.05),])

# 5. BSTS Modeling =====

# select data between 2004-2012 and number of days in the "best" temperature (19-21ºC)
sub.sst <- TimeIntegratedArea[which(TimeIntegratedArea$year %in% presYear &
                                      TimeIntegratedArea$variable == "best"),]

# Binding SST data 
bstsMulletYield <- dataMullet %>% 
  dplyr::inner_join(x = TimeIntegratedArea, by = "year") %>% 
  dplyr::filter(variable == "best") %>% 
  dplyr::rename(sst = 'value')

# last year of systematic monitoring of the industrial fleet
cut.year <- 2012

# get ssst only
ssty <- TimeIntegratedArea[TimeIntegratedArea$variable == "best", c("year", "value")]
names(ssty) <- c("year", "sst")

# get cpue only
industrial.cpue <- bstsMulletYield[, c('year', 'cpue')]

# get fleet size only
fleet.size <- bstsMulletYield[, c('year', 'fleet_size')]

# Format data sets ----

# Self-reporting catches
df_self_reporting <- bstsMulletYield[bstsMulletYield$`Self-reporting` > 0, 
                                     c("year", "Self-reporting")]

df_zoo_self_reporting <- with(subset(df_self_reporting, year <= cut.year), 
                              zoo(`Self-reporting`, 
                                  as.Date(paste(year, "06", "15", sep="-"))))

# Industrial fleet
df_industrial <- bstsMulletYield[bstsMulletYield$`Industrial fleet` > 0, 
                                 c("year", "Industrial fleet")]  

df_zoo_industrial <- with(subset(df_industrial, year <= cut.year),
                          zoo(`Industrial fleet`, 
                              as.Date(paste(year, "06", "15", sep="-"))))

# Media reports 
df_media_reports <- bstsMulletYield[bstsMulletYield$`Media reports` > 0, 
                                    c("year", "Media reports")]

df_zoo_media_reports <- with(subset(df_media_reports, year <= cut.year), 
                             zoo(`Media reports`, 
                                 as.Date(paste(year, "06", "15", sep="-"))))

# Beach seiner community
df_beach_seiners <- bstsMulletYield[bstsMulletYield$`Beach seiner community` > 0, 
                                    c("year", "Beach seiner community")]

df_zoo_beach_seiners <- with(subset(df_beach_seiners, year <= cut.year), 
                             zoo(`Beach seiner community`, 
                                 as.Date(paste(year, "06", "15", sep="-"))))

#_ Self-reporting ----

sst.train.self_reporting <- dplyr::left_join(
  ssty[ssty$year %in% subset(df_self_reporting, year <= cut.year)$year, ], 
  industrial.cpue
  )

# 1) Linear trend, autoregressive, predictors
ssfa.self_reporting <- AddLocalLinearTrend(list(), y = df_zoo_self_reporting)
ssfa.self_reporting <- AddAutoAr(ssfa.self_reporting, y = df_zoo_self_reporting, lags = 3)
fitfa.arltx.self_reporting <- bsts(df_zoo_self_reporting ~ sst  + cpue, 
                           state.specification = ssfa.self_reporting, 
                           data = sst.train.self_reporting, 
                           niter = 10000)

# 2) Linear trend, predictors
ssfa.self_reporting <- AddLocalLinearTrend(list(), y = df_zoo_self_reporting)
fitfa.ltx.self_reporting <- bsts(df_zoo_self_reporting ~ sst + cpue, 
                         state.specification = ssfa.self_reporting, 
                         data = sst.train.self_reporting, 
                         niter=10000)

# 3) Linear trend, autoregressive; no predictors
ssfa.self_reporting <- AddLocalLinearTrend(list(), y = df_zoo_self_reporting)
ssfa.self_reporting <- AddAutoAr(ssfa.self_reporting, y = df_zoo_self_reporting, lags = 3)
fitfa.arlt.self_reporting <- bsts(df_zoo_self_reporting, 
                          state.specification = ssfa.self_reporting,
                          niter = 10000)

# 4) Only autoregressive
ssfa.self_reporting <- AddAutoAr(list(), y = df_zoo_self_reporting, lags = 3)
fitfa.ar.self_reporting <- bsts(df_zoo_self_reporting, 
                        state.specification = ssfa.self_reporting,
                        niter = 10000)

# 5) Only predictors
ssfa.self_reporting <- AddStaticIntercept(list(), y = df_zoo_self_reporting)
fitfa.self_reporting.x <- bsts(df_zoo_self_reporting ~ sst + cpue, 
                              state.specification = ssfa.self_reporting,
                              data = sst.train.self_reporting, 
                              niter=10000)

# 6) Only linear trend
ssfa.self_reporting <- AddLocalLinearTrend(list(), y = df_zoo_self_reporting)
fitfa.lt.self_reporting <- bsts(df_zoo_self_reporting, 
                        state.specification = ssfa.self_reporting,
                        niter = 10000)

# 7) Autoregressive and predictors
ssfa.self_reporting <- AddAutoAr(list(), y=df_zoo_self_reporting, lags = 3)
fitfa.arx.self_reporting <- bsts(df_zoo_self_reporting ~ sst + cpue, 
                         state.specification = ssfa.self_reporting, 
                         data = sst.train.self_reporting, 
                         niter = 10000)

#png("./figures/bsts_self_reporting_.png")
CompareBstsModels(
  list("Linear_Trend+AR+PRED" = fitfa.arltx.self_reporting, 
       "Linear_Tredn+PRED" = fitfa.ltx.self_reporting, 
       "Linear_Trend+AR" = fitfa.arlt.self_reporting, 
       "AR" = fitfa.ar.self_reporting,
       "PRED" = fitfa.self_reporting.x,
       "Linear_Trend" = fitfa.lt.self_reporting, 
       "AR+PRED" = fitfa.arx.self_reporting),
  col = cols, lwd = 3
) 
#dev.off()

# No model much better, but AR+PRED has the lower cumulative error
plot(fitfa.arx.self_reporting, "components")

#png("./figures/bsts_fitfa-arx_self_reporting_.png")
par(mfrow=c(2,2))
plot(fitfa.arx.self_reporting, main = "States")
plot(fitfa.arx.self_reporting, "residuals", main = "Residuals")
plot(fitfa.arx.self_reporting, "predictors", main = "Predictors")
plot(fitfa.arx.self_reporting, "coefficients", main = "Coefficients")
#dev.off()

# Plot self_reporting
burnin <- 1000
plot(density(fitfa.arx.self_reporting$coefficients[burnin:10000, 2]))

sst.test <- ssty[ssty$year > cut.year, ]
sst.test <- dplyr::left_join(sst.test, industrial.cpue)

predfa.arx.self_reporting <- predict(fitfa.arx.self_reporting, 
                             newdata = sst.test, 
                             horizon = 4, burn = burnin, quantiles = c(.025, .975))

plot(predfa.arx.self_reporting, xlab="Year", ylab="Yield", ylim=c(400, 3500))
lines(with(subset(df_self_reporting, year >= cut.year),
           zoo(`Self-reporting`, as.Date(paste(year, "06", "15", sep="-"))) ))
points(with(df_self_reporting,
            zoo(`Self-reporting`, as.Date(paste(year, "06", "15", sep="-"))) ), pch=22, bg="orange")

# _ Media reports ----

# sst
sst.train.media_reports <- dplyr::left_join(
  ssty[ssty$year %in% subset(df_media_reports, year <= cut.year)$year, ], 
  industrial.cpue
  )

# 1) Linear trend, autoregressive, predictors
ssfa.media_reports <- AddLocalLinearTrend(list(), y = df_zoo_media_reports)
ssfa.media_reports <- AddAutoAr(ssfa.media_reports, y = df_zoo_media_reports, lags = 3)
fitfa.arltx.media_reports <- bsts(df_zoo_media_reports ~ sst  + cpue, 
                              state.specification = ssfa.media_reports, 
                              data = sst.train.media_reports, 
                              niter = 10000)

# 2) Linear trend, predictors
ssfa.media_reports <- AddLocalLinearTrend(list(), y = df_zoo_media_reports)
fitfa.ltx.media_reports <- bsts(df_zoo_media_reports ~ sst + cpue, 
                            state.specification = ssfa.media_reports, 
                            data = sst.train.media_reports, 
                            niter=10000)

# 3) Linear trend, autoregressive; no predictors
ssfa.media_reports <- AddLocalLinearTrend(list(), y = df_zoo_media_reports)
ssfa.media_reports <- AddAutoAr(ssfa.media_reports, y = df_zoo_media_reports, lags = 3)
fitfa.arlt.media_reports <- bsts(df_zoo_media_reports, 
                             state.specification = ssfa.media_reports,
                             niter = 10000)

# 4) Only autoregressive
ssfa.media_reports <- AddAutoAr(list(), y = df_zoo_media_reports, lags = 3)
fitfa.ar.media_reports <- bsts(df_zoo_media_reports, 
                           state.specification = ssfa.media_reports,
                           niter = 10000)

# 5) Only predictors
ssfa.media_reports <- AddStaticIntercept(list(), y = df_zoo_media_reports)
fitfa.media_reports.x <- bsts(df_zoo_media_reports ~ sst + cpue, 
                              state.specification = ssfa.media_reports,
                              data = sst.train.media_reports, 
                              niter=10000)

# 6) Only linear trend
ssfa.media_reports <- AddLocalLinearTrend(list(), y = df_zoo_media_reports)
fitfa.lt.media_reports <- bsts(df_zoo_media_reports, 
                           state.specification = ssfa.media_reports,
                           niter = 10000)

# 7) Autoregressive and predictors
ssfa.media_reports <- AddAutoAr(list(), y=df_zoo_media_reports, lags = 3)
fitfa.arx.media_reports <- bsts(df_zoo_media_reports ~ sst + cpue, 
                            state.specification = ssfa.media_reports, 
                            data = sst.train.media_reports, 
                            niter = 10000)

#png("./figures/bsts_media_reports_.png")
CompareBstsModels(
  list("Linear_Trend+AR+PRED" = fitfa.arltx.media_reports, 
       "Linear_Tredn+PRED" = fitfa.ltx.media_reports, 
       "Linear_Trend+AR" = fitfa.arlt.media_reports, 
       "AR" = fitfa.ar.media_reports,
       "PRED" = fitfa.media_reports.x,
       "Linear_Trend" = fitfa.lt.media_reports, 
       "AR+PRED" = fitfa.arx.media_reports),
  col = cols, lwd = 3)
#dev.off()


# No model much better, but AR+PRED has the lower cumulative error
plot(fitfa.arx.media_reports, "components")

#png("./figures/bsts_fitfa-arx_media_reports_.png")
par(mfrow=c(2,2))
plot(fitfa.arx.media_reports, main = "States")
plot(fitfa.arx.media_reports, "residuals", main = "Residuals")
plot(fitfa.arx.media_reports, "predictors", main = "Predictors")
plot(fitfa.arx.media_reports, "coefficients", main = "Coefficients")
#dev.off()

# Plot media_reports
burnin <- 1000
plot(density(fitfa.arx.media_reports$coefficients[burnin:10000, 2]))

sst.test <- ssty[ssty$year > cut.year, ]
sst.test <- dplyr::left_join(sst.test, industrial.cpue)

predfa.arx.media_reports <- predict(fitfa.arx.media_reports, 
                                newdata = sst.test, 
                                horizon = 4, burn = burnin, quantiles = c(.025, .975))

plot(predfa.arx.media_reports, xlab="Year", ylab="Yield")
lines(with(subset(df_media_reports, year >= cut.year),
           zoo(`Media reports`, as.Date(paste(year, "06", "15", sep="-"))) ))
points(with(df_media_reports,
            zoo(`Media reports`, as.Date(paste(year, "06", "15", sep="-"))) ), pch=22, bg="orange")

# _ Beach seiner community ----

# sst
sst.train.beach_seiners <- dplyr::left_join(
  ssty[ssty$year %in% subset(df_beach_seiners, year <= cut.year)$year, ], 
  industrial.cpue
  )

# 1) Linear trend, autoregressive, predictors
ssfa.beach_seiners <- AddLocalLinearTrend(list(), y = df_zoo_beach_seiners)
ssfa.beach_seiners <- AddAutoAr(ssfa.beach_seiners, y = df_zoo_beach_seiners, lags = 3)
fitfa.arltx.beach_seiners <- bsts(df_zoo_beach_seiners ~ sst  + cpue, 
                                 state.specification = ssfa.beach_seiners, 
                                 data = sst.train.beach_seiners, 
                                 niter = 10000)

# 2) Linear trend, predictors
ssfa.beach_seiners <- AddLocalLinearTrend(list(), y = df_zoo_beach_seiners)
fitfa.ltx.beach_seiners <- bsts(df_zoo_beach_seiners ~ sst + cpue, 
                               state.specification = ssfa.beach_seiners, 
                               data = sst.train.beach_seiners, 
                               niter=10000)

# 3) Linear trend, autoregressive; no predictors
ssfa.beach_seiners <- AddLocalLinearTrend(list(), y = df_zoo_beach_seiners)
ssfa.beach_seiners <- AddAutoAr(ssfa.beach_seiners, y = df_zoo_beach_seiners, lags = 3)
fitfa.arlt.beach_seiners <- bsts(df_zoo_beach_seiners, 
                                state.specification = ssfa.beach_seiners,
                                niter = 10000)

# 4) Only autoregressive
ssfa.beach_seiners <- AddAutoAr(list(), y = df_zoo_beach_seiners, lags = 3)
fitfa.ar.beach_seiners <- bsts(df_zoo_beach_seiners, 
                              state.specification = ssfa.beach_seiners,
                              niter = 10000)

# 5) Only predictors
ssfa.beach_seiners <- AddStaticIntercept(list(), y = df_zoo_beach_seiners)
fitfa.beach_seiners.x <- bsts(df_zoo_beach_seiners ~ sst + cpue, 
                             state.specification = ssfa.beach_seiners,
                             data = sst.train.beach_seiners, 
                             niter=10000)


# 6) Only linear trend
ssfa.beach_seiners <- AddLocalLinearTrend(list(), y = df_zoo_beach_seiners)
fitfa.lt.beach_seiners <- bsts(df_zoo_beach_seiners, 
                              state.specification = ssfa.beach_seiners,
                              niter = 10000)

# 7) Autoregressive and predictors
ssfa.beach_seiners <- AddAutoAr(list(), y=df_zoo_beach_seiners, lags = 3)
fitfa.arx.beach_seiners <- bsts(df_zoo_beach_seiners ~ sst + cpue, 
                               state.specification = ssfa.beach_seiners, 
                               data = sst.train.beach_seiners, 
                               niter = 10000)


#png("./figures/bsts_beach_seiners_.png")
CompareBstsModels(
  list("Linear_Trend+AR+PRED" = fitfa.arltx.beach_seiners, 
       "Linear_Tredn+PRED" = fitfa.ltx.beach_seiners, 
       "Linear_Trend+AR" = fitfa.arlt.beach_seiners, 
       "AR" = fitfa.ar.beach_seiners,
       "PRED" = fitfa.beach_seiners.x,
       "Linear_Trend" = fitfa.lt.beach_seiners, 
       "AR+PRED" = fitfa.arx.beach_seiners),
  col = cols, lwd = 3
) 
#dev.off()

# No model much better, but AR+PRED has the lower cumulative error
plot(fitfa.arx.beach_seiners, "components")

#png("./figures/bsts_fitfa-arx_beach_seiners_.png")
par(mfrow = c(2,2))
plot(fitfa.arx.beach_seiners, main = "States")
plot(fitfa.arx.beach_seiners, "residuals", main = "Residuals")
plot(fitfa.arx.beach_seiners, "predictors", main = "Predictors")
plot(fitfa.arx.beach_seiners, "coefficients", main = "Coefficients")
#dev.off()

# Plot beach_seiners
burnin <- 1000
plot(density(fitfa.arx.beach_seiners$coefficients[burnin:10000, 2]))

sst.test <- ssty[ssty$year > cut.year, ]
sst.test <- dplyr::left_join(sst.test, industrial.cpue)

predfa.arx.beach_seiners <- predict(fitfa.arx.beach_seiners, 
                                   newdata = sst.test, 
                                   horizon = 4, burn = burnin, quantiles = c(.025, .975))

plot(predfa.arx.beach_seiners, xlab="Year", ylab="Yield")
lines(with(subset(df_beach_seiners, year >= cut.year),
           zoo(`Beach seiner community`, as.Date(paste(year, "06", "15", sep="-"))) ))
points(with(df_beach_seiners,
            zoo(`Beach seiner community`, as.Date(paste(year, "06", "15", sep="-"))) ), pch=22, bg="orange")

# 6. Predictions  ----

bsts.pred.models <- list(
  "Self-reporting" = predict(fitfa.arx.self_reporting, 
                     newdata = sst.test, 
                     horizon = 4, burn = burnin, quantiles = c(.025, .975)),
  
  "Media reports" = predict(fitfa.arx.media_reports, 
                        newdata = sst.test, 
                        horizon = 4, burn = burnin, quantiles = c(.025, .975)),
  
  "Beach seiner community" = predict(fitfa.arx.beach_seiners, 
                           newdata = sst.test, 
                           horizon = 4, burn = burnin, quantiles = c(.025, .975)))

