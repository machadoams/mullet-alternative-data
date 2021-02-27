# Figure 4 ---- 

# load function
source("./R/gplotCompareBSTS.R")

# list with self reporting models
listBSTS_SelfReporting <- list("Linear_Trend+AR+PRED" = fitfa.arltx.self_reporting, 
                               "Linear_Trend+PRED" = fitfa.ltx.self_reporting, 
                               "Linear_Trend+AR" = fitfa.arlt.self_reporting, 
                               "AR" = fitfa.ar.self_reporting,
                               "PRED" = fitfa.self_reporting.x,
                               "Linear_Trend" = fitfa.lt.self_reporting,
                               "AR+PRED" = fitfa.arx.self_reporting)

# list with media reports models
listBSTS_MediaReports <- list("Linear_Trend+AR+PRED" = fitfa.arltx.media_reports, 
                              "Linear_Trend+PRED" = fitfa.ltx.media_reports, 
                              "Linear_Trend+AR" = fitfa.arlt.media_reports, 
                              "AR" = fitfa.ar.media_reports,
                              "PRED" = fitfa.media_reports.x,
                              "Linear_Trend" = fitfa.lt.media_reports, 
                              "AR+PRED" = fitfa.arx.media_reports)

# list with beach seiner community models
listBSTS_BeachSeiner <- list("Linear_Trend+AR+PRED" = fitfa.arltx.beach_seiners, 
                             "Linear_Trend+PRED" = fitfa.ltx.beach_seiners, 
                             "Linear_Trend+AR" = fitfa.arlt.beach_seiners, 
                             "AR" = fitfa.ar.beach_seiners,
                             "PRED" = fitfa.beach_seiners.x,
                             "Linear_Trend" = fitfa.lt.beach_seiners, 
                             "AR+PRED" = fitfa.arx.beach_seiners)

# # Original series plot ----
# originalSeries <- lapply(list("Self Reporting" = listBSTS_SelfReporting, 
#                               "Media Reports" = listBSTS_MediaReports, 
#                               "Beach Seiner" = listBSTS_BeachSeiner),
#                          function(x){
#                            return(data.frame(
#                              year = as.numeric(format(x[[1]]$timestamp.info$timestamps, "%Y")),
#                              yield = x[[1]]$original.series,
#                              row.names = NULL)
#                            )
#                          }
# )
# 
# # color by dataset
# list_plot_originalSeries <- lapply(originalSeries, function(x){
#   ggplot(data = x,
#          aes(x = year, y = yield)) +
#     geom_line() +
#     scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
#     scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
#     labs(y = "Yield", x = "") +
#     theme(axis.text = element_text(size = 8, colour = "black"),
#           axis.title = element_text(size = 8, colour = "black", 
#                                     margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
#           strip.background = element_rect(fill = NA, colour = "black"),
#           panel.background = element_rect(fill = NA),
#           panel.border = element_rect(fill = NA, colour = "black"),
#           legend.key = element_blank())}
# )

# Cumulative errors plot ----
plot_bstsCumulError <- lapply(list("Self Reporting" = listBSTS_SelfReporting, 
                                   "Media Reports" = listBSTS_MediaReports, 
                                   "Beach Seiner" = listBSTS_BeachSeiner),
                              FUN = function(x){gplotCompareBSTS(x, plot = FALSE)}) %>% 
  reshape2::melt(id.vars = c("Model", "Time", "CumulativeErrors")) %>%
  dplyr::mutate(L1 = factor(L1, 
                            levels = c('Self Reporting', 'Media Reports', 'Beach Seiner'),
                            labels = c('Self-Reporting', 'Media Reports', 'Beach Seiner Community'))) %>% 
  ggplot(aes(x = as.numeric(Time), y = CumulativeErrors, group = Model, colour = Model)) +
  geom_line(size = 0.8, aes(linetype = Model)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  facet_wrap(~ L1, scales = 'free_y', ncol = 1) +
  labs(y = "Cumulative absolute error", x = "") +
  theme(axis.text = element_text(size = rel(1), colour = "black"),
        axis.title.y = element_text(size = rel(1), colour = "black", 
                                    margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")),
        #panel.grid.major = element_line(linetype = "dashed", colour = "grey90"),
        strip.background = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        legend.key = element_blank())

# export
ggsave(filename = "./figures/bsts_cumulative_errors_r2.pdf",
       plot = plot_bstsCumulError,
       units = "mm", width = 150, height = 120)