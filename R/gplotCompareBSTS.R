# select models
listSelfReporting <- list("Linear_Trend+AR+PRED" = fitfa.arltx.self_reporting, 
                          "Linear_Tredn+PRED" = fitfa.ltx.self_reporting, 
                          "Linear_Trend+AR" = fitfa.arlt.self_reporting, 
                          "AR" = fitfa.ar.self_reporting,
                          "PRED" = fitfa.self_reporting.x,
                          "Linear_Trend" = fitfa.lt.self_reporting, 
                          "AR+PRED" = fitfa.arx.self_reporting)

list_models <- listSelfReporting

gplotCompareBSTS <- function(list_models, cutpoint = NULL, burn = NULL, plot = TRUE){
  
  # check list_models as in bsts::CompareBstsModels()
  stopifnot(is.list(list_models))
  stopifnot(length(list_models) > 1)
  stopifnot(all(sapply(list_models, inherits, "bsts")))
  if (HasDuplicateTimestamps(list_models[[1]])) {
    stop("CompareBstsModels does not support duplicate timestamps.")
  }
  
  if(is.null(burn))
    burn <- SuggestBurn(0.1, list_models[[1]])
  
  time.dimension <- sapply(list_models, function(m) {
    dim(m$state.contributions)[3]
  })
  
  stopifnot(all(time.dimension == time.dimension[1]))
  
  model.names <- names(list_models)
  
  if (is.null(model.names)) {
    model.names <- paste("Model", 1:length(list_models))
  }
  
  number.of.models <- length(list_models)

  errors <- bsts.prediction.errors(list_models[[1]], burn = burn)$in.sample
  
  cumulative.errors <- matrix(nrow = number.of.models, ncol = ncol(errors))
  
  for (i in 1:number.of.models) {
    if (is.null(cutpoint)) {
      prediction.errors <- bsts.prediction.errors(list_models[[i]], 
                                                  burn = burn)$in.sample
      }
    else {
      prediction.errors <- bsts.prediction.errors(list_models[[i]], 
                                                  burn = burn, cutpoints = cutpoint)[[1]]
      }
    cumulative.errors[i, ] <- cumsum(abs(colMeans(prediction.errors)))
    }
  
  colnames(cumulative.errors) <- as.numeric(format(list_models[[1]]$timestamp.info$timestamps, "%Y"))
  rownames(cumulative.errors) <- model.names
  
  tmp.df <- reshape2::melt(cumulative.errors, value.name = "CumulativeErrors", varnames = c('Model', 'Time'))
  
if(isTRUE(plot)){
  
  if (is.null(colors)) 
    tmp.colors <- RColorBrewer::brewer.pal(number.of.models, "Dark2")
  
  tmp.plot <- ggplot(data = tmp.df,
                     aes(x = Time, y = CumulativeErrors, group = Model, colour = Model)) +
    geom_line(size = 1.2) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_colour_manual(values = tmp.colors) +
    labs(y = "Cumulative absolute error", x = "") +
    theme(axis.text = element_text(size = 12, colour = "black"),
          axis.title.y = element_text(size = 12, colour = "black", margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")),
          panel.grid.major = element_line(linetype = "dashed", colour = "grey85"),
          panel.grid.minor = element_line(linetype = "dashed", colour = "grey85"),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA, colour = "black"),
          legend.key = element_blank())
  }
  
  if(isTRUE(plot)){
    return(tmp.plot)
    }
  else {
    return(tmp.df)
  }
}