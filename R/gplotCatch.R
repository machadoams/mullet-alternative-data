#' gplotCatch
#' 
#' @description Plot catch vs year + bsts predictions
#'  
#' @param dataset string containing the subCatch level to plot
#' @param pred.dataset data.frame with predictions and confidence interval
#' @param pred.color.l string to set the color of line of predicted values
#' @param pred.color.p string to set the color of points of predicted values 
#' @param ci.color string to set the color of geom_ribbon showing prediction intervals
#' @param ci.alpha value between 0 and 1 to set the transparency of geom_ribbon
#' @param xaxis logical argument to show x axis
#' @param xlab string to label x axis
#' @param ylab string to label y axis


gplotCatch <- function(dataset, pred.dataset, pred.color.l = NULL, pred.color.p = NULL, ci.color = NULL, ci.alpha = NULL, xaxis = FALSE, xlab = NULL, ylab = NULL) {
  
  if(xaxis == FALSE){
    tmp.axis <- element_blank()
  } else {
    tmp.axis <- element_text(size = rel(1), colour = "black")
  }
  
  if(is.null(xlab)){
    xlab <- ""
  } 
  
  if(is.null(ylab)){
    xlab <- ""
  } 
  
  if(is.null(pred.color.l))
    pred.color.l <- "grey70"
  
  if(is.null(pred.color.p))
    pred.color.p <- 0.5
  
  if(is.null(ci.color))
    ci.color <- "grey70"
  
  if(is.null(ci.alpha))
    ci.color <- 0.5
  
  subCatch[[dataset]] %>% 
    ggplot(aes(x = year, y = FishCaughtTon)) +
    
    # sst background
    geom_bar(inherit.aes = FALSE,
             stat = "identity", width = 1, fill = "grey85",
             data = subTimeIntegratedArea[subTimeIntegratedArea$variable == "best",],
             aes(x = year, y = value * normalized_catch[[dataset]])) +
    
    ## confidence interval
    geom_ribbon(inherit.aes = FALSE, data = pred.dataset,
                fill = ci.color, alpha = ci.alpha, # grey ribbon is default
                aes(x = year, ymin = ci.low, ymax = ci.up)) +
    
    # dashed lines 2003-2012
    geom_vline(aes(xintercept = 2013), colour = "grey70", size = 1, linetype = 'dashed') +
    
    # set dual axis
    scale_y_continuous(limits = c(0, ymax[[dataset]]),
                       expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)),
                       sec.axis = sec_axis("Days in optimal SST\n",
                                           trans = ~ . / normalized_catch[[dataset]],
                                           breaks = c(0, 30, 60, 90))) + # add sec axis breaks +
    
    # line of catch data
    geom_line(aes(y = FishCaughtTon, x = year, linetype = "n", colour = source), size = 0.8) +
    geom_point(shape = 22, fill = colMullet[[dataset]], color = "black", size = 3) +
    
    # predicted values
    geom_line(inherit.aes = FALSE, data = pred.dataset, 
              colour = pred.color.l, size = 0.8, alpha = 0.5, #linetype = "dashed",
              aes(y = mean, x = year)) +
    
    geom_point(inherit.aes = FALSE, data = pred.dataset, 
               shape = 23, color = pred.color.p, alpha = 1, fill = pred.color.p, size = 3,
               aes(y = mean, x = year)) +
    
    # set x axis
    scale_x_continuous(expand = c(0, 0), breaks = yrBreaks) +
    
    # set color of lines according to source
    scale_color_manual(values = colMullet[[dataset]]) +
    
    # other theme graphic params
    theme(axis.text.y = element_text(size = rel(1), colour = "black"),
          axis.text.x = tmp.axis,
          axis.title = element_text(size = rel(1), colour = "black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour = NA),
          panel.grid.minor = element_line(colour = NA),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA, colour = "black"),
          legend.position = "none") +
    
    # set xlab and ylab
    labs(x = xlab, y = ylab) 
  
}

