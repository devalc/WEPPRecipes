#' Returns a timeseries plot of observed and simulated data
#'
#' @param x A vector containing time/date
#' @param y1 A vector containing simulated values
#' @param y2 A vector containing observed values
#' @param units units of the data
#' @param plot.var.name String containing the name of the variable being plotted
#' @export


plot_obs_sim_ts <- function(x, y1,y2,
                            plot.var.name= NULL, units = NULL){

  nse_val = signif(NSeff_function(y2,y1),2)
  kge_val = signif( hydroGOF::KGE(y2,y1),2)
  pbias = signif(hydroGOF::pbias(y2,y1),2)
  R2_val = signif(R2_function(y2,y1),2)


  maxSF1  = max(y1, na.rm = T)
  maxSF2 = max(y2, na.rm = T)
  maxSF = max(c(maxSF1, maxSF2))

  par(mar = c(4, 4, 3, 4) + 0.1, xpd=TRUE)
  plot(x, y1,
       type = 'l', lwd=2, col = "red",
       xaxs = "i", yaxs = "i",
       ylim = c(0, 1.3 * maxSF),
       xlab = "Time",
       ylab = paste(plot.var.name,"(", units, ")", sep = " "),
       main = paste0("NSE:", nse_val, "       ",
                     "KGE:", kge_val, "       ",
                     "pbias: ", pbias, "       ",
                     "RSq:", R2_val,  "       "), cex.main = 0.9)
  lines(x, y2, col = "black", lwd=2 )
  legend("topright",
         legend=c(paste0(plot.var.name,"_sim"),
                  paste0(plot.var.name,"_obs")),
         col=c("red", "black"), cex=1, lty = 1,bty = "n",
         text.col = "black",
         xjust = 1,
         x.intersp = 0.25,
         y.intersp = 1)
}
