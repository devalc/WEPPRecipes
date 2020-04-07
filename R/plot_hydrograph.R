#' Returns a hydrograph of observed and simulated data and includes
#' precipitation for the same time period
#'
#' @param df Dataframe of 4 coulmns in this specific order: 1.Time/Date 2.Simulated streamflow 3.Observed streamflow 4.Precipitation
#' @param units units of the data
#' @export


plot_hydrograph <- function(df, units = "mm"){

  nse_val = signif(NSeff_function(df[,3],df[,2]),2)
  kge_val = signif( hydroGOF::KGE(df[,3],df[,2]),2)
  R2_val = signif(R2_function(df[,3],df[,2]),2)

  maxSF1  = max(df[,2], na.rm = T)
  maxSF2 = max(df[,3], na.rm = T)
  maxSF = max(c(maxSF1, maxSF2))
  maxPR = max(df[,4], na.rm = T)
  par(mar = c(4, 4, 3, 4) + 0.1, xpd=TRUE)
  plot(df[,1], df[,2],
       type = 'l', lwd=1.2, col = "red",
       xaxs = "i", yaxs = "i",
       ylim = c(0, 1.3 * maxSF),
       xlab = "Time",
       ylab = paste("Streamflow(", units, ")", sep = " "),
       main = paste0("NSE:", nse_val, "       ",
                     "KGE:", kge_val, "       ",
                     "RSq:", R2_val,  "       "), cex.main = 0.9)
  lines(df[,1], df[,3], col = "black")
  legend("right",  legend=c("Qsim", "Qobs"),
         col=c("red", "black"), cex=1, lty = 1,bty = "n",
         text.col = "black",
         xjust = 1,
         x.intersp = 0.25,
         y.intersp = 1)
  par(new = TRUE)
  plot(x =df[,1], y = rep(0, nrow(df)),
       type = "n", ylim = c(5 * maxPR, 0),
       xaxs = "i", yaxs = "i",
       axes = FALSE, xlab = "", ylab = "")
  segments(x0 = df[,1], y0 = rep(0, nrow(df)),
           x1 = df[,1], y1 = df[,4],
           lend = 2, lwd =1, col = "blue")
  yrAxis  <- seq(0, ceiling(maxPR), length.out = 5)
  axis(4, at = yrAxis, labels = paste0(yrAxis))
  mtext(paste("Precip(", units,")"), side = 4, line = 2, adj = 1)
}
