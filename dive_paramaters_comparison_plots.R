# Make illustrative plots comparing murre dive paramaters between with
# and without GPS


# Load in summary data ----
load("dive_stats_summary.RData")

# Remove first record which is for murre where data were not collected for both treatments
stats.df.f <- stats.df[-1,]

stats.df.f$type <- NA
stats.df.f$type[stats.df.f$GPS_TDR_order == "GPS_first" & stats.df.f$GPS_TDR_event == 1] <- "GPS"
stats.df.f$type[stats.df.f$GPS_TDR_order == "GPS_first" & stats.df.f$GPS_TDR_event == 2] <- "TDR"
stats.df.f$type[stats.df.f$GPS_TDR_order == "TDR_first" & stats.df.f$GPS_TDR_event == 1] <- "TDR"
stats.df.f$type[stats.df.f$GPS_TDR_order == "TDR_first" & stats.df.f$GPS_TDR_event == 2] <- "GPS"

stats.df.f <- stats.df.f[order(stats.df.f$ring_number, stats.df.f$type),]


# Re-arrange data to individual + deployment type ----
library(reshape2)


library(ggplot2)
library(plyr)

# Plot function -------
plot_change <- function(x, y, ymin, ymax, xmin, xmax, type, xlab = "x", ylab = "y",
                        main = "", fit = FALSE, groupmeans = TRUE){
 
   data_df <- data.frame(x = x, y = y, group = type, ymin = ymin,
                        ymax = ymax, xmin = xmin, xmax = xmax)
  
  
  
  p <- ggplot(data = data_df,aes(x = x, y = x-y, col = factor(group)))
  p <- p + geom_point()
  p <- p + geom_errorbar(aes(ymin = x-ymin, ymax = x-ymax))
  p <- p + geom_errorbarh(aes(xmin = xmin, xmax = xmax))
  p <- p + geom_abline(intercept = 0, slope = 0, 
                       linetype="dashed", size=1.5)
  p <- p + theme_bw()
  p <- p  + labs(list(title = main, x = xlab, y = ylab))
  
  
  if(groupmeans){
    ci_fun_low <- function(ix){
      n <- length(ix)
      sd_calc <- sd(ix)
      e <- qt(0.975,df=n-1)*sd_calc/sqrt(n)
      out.val <- mean(ix)-e
      return(out.val)
    }
    
    ci_fun_high <- function(ix){
      n <- length(ix)
      sd_calc <- sd(ix)
      e <- qt(0.975,df=n-1)*sd_calc/sqrt(n)
      out.val <- mean(ix)+e
      return(out.val)
    }
    # ?qt
    
    # ci_fun_high(data_df$y[data_df$group=="GPS_first"])
    z <- ddply(data_df, .(group), summarise,
               ymin = ci_fun_low(y),
               ymax = ci_fun_high(y),
               xmin = ci_fun_low(x),
               xmax = ci_fun_high(x),
               x = mean(x),
               y = mean(y))
    
    
    p <- p + geom_point(data = z,aes(x = x, y = x-y, colour = group))
    p <- p + geom_errorbarh(data = z,aes(xmin = xmin, xmax = xmax,
                                         colour = group, height = 0.01),
                            linetype="dashed") 
    p <- p + geom_errorbar(data = z,aes(ymin = x-ymin, ymax = x-ymax,
                                        colour = group, width = 0.01),
                           linetype="dashed")
    
    
  }
  # ?geom_errorbar
  
  if(fit == TRUE){
  p <- p  + stat_smooth(method = "lm")
  }
  
  return(p)
}


pdf("Dive_par_plots_means.pdf")

# Descent rate -----
plot_change(x = stats.df.f$velo_down_ms_mean[stats.df.f$type == "TDR"],
            y = stats.df.f$velo_down_ms_mean[stats.df.f$type == "GPS"],
            ymin = stats.df.f$velo_down_ms_ci_low[stats.df.f$type == "GPS"],
            ymax = stats.df.f$velo_down_ms_ci_high[stats.df.f$type == "GPS"],
            xmin = stats.df.f$velo_down_ms_ci_low[stats.df.f$type == "TDR"],
            xmax = stats.df.f$velo_down_ms_ci_high[stats.df.f$type == "TDR"],
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = expression("Descent rate ("~ms^{-1}~")   with TDR only"),
            ylab = expression(Delta~~"Descent rate ("~ms^{-1}~")   with GPS & TDR"),
            main = "Descent rate")

plot_change(x = stats.df.f$velo_down_ms_mean[stats.df.f$GPS_TDR_event == 1],
            y = stats.df.f$velo_down_ms_mean[stats.df.f$GPS_TDR_event == 2],
            ymin = stats.df.f$velo_down_ms_ci_low[stats.df.f$GPS_TDR_event == 2],
            ymax = stats.df.f$velo_down_ms_ci_high[stats.df.f$GPS_TDR_event == 2],
            xmin = stats.df.f$velo_down_ms_ci_low[stats.df.f$GPS_TDR_event == 1],
            xmax = stats.df.f$velo_down_ms_ci_high[stats.df.f$GPS_TDR_event == 1],
            type = stats.df.f$GPS_TDR_order[stats.df.f$GPS_TDR_event == 1],
            xlab = expression("Descent rate ("~ms^{-1}~")   "~1^st~" deployment"),
            ylab = expression(Delta~~"Descent rate ("~ms^{-1}~")   "~2^nd~" deployment"),
            main = "Descent rate")


# Ascent rate -----
plot_change(x = stats.df.f$velo_up_ms_mean[stats.df.f$type == "TDR"],
            y = stats.df.f$velo_up_ms_mean[stats.df.f$type == "GPS"],
            ymin = stats.df.f$velo_up_ms_ci_low[stats.df.f$type == "GPS"],
            ymax = stats.df.f$velo_up_ms_ci_high[stats.df.f$type == "GPS"],
            xmin = stats.df.f$velo_up_ms_ci_low[stats.df.f$type == "TDR"],
            xmax = stats.df.f$velo_up_ms_ci_high[stats.df.f$type == "TDR"],
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = expression("Ascent rate ("~ms^{-1}~")   with TDR only"),
            ylab = expression(Delta~~"Ascent rate ("~ms^{-1}~")   with GPS & TDR"),
            main = "Ascent rate")

plot_change(x = stats.df.f$velo_up_ms_mean[stats.df.f$GPS_TDR_event == 1],
            y = stats.df.f$velo_up_ms_mean[stats.df.f$GPS_TDR_event == 2],
            ymin = stats.df.f$velo_up_ms_ci_low[stats.df.f$GPS_TDR_event == 2],
            ymax = stats.df.f$velo_up_ms_ci_high[stats.df.f$GPS_TDR_event == 2],
            xmin = stats.df.f$velo_up_ms_ci_low[stats.df.f$GPS_TDR_event == 1],
            xmax = stats.df.f$velo_up_ms_ci_high[stats.df.f$GPS_TDR_event == 1],
            type = stats.df.f$GPS_TDR_order[stats.df.f$GPS_TDR_event == 1],
            xlab = expression("Ascent rate ("~ms^{-1}~")   "~1^st~" deployment"),
            ylab = expression(Delta~~"Ascent rate ("~ms^{-1}~")   "~2^nd~" deployment"),
            main = "Ascent rate")



# Dive depth -----
plot_change(x = stats.df.f$depth_max_m_mean[stats.df.f$type == "TDR"],
            y = stats.df.f$depth_max_m_mean[stats.df.f$type == "GPS"],
            ymin = stats.df.f$depth_max_m_ci_low[stats.df.f$type == "GPS"],
            ymax = stats.df.f$depth_max_m_ci_high[stats.df.f$type == "GPS"],
            xmin = stats.df.f$depth_max_m_ci_low[stats.df.f$type == "TDR"],
            xmax = stats.df.f$depth_max_m_ci_high[stats.df.f$type == "TDR"],
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = expression("Dive depth (m)   with TDR only"),
            ylab = expression(Delta~~"Dive depth (m)   with GPS & TDR"),
            main = "Dive depth")

plot_change(x = stats.df.f$depth_max_m_mean[stats.df.f$GPS_TDR_event == 1],
            y = stats.df.f$depth_max_m_mean[stats.df.f$GPS_TDR_event == 2],
            ymin = stats.df.f$depth_max_m_ci_low[stats.df.f$GPS_TDR_event == 2],
            ymax = stats.df.f$depth_max_m_ci_high[stats.df.f$GPS_TDR_event == 2],
            xmin = stats.df.f$depth_max_m_ci_low[stats.df.f$GPS_TDR_event == 1],
            xmax = stats.df.f$depth_max_m_ci_high[stats.df.f$GPS_TDR_event == 1],
            type = stats.df.f$GPS_TDR_order[stats.df.f$GPS_TDR_event == 1],
            xlab = expression("Dive depth (m)   "~1^st~" deployment"),
            ylab = expression(Delta~~"Dive depth (m)   "~2^nd~" deployment"),
            main = "Dive depth")


# Bottom time -----
plot_change(x = stats.df.f$bottom_dur_prop_mean[stats.df.f$type == "TDR"]*100,
            y = stats.df.f$bottom_dur_prop_mean[stats.df.f$type == "GPS"]*100,
            ymin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$type == "GPS"]*100,
            ymax = stats.df.f$bottom_dur_prop_ci_high[stats.df.f$type == "GPS"]*100,
            xmin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$type == "TDR"]*100,
            xmax = stats.df.f$bottom_dur_prop_ci_high[stats.df.f$type == "TDR"]*100,
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = expression("Bottom time (% of dive)   with TDR only"),
            ylab = expression(Delta~~"Bottom time (% of dive)   with GPS & TDR"),
            main = "Bottom time")

plot_change(x = stats.df.f$bottom_dur_prop_mean[stats.df.f$GPS_TDR_event == 1]*100,
            y = stats.df.f$bottom_dur_prop_mean[stats.df.f$GPS_TDR_event == 2]*100,
            ymin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$GPS_TDR_event == 2]*100,
            ymax = stats.df.f$bottom_dur_prop_ci_high[stats.df.f$GPS_TDR_event == 2]*100,
            xmin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$GPS_TDR_event == 1]*100,
            xmax = stats.df.f$bottom_dur_prop_ci_high[stats.df.f$GPS_TDR_event == 1]*100,
            type = stats.df.f$GPS_TDR_order[stats.df.f$GPS_TDR_event == 1],
            xlab = expression("Bottom time (% of dive)   "~1^st~" deployment"),
            ylab = expression(Delta~~"Bottom time (% of dive)   "~2^nd~" deployment"),
            main = "Bottom time")



# Dives per bout -----
plot_change(x = stats.df.f$dives_per_bout_mean[stats.df.f$type == "TDR"] ,
            y = stats.df.f$dives_per_bout_mean[stats.df.f$type == "GPS"] ,
            ymin = stats.df.f$dives_per_bout_ci_low[stats.df.f$type == "GPS"] ,
            ymax = stats.df.f$dives_per_bout_ci_high[stats.df.f$type == "GPS"] ,
            xmin = stats.df.f$dives_per_bout_ci_low[stats.df.f$type == "TDR"] ,
            xmax = stats.df.f$dives_per_bout_ci_high[stats.df.f$type == "TDR"] ,
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = expression("Dives per bout (N)   with TDR only"),
            ylab = expression(Delta~~"Dives per bout (N)   with GPS & TDR"),
            main = "Dives per bout")

plot_change(x = stats.df.f$dives_per_bout_mean[stats.df.f$GPS_TDR_event == 1] ,
            y = stats.df.f$dives_per_bout_mean[stats.df.f$GPS_TDR_event == 2] ,
            ymin = stats.df.f$dives_per_bout_ci_low[stats.df.f$GPS_TDR_event == 2] ,
            ymax = stats.df.f$dives_per_bout_ci_high[stats.df.f$GPS_TDR_event == 2] ,
            xmin = stats.df.f$dives_per_bout_ci_low[stats.df.f$GPS_TDR_event == 1] ,
            xmax = stats.df.f$dives_per_bout_ci_high[stats.df.f$GPS_TDR_event == 1] ,
            type = stats.df.f$GPS_TDR_order[stats.df.f$GPS_TDR_event == 1],
            xlab = expression("Dives per bout (N)   "~1^st~" deployment"),
            ylab = expression(Delta~~"Dives per bout (N)   "~2^nd~" deployment"),
            main = "Dives per bout")

dev.off()