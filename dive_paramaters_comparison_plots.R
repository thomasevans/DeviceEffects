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


# Compare dive depth ------
library(ggplot2)

dive_depth_df <- data.frame(x = stats.df.f$depth_max_m_mean[stats.df.f$type == "TDR"],
                            y = stats.df.f$depth_max_m_mean[stats.df.f$type == "GPS"],
                            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "GPS"],
                 ymin = stats.df.f$depth_max_m_ci_low[stats.df.f$type == "GPS"],
                 ymax = stats.df.f$depth_max_m_ci_high[stats.df.f$type == "GPS"],
                 xmin = stats.df.f$depth_max_m_ci_low[stats.df.f$type == "TDR"],
                 xmax = stats.df.f$depth_max_m_ci_high[stats.df.f$type == "TDR"])


p <- ggplot(data = dive_depth_df,aes(x = x,y = y, col = type))
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymin = ymin,ymax = ymax))
p <- p + geom_errorbarh(aes(xmin = xmin,xmax = xmax))
p <- p + geom_abline(intercept = 0, slope = 1, color="red", 
                       linetype="dashed", size=1.5)

p


# Compare descent rate ------
dive_depth_df <- data.frame(x = stats.df.f$velo_down_ms_mean[stats.df.f$type == "TDR"],
                            y = stats.df.f$velo_down_ms_mean[stats.df.f$type == "GPS"],
                            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "GPS"],
                            ymin = stats.df.f$velo_down_ms_ci_low[stats.df.f$type == "GPS"],
                            ymax = stats.df.f$velo_down_ms_ci_high[stats.df.f$type == "GPS"],
                            xmin = stats.df.f$velo_down_ms_ci_low[stats.df.f$type == "TDR"],
                            xmax = stats.df.f$velo_down_ms_ci_high[stats.df.f$type == "TDR"])


p <- ggplot(data = dive_depth_df,aes(x = x,y = y, col = type))
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymin = ymin,ymax = ymax))
p <- p + geom_errorbarh(aes(xmin = xmin,xmax = xmax))
p <- p + geom_abline(intercept = 0, slope = 1, color="red", 
                     linetype="dashed", size=1.5)
p


# Compare ascent rate ------
dive_depth_df <- data.frame(x = stats.df.f$velo_up_ms_mean[stats.df.f$type == "TDR"],
                            y = stats.df.f$velo_up_ms_mean[stats.df.f$type == "GPS"],
                            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "GPS"],
                            ymin = stats.df.f$velo_up_ms_ci_low[stats.df.f$type == "GPS"],
                            ymax = stats.df.f$velo_up_ms_ci_high[stats.df.f$type == "GPS"],
                            xmin = stats.df.f$velo_up_ms_ci_low[stats.df.f$type == "TDR"],
                            xmax = stats.df.f$velo_up_ms_ci_high[stats.df.f$type == "TDR"])


p <- ggplot(data = dive_depth_df,aes(x = -x,y = -y, col = type))
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymin = -ymin,ymax = -ymax))
p <- p + geom_errorbarh(aes(xmin = -xmin,xmax = -xmax))
p <- p + geom_abline(intercept = 0, slope = 1, color="red", 
                     linetype="dashed", size=1.5)
p



# Compare dives per bout ------
dive_depth_df <- data.frame(x = stats.df.f$dives_per_bout_mean[stats.df.f$type == "TDR"],
                            y = stats.df.f$dives_per_bout_mean[stats.df.f$type == "GPS"],
                            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "GPS"],
                            ymin = stats.df.f$dives_per_bout_ci_low[stats.df.f$type == "GPS"],
                            ymax = stats.df.f$dives_per_bout_ci_high[stats.df.f$type == "GPS"],
                            xmin = stats.df.f$dives_per_bout_ci_low[stats.df.f$type == "TDR"],
                            xmax = stats.df.f$dives_per_bout_ci_high[stats.df.f$type == "TDR"])


p <- ggplot(data = dive_depth_df,aes(x = x,y = y, col = type))
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymin = ymin,ymax = ymax))
p <- p + geom_errorbarh(aes(xmin = xmin,xmax = xmax))
p <- p + geom_abline(intercept = 0, slope = 1, color="red", 
                     linetype="dashed", size=1.5)
p



# Compare bottom time duration ------
dive_depth_df <- data.frame(x = stats.df.f$bottom_dur_prop_mean[stats.df.f$type == "TDR"],
                            y = stats.df.f$bottom_dur_prop_mean[stats.df.f$type == "GPS"],
                            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "GPS"],
                            ymin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$type == "GPS"],
                            ymax = stats.df.f$bottom_dur_prop_ci_high[stats.df.f$type == "GPS"],
                            xmin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$type == "TDR"],
                            xmax = stats.df.f$bottom_dur_prop_ci_high[stats.df.f$type == "TDR"])


p <- ggplot(data = dive_depth_df,aes(x = x,y = x-y, col = type))
p <- p + geom_point()
p <- p + geom_errorbar(aes(ymin = x-ymin,ymax = x-ymax))
p <- p + geom_errorbarh(aes(xmin = xmin,xmax = xmax))
p <- p + geom_abline(intercept = 0, slope = 0, color="red", 
                     linetype="dashed", size=1.5)
p + theme_bw()






# Plot function -------
plot_change <- function(x, y, ymin, ymax, xmin, xmax, type, xlab = "x", ylab = "y",
                        main = ""){
  data_df <- data.frame(x = x, y = y, group = type, ymin = ymin,
                        ymax = ymax, xmin = xmin, xmax = xmax)
  
  p <- ggplot(data = data_df,aes(x = x,y = x-y, col = group))
  p <- p + geom_point()
  p <- p + geom_errorbar(aes(ymin = x-ymin,ymax = x-ymax))
  p <- p + geom_errorbarh(aes(xmin = xmin,xmax = xmax))
  p <- p + geom_abline(intercept = 0, slope = 0, 
                       linetype="dashed", size=1.5)
  p <- p + theme_bw()
  p <- p  + labs(list(title = main, x = xlab, y = ylab))
  
  return(p)
}



# Dive bottom time -----
plot_change(x = stats.df.f$bottom_dur_prop_mean[stats.df.f$type == "TDR"]*100,
            y = stats.df.f$bottom_dur_prop_mean[stats.df.f$type == "GPS"]*100,
            ymin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$type == "GPS"]*100,
            ymax = stats.df.f$bottom_dur_prop_ci_high[stats.df.f$type == "GPS"]*100,
            xmin = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$type == "TDR"]*100,
            xmax = stats.df.f$bottom_dur_prop_ci_low[stats.df.f$type == "TDR"]*100,
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = "Dive bottom time (proportion) - TDR only",
            ylab = expression(paste(Delta," Dive bottom time (proportion) - GPS + TDR")),
            main = "Dive bottom time")



# Dive duration -----
plot_change(x = stats.df.f$depth_max_m_mean[stats.df.f$type == "TDR"],
            y = stats.df.f$depth_max_m_mean[stats.df.f$type == "GPS"],
            ymin = stats.df.f$depth_max_m_ci_low[stats.df.f$type == "GPS"],
            ymax = stats.df.f$depth_max_m_ci_high[stats.df.f$type == "GPS"],
            xmin = stats.df.f$depth_max_m_ci_low[stats.df.f$type == "TDR"],
            xmax = stats.df.f$depth_max_m_ci_low[stats.df.f$type == "TDR"],
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = "Dive depth (m) - TDR only",
            ylab = expression(paste(Delta," Dive depth (m) - GPS + TDR")),
            main = "Dive depth")


# Dive duration -----
plot_change(x = stats.df.f$velo_down_ms_mean[stats.df.f$type == "TDR"],
            y = stats.df.f$velo_down_ms_mean[stats.df.f$type == "GPS"],
            ymin = stats.df.f$velo_down_ms_ci_low[stats.df.f$type == "GPS"],
            ymax = stats.df.f$velo_down_ms_ci_high[stats.df.f$type == "GPS"],
            xmin = stats.df.f$velo_down_ms_ci_low[stats.df.f$type == "TDR"],
            xmax = stats.df.f$velo_down_ms_ci_low[stats.df.f$type == "TDR"],
            type = stats.df.f$GPS_TDR_order[stats.df.f$type == "TDR"],
            xlab = expression("Descent rate ("~ms^{-1}~") :TDR only"),
            ylab = expression(Delta~"Descent rate ("~ms^{-1}~") :GPS & TDR"),
            main = "Descent rate")
