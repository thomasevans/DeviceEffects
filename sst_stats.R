# Analysis of SST at bout level

# Read in bout data ----

# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')



# Read in bout data table
bouts <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_bouts.*, guillemots_GPS_TDR_bouts_sst.sst_var, guillemots_GPS_TDR_bouts_sst.sst_mean, guillemots_GPS_TDR_bouts_sst.sst_median, guillemots_GPS_TDR_bouts_sst.sst_vallid
FROM guillemots_GPS_TDR_bouts INNER JOIN guillemots_GPS_TDR_bouts_sst ON guillemots_GPS_TDR_bouts.bout_id = guillemots_GPS_TDR_bouts_sst.bout_id
ORDER BY guillemots_GPS_TDR_bouts.bout_id;",
                  as.is = TRUE)

# Fix data types
bouts$date_time <- as.POSIXct(bouts$date_time,
                              tz = "UTC")
bouts$sst_vallid <- as.logical(bouts$sst_vallid)

str(bouts)


# Read in deployment data ----

# (need to label by device order and device type deployed)
tdr.deployments <- sqlQuery(gps.db,
                            query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_events.GPS_TDR_event_id, guillemots_GPS_TDR_events.date_time_rel_utc, guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.GPS_TDR_event, guillemots_GPS_TDR_events.GPS_TDR_order
                            FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_ON = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                            ",
                            as.is = TRUE)


# Combine these ------

# Combine above two
# I.e. so can see if with GPS+TDR or TDR alone + deployment order
# Add deployment info to pdi df (GPS/ TDR and order of deployment) ----
tdr.deployments$type <- NA
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 1] <- "GPS"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 2] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 1] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 2] <- "GPS"



bouts_df <- merge(bouts, tdr.deployments,
                  by = "TDR_deployment_id")

str(bouts_df)

# Remove record for murre where both sets of data were not collected
bouts_df <- bouts_df[bouts_df$TDR_deployment_id != 1,]

names(bouts_df)[names(bouts_df)=="ring_number.x"] <- "ring_number"


# Add time period to bout data -----

# Time of day (sun elevation) ------

# Using sun elevation function from GeoLight package
library("GeoLight")

# Get sun elevation angle for given date_time (POSIXct format)
fun_sun_elev <- function(t){
  require(GeoLight)
  
  # Solar time
  s <- solar(t)
  
  # Island centre
  lon <- 17.972
  lat <- 57.285
  
  # Zenith angle (angle below vertical)
  z <- zenith(s,lon,lat)
  
  # Calculate sun elevation (angle above horizon, negative if below)
  h <- -1*(z-90)
  
  return(h)
}

elevs <- sapply(bouts_df$date_time, fun_sun_elev) 

# Function to label period of day based on sun elevation angle  
fun_day_per <- function(h){
  if(h< -6)return("NIGHT")else{
    if(h < 0)return("TWLGHT")else return("DAY")
  }
}

# RUn this
time_per <- sapply(elevs, fun_day_per)

bouts_df$day_period <- time_per

# Day of june -----
# day of June
bouts_df$june_day <- as.numeric(format(bouts_df$date_time, "%d"))
hist(bouts_df$june_day)





# Get breeding data ----
chick.dates <- sqlQuery(gps.db,
                        query = "SELECT guillemots_GPS_TDR_breeding.ring_number, guillemots_GPS_TDR_breeding.hatch, guillemots_GPS_TDR_breeding.hatch_unc, guillemots_GPS_TDR_breeding.hatch_june_day, guillemots_GPS_TDR_breeding.chick_age_day_june_conv, guillemots_GPS_TDR_breeding.notes
FROM guillemots_GPS_TDR_breeding
ORDER BY guillemots_GPS_TDR_breeding.ring_number;

                            ",
                        as.is = TRUE)



bouts_df <- merge(bouts_df, chick.dates,
                  by = "ring_number")





bouts_df$chick_age <- bouts_df$june_day-bouts_df$hatch_june_day
hist(bouts_df$chick_age, breaks = 100)



# SST data from NOAA -----

# Functions to look at this data, from: https://github.com/millerlp/Misc_R_scripts/blob/master/NOAA_OISST_ncdf4.R
source("sst_data_fun.R")
# See: http://lukemiller.org/index.php/2014/11/extracting-noaa-sea-surface-temperatures-with-ncdf4/

range(bouts_df$date_time)

# Should aknowledge this data:
# Reynolds, Richard W., Thomas M. Smith, Chunying Liu, Dudley B. Chelton, Kenneth S. Casey, Michael G. Schlax, 2007: Daily High-Resolution-Blended Analyses for Sea Surface Temperature. J. Climate, 20, 5473-5496.

# source("NOAA_OISST_ncdf4.R")
ssts <- extractOISSTdaily("D:/Dropbox/Guillemots/2015/sst_data/sst.day.mean.2015.v2.nc",
                          "D:/Dropbox/Guillemots/2015/sst_data/lsmask.oisst.v2.nc",
                          lonW = 17.1, lonE=18,
                          latS = 57.1, latN=57.5,
                          date1 = '2015-06-09',
                          date2 = '2015-06-25')
str(ssts)
sst_mean <- NA
sst_date <- NA
for(i in 1:17){
  sst_mean[i] <- mean(ssts[,,i])
  sst_date[i] <- 8 + i
}
plot(sst_mean~sst_date)

sst_noaa <- cbind.data.frame(sst_date, sst_mean)
names(sst_noaa) <- c("june_day","sst_noaa_day_mean")

bouts_df_s <- merge(bouts_df, sst_noaa,
                  by = "june_day")
bouts_df_s$sst_median_dev <- bouts_df_s$sst_median - bouts_df_s$sst_noaa_day_mean
hist(bouts_df_s$sst_median_dev, breaks = 100)


# Accounting for daily fluctuation -----
bouts_df_s$h_fraction <- as.numeric(format(bouts_df_s$date_time, "%H")) + as.numeric(format(bouts_df_s$date_time, "%M"))/60

# Peak daily temperature and minimima about 3h forward
# See: Karagali, I., and Høyer, J.L. (2013). Observations and modeling of the diurnal SST cycle in the North and Baltic Seas. J. Geophys. Res. Oceans 118, 4488–4503.

bouts_df_s$sst_day_cycle <- -cos((((bouts_df_s$h_fraction-3)%%24)/24)*2*pi)


# Do stats ------

library(lme4)
library(arm)
library(lattice)
library(MuMIn)
library(pbkrtest)

KRSumFun <- function(object, objectDrop, ...) {
  krnames <- c("ndf","ddf","Fstat","p.value","F.scaling")
  r <- if (missing(objectDrop)) {
    setNames(rep(NA,length(krnames)),krnames)
  } else {
    krtest <- KRmodcomp(object,objectDrop)
    unlist(krtest$stats[krnames])
  }
  attr(r,"method") <- c("Kenward-Roger via pbkrtest package")
  r
}

# Only keep vallid sst
bouts_df_x <- bouts_df_s[bouts_df_s$sst_vallid,]
# sst_median


models <- list()

models[[1]] <- glmer(sst_median_dev ~
                       GPS_TDR_order*type +
                       june_day +
                       day_period +
                       sst_day_cycle + (1|ring_number/june_day),
                     data = bouts_df_x)



models[[2]] <- glmer(sst_median_dev ~
                       GPS_TDR_order+type +
                       june_day +
                       day_period +
                       sst_day_cycle + (1|ring_number/june_day),
                     data = bouts_df_x)

models[[3]] <- glmer(sst_median_dev ~
                       GPS_TDR_order +
                       june_day +
                       day_period +
                       sst_day_cycle + (1|ring_number/june_day),
                     data = bouts_df_x)

models[[4]] <- glmer(sst_median_dev ~
                       type +
                       june_day +
                       day_period +
                       sst_day_cycle + (1|ring_number/june_day),
                     data = bouts_df_x)

models[[5]] <- glmer(sst_median_dev ~
                       june_day +
                       day_period +
                       sst_day_cycle + (1|ring_number/june_day),
                     data = bouts_df_x)

models[[6]] <- glmer(sst_median_dev ~
                       1 +
                       (1|ring_number/june_day),
                     data = bouts_df_x)



# Summarise information from the models
models.aicc <- sapply(models, AICc)
models.aicc.dif <- models.aicc-min(models.aicc)
models.r2m <- sapply(models, r.squaredGLMM)
# t(models.r2m)
# MuMIn::r.squaredGLMM
models.fit.df <- cbind.data.frame(c(1:length(models)),models.aicc,
                                  models.aicc.dif,
                                  t(models.r2m))
names(models.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")


# Significance for dropped terms
drop1(models[[1]], test = "user", sumFun = KRSumFun)

summary(models[[1]])

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
model_va_coef <- summary(models[[1]])$coef[, 1]
model_va_ci <- confint(models[[1]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:3),])

summary(models[[1]])
# Check model behaviour
plot(models[[1]])
qqmath(models[[1]])


# bout sst vs. daily satellite SST -----
mod1 <- lm(bouts_df_x$sst_median~bouts_df_x$sst_noaa_day_mean)
summary(mod1)
anova(mod1)

# Make some plots ??? ----



# Colour scheme from http://colorbrewer2.org/, 3-class Dark2 qualitative, print and colour blind freindly
col_3 <- c("#1b9e77","#d95f02", "#7570b3", "grey60")

library("ggplot2")
library(scales)
library(cowplot)


theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        legend.key.width = unit(3, "lines"),
        legend.title = element_blank()
  )

ggplot(bouts_df_x,
       aes(x = type, fill = GPS_TDR_order, y = sst_median_dev))+
  geom_boxplot(outlier.size = 0, alpha = 0.7, show.legend = FALSE) +
  geom_point(pch = 21, position = position_jitterdodge(),
             alpha = 0.6, show.legend = FALSE)+
  labs(list(x = "Type",
            y = expression("SST (deviation)"~~degree~C),
            fill = "")) +
  theme_new +
  scale_colour_manual(values=col_3[1:3]) +
  scale_fill_manual(values=col_3[1:3]) +
  theme(legend.position = c(0.4, 1))

ggsave(filename = "sst_dev.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "sst_dev.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "sst_dev.pdf", width = 4, height = 4,
       units = "in")

# SST by date - should we detrend it?? ------
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
summary(bouts_df$sst_vallid)


png("bout_sst.png", width = 6, height = 4, res = 300,
    units = "in")
pdf("bout_sst.pdf", width = 6, height = 4)
svg("bout_sst.svg", width = 6, height = 4)
# ?svg
# ?png
plot(bouts_df_x$sst_median~bouts_df_x$date_time,
     col = add.alpha("black", alpha = 0.3),
                     xaxt = "n",
                     ylab = expression("Temperature "~~degree~C),
     las = 1,
     xlab = "Date",
     cex = 0.7)
     # col = add.alpha(as.numeric(as.factor(bouts_df_x$type)), alpha = 0.3),
     # pch = as.numeric(as.factor(bouts_df_x$GPS_TDR_order))
     # )
# sst_noaa$june_day
sst_noaa$times <- seq(as.POSIXct("2015-06-09 12:00", tz = "UTC"), by = "day", length.out = nrow(sst_noaa))
points(sst_noaa$sst_noaa_day_mean~sst_noaa$times,
       type = "b", col = "red",
       lwd = 2) 

# Add daily cycle thing
day.cy <- sst_noaa[rep(seq_len(nrow(sst_noaa)), 24), ]
h <- NULL
for(i in 1:24){
  h <- c(h,rep(i,nrow(sst_noaa)))
} 
day.cy$h <- h
day.cy$times <- (day.cy$times-60*60*12)+(day.cy$h*60*60)
day.cy$ht <- -cos((((day.cy$h-3)%%24)/24)*2*pi)
# hist(day.cy$ht)
day.cy$sst_noaa_day_mean_h <- day.cy$sst_noaa_day_mean+(day.cy$ht)
day.cy <- day.cy[order(day.cy$times),]
points(day.cy$sst_noaa_day_mean_h~day.cy$times,
       type = "l", col = add.alpha("red", 0.7),
       lwd = 1) 

# axis.POSIXct(1, at=seq(as.POSIXct("2015-06-09 12:00", tz = "UTC"), by = "12 hour", length.out = 4*20), labels= FALSE, cex = 0.5)
axis.POSIXct(1, at=seq(as.POSIXct("2015-06-09 00:00", tz = "UTC"), by = "day", length.out = 4*20), labels= TRUE)
# ?axis.POSIXct
dev.off()


# 
# # Dev only
# plot(bouts_df_x$sst_median_dev~bouts_df_x$date_time,
#      col = add.alpha("black", alpha = 0.3)
#      # col = add.alpha(as.numeric(as.factor(bouts_df_x$type)), alpha = 0.3),
#      # pch = as.numeric(as.factor(bouts_df_x$GPS_TDR_order))
# )
# points(day.cy$ht~day.cy$times,
#        type = "l", col = add.alpha("red", 0.7),
#        lwd = 1) 
# 
# 
# 
# 
# 
# 
# d <- order(bouts_df$date_time)
# t3 <- bouts_df$date_time+(3*60*60)
# points(((elevs[d]+90)/8)~(t3[d]), type = "l",
#        col = "blue", lty = 2)
# # ??sine
# # library()
# h <- seq(0,24,0.5)
# ht <- -cos((((h-3)%%24)/24)*2*pi)
# range(ht)
# plot(ht~h)
# 
# h_fraction <- as.numeric(format(bouts_df$date_time, "%H")) + as.numeric(format(bouts_df$date_time, "%M"))/60
# # ?format
