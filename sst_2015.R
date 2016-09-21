# SST data from NOAA -----

# Functions to look at this data, from: https://github.com/millerlp/Misc_R_scripts/blob/master/NOAA_OISST_ncdf4.R
source("sst_data_fun.R")
# See: http://lukemiller.org/index.php/2014/11/extracting-noaa-sea-surface-temperatures-with-ncdf4/

# range(bouts_df$date_time)

# Should aknowledge this data:
# Reynolds, Richard W., Thomas M. Smith, Chunying Liu, Dudley B. Chelton, Kenneth S. Casey, Michael G. Schlax, 2007: Daily High-Resolution-Blended Analyses for Sea Surface Temperature. J. Climate, 20, 5473-5496.

# source("NOAA_OISST_ncdf4.R")
ssts <- extractOISSTdaily("D:/Dropbox/Guillemots/2015/sst_data/sst.day.mean.2015.v2.nc",
                          "D:/Dropbox/Guillemots/2015/sst_data/lsmask.oisst.v2.nc",
                          lonW = 17.1, lonE=18,
                          latS = 56.5, latN=58,
                          date1 = '2015-01-01',
                          date2 = '2015-12-30')

# str(ssts)
n <- 364
sst_mean <- NA
sst_date <- sst_sd <- sst_n <- sst_range <- NA
for(i in 1:n){
  sst_mean[i] <- mean(ssts[,,i])
  sst_sd[i] <- sd(ssts[,,i])
  sst_range[i] <- max(ssts[,,i]) - min(ssts[,,i])
  sst_n[i] <- length(ssts[,,i])
  sst_date[i] <- 8 + i
}
sst_n

# Daily range
mean(sst_range)
sd(sst_range)

# Period range
mean(sst_mean)
sd(sst_mean)

dates <- seq.POSIXt(as.POSIXct("2015-01-01"),as.POSIXct("2015-12-30"), by = "day")
plot(sst_mean~dates, type = "l", lwd = 2)
points((sst_mean- 2*sst_sd)~dates, type = "l", lty = 2)
points((sst_mean+ 2*sst_sd)~dates, type = "l", lty = 2)
# axis.POSIXct(1, at = dates, labels = TRUE)
# ?axis.POSIXct
