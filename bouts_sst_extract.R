# Bout + SST


# Read in bout data ------
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')



# Read in bout data table
bouts <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_bouts.*
                  FROM guillemots_GPS_TDR_bouts
                  WHERE (((guillemots_GPS_TDR_bouts.deployed)='TRUE'))
                  ORDER BY guillemots_GPS_TDR_bouts.TDR_deployment_id, guillemots_GPS_TDR_bouts.date_time;",
                  as.is = TRUE)
bouts$date_time <- as.POSIXct(bouts$date_time,
                              tz = "UTC")

str(bouts)

summary(as.factor(bouts$dives_n))

# Get raw tag data ------
tdr_raw <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_raw_tdr.*
FROM guillemots_GPS_TDR_raw_tdr
ORDER BY guillemots_GPS_TDR_raw_tdr.TDR_deployment_id, guillemots_GPS_TDR_raw_tdr.date_time;",
                  as.is = TRUE)
tdr_raw$date_time <- as.POSIXct(tdr_raw$date_time,
                              tz = "UTC")



# For each bout get summary stats for SST -----
# i <- 10

bouts$sst_mean <- bouts$sst_median <- bouts$sst_var <- NA

for(i in 1:nrow(bouts)){
  # for(i in 1:100){
  tdr_dep_id <- bouts$TDR_deployment_id[i]
  bout_start <- bouts$date_time[i]
  
  recs <- (tdr_raw$TDR_deployment_id == tdr_dep_id &
             tdr_raw$date_time >= (bout_start - 102) &
             tdr_raw$date_time <= (bout_start - 12))
  temps <- tdr_raw$temp_C[recs]
  wet_dry <- tdr_raw$wet_dry[recs]
  n <- length(temps)
  if(sum(wet_dry)<2){
    bouts$sst_mean[i] <- mean(temps[15:23])
    bouts$sst_median[i] <- median(temps[15:23])
    bouts$sst_var[i] <- var(temps[15:23])
    
  } else {
    # SET ALL TO NA
    bouts$sst_mean[i] <- NA
    bouts$sst_median[i] <- NA
    bouts$sst_var[i] <- NA
  }
  
}  



# What's up with the high temperatures? -----
# They look to be mostly 'real' SSTs, mostly from 21st June, and mostly
# associated with deep dives, so may be in area/s of highly stratified
# water

bouts_x <- bouts$sst_var < 0.25
summary(bouts_x)

hist(bouts$sst_median, breaks = 1000)
idx <- 1:nrow(bouts)
high_t <- idx [bouts$sst_median > 16 & !is.na(bouts$sst_median) &
                 bouts$sst_var < 0.25]

i <- high_t[40]
tdr_dep_id <- bouts$TDR_deployment_id[i]
bout_start <- bouts$date_time[i]

recs <- (tdr_raw$TDR_deployment_id == tdr_dep_id &
           tdr_raw$date_time >= (bout_start - 1002) &
           tdr_raw$date_time <= (bout_start + 1000))
temps <- tdr_raw$temp_C[recs]
wet_dry <- tdr_raw$wet_dry[recs]
par(mfrow=c(3,1))
plot(temps, col = as.factor(wet_dry))
abline(h = bouts$sst_median[i])
plot(tdr_raw$pressure_dBars_base[recs], col = as.factor(wet_dry))
plot(tdr_raw$wet_dry[recs], col = as.factor(wet_dry))

bouts$bout_id[i]
bouts_h <- bouts[high_t,]

hist(bouts$sst_var, breaks = 1000, xlim = c(0,5))

# Vallid SST values ------
# Add column for 'vallid' SST values
bouts$sst_vallid <- FALSE
bouts$sst_vallid[bouts$sst_var < 0.25 &
                   !is.na(bouts$sst_median) &
                   bouts$sst_median < 20] <- TRUE

par(mfrow=c(1,1))
hist(bouts$sst_median[bouts$sst_vallid], breaks = 100)

# Output these details to DB table ------
# prep table
export.df <- cbind.data.frame(bouts$bout_id, bouts$ring_number, bouts$date_time,
                   bouts$sst_var, bouts$sst_mean, bouts$sst_median,
                   bouts$sst_vallid)
names(export.df) <- c("bout_id", "ring_number", "date_time",
                      "sst_var", "sst_mean", "sst_median",
                      "sst_vallid")

#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, export.df,
        tablename = "guillemots_GPS_TDR_bouts_sst",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)

# How many are vallid?
summary(export.df$sst_vallid)
