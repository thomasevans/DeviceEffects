

# Load data ------


# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Load in dive data
dives <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_dives.*
                  FROM guillemots_GPS_TDR_dives
                  WHERE (((guillemots_GPS_TDR_dives.deployed)='TRUE'))
                  ORDER BY guillemots_GPS_TDR_dives.TDR_deployment_id, guillemots_GPS_TDR_dives.date_time;
                  ",
                  as.is = TRUE)
dives$date_time <- as.POSIXct(dives$date_time,
                              tz = "UTC")

# Load in deployment information
# (need to label by device order and device type deployed)
tdr.deployments <- sqlQuery(gps.db,
                            query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_events.GPS_TDR_event_id, guillemots_GPS_TDR_events.date_time_rel_utc, guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.GPS_TDR_event, guillemots_GPS_TDR_events.GPS_TDR_order
                            FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_ON = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                            ",
                            as.is = TRUE)


# Get PDI for dives (currently only pre-dive pause) -----
# Order dive data
dives <- dives[order(dives$TDR_deployment_id, dives$date_time),]

# Get PDI and set to NA for first dive of deployment
dives$pdi <- c(dives$pause_pre_s[-1],NA)
x <- dives$TDR_deployment_id == c(dives$TDR_deployment_id[-1],NA)
dives$pdi[!x] <- NA


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

# Get bout ID for each dive
bouts$date_time_end <- bouts$date_time + bouts$bout_duration_s

# i <- 6074
# dives$dive_id[i

n <- nrow(dives)
dive_bout_id <- NULL
for(i in 1:n){
  a <- bouts$bout_id[dives$date_time[i] >= bouts$date_time  &
                          dives$date_time[i] < bouts$date_time_end &
                          bouts$TDR_deployment_id == dives$TDR_deployment_id[i]]
  if(length(a) == 0L){a <- NA}
  if(length(a) >1) a <- a[1]
  dive_bout_id[i] <- a
}


# Number dives for each bout
ndive <- NULL
cdive <- 1
ndive[1] <- 1
for(i in 2:n){
  if(is.na(dive_bout_id[i])|is.na(dive_bout_id[i-1])){ndive[i] <- 1
  cdive <- 1}else{
    if(dive_bout_id[i] == dive_bout_id[i-1]){ndive[i] <- cdive +1
    cdive <- cdive +1} else{
      ndive[i] <- 1
      cdive <- 1
    }
  }
  
}

# Plus label: first, middle, final
divetype <- rep("mid", n)
divetype[ndive == 1] <- "first"
divetype[-n][divetype[-n] == "mid" & dive_bout_id[-n] != dive_bout_id[-1]] <- "last"
divetype[-n][divetype[-n] == "first" & dive_bout_id[-n] != dive_bout_id[-1]] <- "single"
divetype[n] <- "last"

# summary(as.factor(divetype))
# ix <- 1:n
divetype[-n][divetype[-n]=="mid" & divetype[-1]=="first"] <- "last"
divetype[is.na(dive_bout_id)] <- NA
# 
# ix[-n][divetype[-n]=="first" & divetype[-1]=="first"]


# test <- cbind.data.frame(dive_bout_id, divetype)
# 
# a <- 1:9
# a[-1]


# Get PDI and set to NA for first dive of deployment
# dives$pdi <- c(dives$pause_pre_s[-1],NA)
# x <- dives$TDR_deployment_id == c(dives$TDR_deployment_id[-1],NA)
# dives$pdi[!x] <- NA


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

# # Test above
# # Make a vector of times
# times <- seq(as.POSIXct("2015-06-21 00:00", tz = "UTC"), by = "hour", length.out = 24)
# 
# # Calculate elevations
# elevs <- sapply(times, fun_sun_elev)  
# 
# # See how this looks
# plot(elevs~times, type = "b")
# abline(v=times, lty = 2, col = "light grey")

elevs <- sapply(dives$date_time, fun_sun_elev) 
# hist(elevs, breaks = 40)

# plot(elevs~dives$date_time)

# Label by day period
# - day >0 deg
# - civil twilight <0 & >-6
# - night <-6
# Alternatively could use nautical twilight, which is down to -12

# Function to label period of day based on sun elevation angle  
fun_day_per <- function(h){
  if(h< -6)return("NIGHT")else{
    if(h < 0)return("TWLGHT")else return("DAY")
  }
}

# RUn this
time_per <- sapply(elevs, fun_day_per)
summary(as.factor(time_per))
plot(elevs~dives$date_time,
     col = as.numeric(as.factor(time_per)))


# Make DF of these, then output to DB
dive.details <- cbind.data.frame(dive_bout_id,
                                 divetype,
                                 ndive,
                                 dives$pdi,
                                 dives$dive_id,
                                 dives$date_time,
                                 dives$TDR_deployment_id,
                                 elevs,
                                 time_per)

names(dive.details) <- c("dive_bout_id",
                         "divetype",
                         "ndive",
                         "pdi",
                         "dive_id",
                         "date_time",
                         "TDR_deployment_id",
                         "sun_elevation",
                         "day_period")

str(dive.details)



#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, dive.details,
        tablename = "guillemots_GPS_TDR_dives_details",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)
