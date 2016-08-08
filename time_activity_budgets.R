# Constructing activity budgets from TDR data (first using GPS data to vallidate)


# Load in require data ----


# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Deployment details
# - Including start and end times

# Get TDR deployment data (including file names)
tdr.deployments <- sqlQuery(gps.db,
                            query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_deployments_TDR.file_name, guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.date_time_cap_utc
                            FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_ON = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                            ",
                            as.is = TRUE)



tdr.deployments2 <- sqlQuery(gps.db,
                             query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_deployments_TDR.file_name, guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.date_time_cap_utc
                             FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_OFF = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                             ",
                             as.is = TRUE)

names(tdr.deployments)[5] <- "date_time_rel_utc_start"
tdr.deployments$date_time_cap_utc_end <- tdr.deployments2$date_time_cap_utc

str(tdr.deployments)

tdr.deployments$date_time_cap_utc_end <- as.POSIXct(tdr.deployments$date_time_cap_utc_end, tz = "UTC")
tdr.deployments$date_time_rel_utc_start <- as.POSIXct(tdr.deployments$date_time_rel_utc_start, tz = "UTC")


# GPS all ----

gps.points <- sqlQuery(gps.db,
                             query = "SELECT guillemots_gps_points_all.*
FROM guillemots_gps_points_all
WHERE (((guillemots_gps_points_all.date_time)>#1/1/2015# And (guillemots_gps_points_all.date_time)<#8/1/2015#))
ORDER BY guillemots_gps_points_all.ring_number, guillemots_gps_points_all.date_time;
",
                             as.is = TRUE)

str(gps.points)
gps.points$date_time <- as.POSIXct(gps.points$date_time, tz = "UTC")


# TDR all
tdr_raw <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_raw_tdr.*
                  FROM guillemots_GPS_TDR_raw_tdr
                  ORDER BY guillemots_GPS_TDR_raw_tdr.TDR_deployment_id, guillemots_GPS_TDR_raw_tdr.date_time;",
                  as.is = TRUE)

str(tdr_raw)

tdr_raw$date_time <- as.POSIXct(tdr_raw$date_time, tz = "UTC")


# Dives all
dives <- sqlQuery(gps.db,
                             query = "SELECT guillemots_GPS_TDR_dives.*
  FROM guillemots_GPS_TDR_dives
                  ORDER BY guillemots_GPS_TDR_dives.TDR_deployment_id, guillemots_GPS_TDR_dives.date_time;",
                             as.is = TRUE)

str(dives)
dives$date_time <- as.POSIXct(dives$date_time, tz = "UTC")


# For one GPS + TDR deployment plot data together ----
for(i in 1:23){
  
  
  # Get start time for deployment
  start_date_time <- tdr.deployments$date_time_rel_utc_start[i]
  
  h30 <- as.difftime(30, format = "%H", units = "hours")
  
  # Get end time for deployment
  # end_date_time <- tdr.deployments$date_time_cap_utc_end[i]
  
  end_date_time <- start_date_time + h30
  
  # Ring_number
  ring_number <- tdr.deployments$ring_number[i]
  
  # Deploy_id
  deploy_id <- tdr.deployments$TDR_deployment_id[i]
  
  # Make a multi-panel figure with  6 rows ----
  
  png(paste("activity_combined_first_30h_", i, ".png", sep = ""), width = 8, height = 6, units = "in", res = 300)
  # ?png
  par(mfrow = c(5, 1))
  par(mar=c(0,4,1,2))   
  
  gps.sub <- gps.points$date_time >= start_date_time & gps.points$date_time <= end_date_time &
    gps.points$ring_number == ring_number
  # summary(gps.sub)
  # dist~time
  
  if(sum(gps.sub)<10){
    
    plot(1,1,
         ylab = "Displacement (km)",
         xlab = "",
         xaxt = "n",
         xlim = c(start_date_time, end_date_time),
         type = "b")
    
    
    # speed~time
    plot(1,1,
         ylab = "Speed (ms-1)",
         xlab = "",
         xaxt = "n",
         xlim = c(start_date_time, end_date_time),
         type = "b")
    
  } else {
    
    plot(gps.points$coldist[gps.sub]/1000~
           gps.points$date_time[gps.sub],
         ylab = "Displacement (km)",
         xlab = "",
         xaxt = "n",
         xlim = c(start_date_time, end_date_time),
         type = "b")
    
    
    # speed~time
    plot(gps.points$speed_ms[gps.sub]~
           gps.points$date_time[gps.sub],
         ylab = "Speed (ms-1)",
         xlab = "",
         xaxt = "n",
         xlim = c(start_date_time, end_date_time),
         type = "b")
  }
  
#   plot(gps.points$coldist[gps.sub]/1000~
#          gps.points$date_time[gps.sub],
#        ylab = "Displacement (km)",
#        xlab = "",
#        xaxt = "n",
#        xlim = c(start_date_time, end_date_time),
#        type = "b")
#   
#   
#   # speed~time
#   plot(gps.points$speed_ms[gps.sub]~
#          gps.points$date_time[gps.sub],
#        ylab = "Speed (ms-1)",
#        xlab = "",
#        xaxt = "n",
#        xlim = c(start_date_time, end_date_time),
#        type = "b")
  
  
  
  tdr_raw.sub <- tdr_raw$date_time >= start_date_time & tdr_raw$date_time <= end_date_time &
    tdr_raw$TDR_deployment_id == deploy_id
  summary(tdr_raw.sub)
  
  # Temperature (C)
  plot(tdr_raw$temp_C[tdr_raw.sub]~
         tdr_raw$date_time[tdr_raw.sub],
       ylab = "Temp (C)",
       xlab = "",
       xaxt = "n",
       xlim = c(start_date_time, end_date_time),
       cex = 0.5)
  
  # Wet/dry
  plot(tdr_raw$wet_dry[tdr_raw.sub]~
         tdr_raw$date_time[tdr_raw.sub],
       ylab = "Wet/dry",
       xlab = "",
       xaxt = "n",
       xlim = c(start_date_time, end_date_time),
       type = "l")
  
  # tdr_raw$wet_dry[1:10]
  par(mar=c(2,4,1,2))   
  
  # Pressure (dB)
  plot(tdr_raw$pressure_dBars_base[tdr_raw.sub]~
         tdr_raw$date_time[tdr_raw.sub],
       ylab = "depth",
       xlab = "",
       xaxt = "n",
       ylim = c(80,0),
       xlim = c(start_date_time, end_date_time),
       type = "l")
  axis.POSIXct(1, at=seq(start_date_time, end_date_time, by="hour"), labels= FALSE)
  axis.POSIXct(1, at=seq(start_date_time, end_date_time, by="3 hour"), format="%H")
  # ?axis.POSIXct
  
  dev.off()
  
  
  
  
}
# i <- 20
