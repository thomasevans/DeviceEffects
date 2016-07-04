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



# GPS all ----




# For GPS locations calculate distance from colony for each



# TDR all


# Dives all
dives <- sqlQuery(gps.db,
                             query = "SELECT guillemots_GPS_TDR_dives.*
  FROM guillemots_GPS_TDR_dives
                  ORDER BY guillemots_GPS_TDR_dives.TDR_deployment_id, guillemots_GPS_TDR_dives.date_time;",
                             as.is = TRUE)






# For one GPS + TDR deployment plot data together ----

# Get start time for deployment

# Get end time for deployment



# Make a multi-panel figure with 5 rows

par(mfrow = c(5,1))


