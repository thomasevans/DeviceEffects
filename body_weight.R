# Analyse and plot the weight data to compare body mass changes between deployments

# Get data from DB ------
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Get GPS data
tag.events <- sqlQuery(gps.db,
                      query = "SELECT guillemots_GPS_TDR_events.*
FROM guillemots_GPS_TDR_events
                      ORDER BY guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.GPS_TDR_event;",
                      as.is = TRUE)

# Drop record with missing data
tag.events <- tag.events[tag.events$ring_number != "AAZ988",]

# Do some calculations on this -----
# Weight changes (from start, and from previous occasion)
tag.events$weight_change_start <- NULL

tag.events$mass_change_start[tag.events$GPS_TDR_event == 1] <- tag.events$mass[tag.events$GPS_TDR_event == 1] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$mass_change_start[tag.events$GPS_TDR_event == 2] <- tag.events$mass[tag.events$GPS_TDR_event == 2] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$mass_change_start[tag.events$GPS_TDR_event == 3] <- tag.events$mass[tag.events$GPS_TDR_event == 3] - tag.events$mass[tag.events$GPS_TDR_event == 1]

# Make some plots with ggplot -------

# Get data into suitable format


# Make an initial plot