# Script to process the MTdive diving data files then output to a new DB table.
# Based on 'read_raw_tag_data.R'


# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


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


# Get TDR data and assemble together -------

# For each tag, read in data
n_files <- nrow(tdr.deployments)

# i <- 6
data.all.list <- list()

for(i in 1:n_files){
  
  # File name + location
  file.name <- paste("D:/Dropbox/Guillemots/2015/TDR_data_processed/", tdr.deployments$file_name[i], ".ex1", sep = "")
  
  # Read in file
  tdr.df <- read.table(file.name, header = FALSE, skip = 4, sep = "\t")
  
  # name the data columns
  names(tdr.df) <- c("rec_num", "date", "time", "shape", "pause_pre_s",
                     "duration_s", "descent_dur_s", "bottom_dur_s",
                     "ascent_dur_s", "velo_down_ms", "velo_bot_ms",
                     "velo_up_ms", "depth_max_m", "depth_bot_min_m",
                     "depth_bot_mean_m",
                     "depth_bot_med_m", "ignore_1", "depth_bot_sd_m",
                     "ignore_2", "ignore_3", "ignore_4", "ignore_5",
                     "ignore_6", "depth_mean_dive_m", "depth_integral_dive",
                     "ignore_7", "ignore_8", "temp_c_mean", "temp_c_bot_mean",
                     "temp_c_bot_min", "temp_c_bot_max", "temp_c_depth_max",
                     "ignore_9", "ignore_10", "temp_c_min", "temp_c_start",
                     "depth_integral_bottom", "ignore_11")
  
  
  # drop columns we don't want
  tdr.df <- tdr.df[,-c(17,19:23,26:27,33:34,38,39:40)]
  
  # make date_time thing
  date_time_text <- paste(tdr.df$date, tdr.df$time, sep = " ")
  # head(date_time_text)
  
  date_time <- as.POSIXct(date_time_text, tz = "UTC", format = "%d-%m-%Y %H:%M:%S")
  # range(date_time)
  # head(date_time)
  # ?as.POSIXct
  tdr.df$date_time <- date_time
  
  # Add tdr_id, file_name, and ring_number columns
  tdr.df$ring_number <- tdr.deployments$ring_number[i]
  tdr.df$file_name <- paste(tdr.deployments$file_name[i], ".ex1", sep = "")
  tdr.df$TDR_ID <- tdr.deployments$TDR_ID[i]
  tdr.df$TDR_deployment_id <- tdr.deployments$TDR_deployment_id[i]
  
  # Pressure - base-line
  # tdr.df$pressure_dBars_base <- tdr.df$pressure_dBars - median(tdr.df$pressure_dBars[tdr.df$pressure_dBars < 5 & tdr.df$wet_dry == 0])
  
  
  # Is tag deployed?
  tdr.df$deployed <- (tdr.df$date_time < as.POSIXct(tdr.deployments$date_time_cap_utc[i], tz = "UTC")) &
    (tdr.df$date_time > as.POSIXct(tdr.deployments$date_time_rel_utc_start[i], tz = "UTC"))
    # summary(tdr.df$deployed)
  #   summary((tdr.df$date_time < as.POSIXct(tdr.deployments$date_time_cap_utc[i], tz = "UTC")))
  #   summary(tdr.df$date_time > as.POSIXct(tdr.deployments$date_time_rel_utc_start[i], tz = "UTC"))
  #   
  data.all.list[[i]] <- tdr.df
  
  
}

# Combine the data -----
data.all.df <- do.call("rbind", data.all.list)


# Convert pressure to real metres reading -----
# I don't think it's worth doing this, the largest differences are < 1 m, and those
# only for high pressure value (i.e. deeper)
# p <- 10
# 
# pres2m <- function(p){
#   lat <- 70
#   x <- (sin(lat/57.29578))^2
#   g <- 9.780318*(1 + ((5.2788 * 10^-3) +
#                         (2.36 * 10^-5)*x)*x) + (
#                           1.092 * 10^-6 * p
#                         )
#   d <- (((((-1.82 * 10^-15)*p +
#              2.279*10^-10)*p - 2.2512*10^-5)*p + 9.72659)*p)/g
#   return(d)
# }
# 
# 
# depth.new <- sapply(tdr.df$pressure_dBars_base, pres2m)
# 
# hist(depth.new)
# hist(tdr.df$pressure_dBars_base-depth.new)
# 
# dif <- tdr.df$pressure_dBars_base-depth.new
# 
# hist(depth.new[dif > 0.4])
str(data.all.df$deployed)
# Export data table -----
data.export <- data.all.df[,-c(2:3)]
# data.export[1,]

data.export$dive_id <- c(1:nrow(data.export))

str(data.export)
# names(sqlTypeInfo(gps.db))
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, data.export,
        tablename = "guillemots_GPS_TDR_dives",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)
# ?sqlSave

# hist(data.export$depth_max_m[data.export$depth_max_m < 150], breaks = 40)
# 
# hist(data.export$depth_max_m[data.export$depth_max_m < 150], breaks = 200,
#      xlim = c(0,20))
