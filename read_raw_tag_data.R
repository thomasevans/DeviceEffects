# Script to process the raw tag data files then output to a new DB table.



# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get TDR deployment data (including file names)
tdr.deployments <- sqlQuery(gps.db,
                          query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_deployments_TDR.file_name, guillemots_GPS_TDR_events.ring_number
FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_ON = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                          ",
                          as.is = TRUE)



# Get TDR data and assemble together -------

# For each tag, read in data
n_files <- nrow(tdr.deployments)

# i <- 1
data.all.list <- list()

for(i in 1:n_files){
  
  # File name + location
  file.name <- paste("D:/Dropbox/Guillemots/2015/TDR_data_processed/", tdr.deployments$file_name[i], ".csv", sep = "")
  
  # Read in file
  tdr.df <- read.csv(file.name, header = FALSE, skip = 3)
  
  # name the data columns
  names(tdr.df) <- c("rec_n", "date", "time", "pressure_dBars", "temp_C", "wet_dry")
  
  # make date_time thing
  date_time_text <- paste(tdr.df$date, tdr.df$time, sep = " ")
  # head(date_time_text)
  
  date_time <- as.POSIXct(date_time_text, tz = "UTC")
  # head(date_time)
  
  tdr.df$date_time <- date_time
  
  # Add tdr_id, file_name, and ring_number columns
  tdr.df$ring_number <- tdr.deployments$ring_number[i]
  tdr.df$file_name <- paste(tdr.deployments$file_name[i], ".csv", sep = "")
  tdr.df$TDR_ID <- tdr.deployments$TDR_ID[i]
  tdr.df$TDR_deployment_id <- tdr.deployments$TDR_deployment_id[i]
  
  # Pressure - base-line
  tdr.df$pressure_dBars_base <- tdr.df$pressure_dBars - median(tdr.df$pressure_dBars[tdr.df$pressure_dBars < 5])
  
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

# Export data table -----
data.export <- data.all.df[,-c(2:3)]
# data.export[1,]

#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, data.export,
        tablename = "guillemots_GPS_TDR_raw_tdr",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)



