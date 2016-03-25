# dive paramaters correlations etc



# Load in deployment data ----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get TDR deployment data (including file names)
tdr.deployments <- sqlQuery(gps.db,
                            query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_events.GPS_TDR_event_id, guillemots_GPS_TDR_events.date_time_rel_utc, guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.GPS_TDR_event, guillemots_GPS_TDR_events.GPS_TDR_order
                            FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_ON = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                            ",
                            as.is = TRUE)

# Load dives data -----
dives <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_dives.*
FROM guillemots_GPS_TDR_dives
                  WHERE (((guillemots_GPS_TDR_dives.deployed)='TRUE'))
                  ORDER BY guillemots_GPS_TDR_dives.TDR_deployment_id, guillemots_GPS_TDR_dives.date_time;
                  ",
                  as.is = TRUE)


# Join df ----
tdr.deployments$type <- NA
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 1] <- "GPS"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 2] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 1] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 2] <- "GPS"


# Combine data ------
dives.comb <- merge(dives, tdr.deployments,
                    by = "TDR_deployment_id")

# Drop murre without both treatments represented
dives.comb <- dives.comb[dives.comb$TDR_deployment_id != 1,]
dives.comb$date_time <- as.POSIXct(dives.comb$date_time, tz = "UTC")


# Add PDI -----
dives.comb <- dives.comb[order(dives.comb$TDR_deployment_id, dives.comb$date_time),]

dives.comb$pdi <- c(dives.comb$pause_pre_s[-1],NA)
x <- dives.comb$TDR_deployment_id == c(dives.comb$TDR_deployment_id[-1],NA)
dives.comb$pdi[!x] <- NA

# make some quick plots ----
library(ggplot2)


# Dive duration vs. depth
p <- ggplot(data = dives.comb,aes(x = depth_max_m, y = duration_s, col = factor(type)))
p <- p + geom_point(alpha=0.25)
p <- p + stat_smooth()
p

# Dive duration vs. pdi
hist(dives.comb$pdi, xlim = c(0,1000), breaks = 1000)
dives.comb.f <- dives.comb[dives.comb$pdi<1000,]
p <- ggplot(data = dives.comb.f,aes(x = duration_s, y = pdi, col = factor(type)))
p <- p + geom_point(alpha=0.4)
# p <- p + stat_smooth()
p <- p + ylim(0, 250)
p



# Calculate pdf minimma -----

TDR_deployment_ids <- unique(dives.comb$TDR_deployment_id)

# i <- 1
pdi_ls <- list()
for(i in 1:length(TDR_deployment_ids)){
  id <- TDR_deployment_ids[i]
  durations <- seq(20,160, 8)
  
  pdi_min <- NULL
  pdi_05 <- NULL
  pdi_10 <- NULL
  pdi_med <- NULL
  
  for(ix in 1:17){
    f <- dives.comb$duration_s >= durations[ix] & 
      dives.comb$duration_s < durations[ix+1] & dives.comb$TDR_deployment_id == id
    pdi_min[ix] <- min(dives.comb$pdi[f])
    pdi_05[ix] <- quantile(dives.comb$pdi[f], 0.05, na.rm = TRUE)
    pdi_10[ix] <- quantile(dives.comb$pdi[f], 0.1, na.rm = TRUE)
    pdi_med[ix] <- median(dives.comb$pdi[f], na.rm = TRUE)
    
  }
  
  pdi_df <- cbind.data.frame(id, durations[1:17], durations[2:18],
                             ((durations[2:18] + durations[1:17])/2),
                             pdi_min, pdi_05, pdi_10, pdi_med
  )
  pdi_ls[[i]] <- pdi_df
  
}

pdi.df <- do.call(rbind , pdi_ls)

names(pdi.df) <- c("TDR_deployment_id",
                   "Duration_min",
                   "Duration_max",
                   "Duration_mid",
                   "pdi_min",
                   "pdi_05",
                   "pdi_10",
                   "pdi_med")


pdi.df.details <- merge(pdi.df, tdr.deployments,
                        by = "TDR_deployment_id")


# Dive duration vs. pdi
hist(dives.comb$pdi, xlim = c(0,1000), breaks = 1000)
dives.comb.f <- dives.comb[dives.comb$pdi<1000,]
pdi.df.details$pdi_10_exm <- pdi.df.details$pdi_10
pdi.df.details$pdi_10_exm[pdi.df.details$pdi_10>250] <- NA
pdi.df.details$pdi_10_exm[pdi.df.details$pdi_10<5] <- NA

p <- ggplot(data = pdi.df.details,aes(x = Duration_mid, y = pdi_10_exm, col = factor(type)))
p <- p + geom_point(alpha=0.6)
p <- p + stat_smooth()
p <- p + ylim(0, 100)
p


