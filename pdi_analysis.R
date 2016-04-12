# Analyse PDI - post-dive interval

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


# Look at PDI and dive duration distributions ----
# Histograms of each
hist(dives$duration_s, breaks = 400)
hist(dives$pdi, xlim = c(0,400), breaks = 10000)

# Calculate 0.05, 0.95 % quantiles for dive duration
quantile(dives$duration_s, c(0.05,0.5,0.95))
#'  5% 50% 95% 
#' 28  84 132 




# Make PDI calculations -----

deployments <- unique(tdr.deployments$TDR_deployment_id)
n <- length(deployments)



# Initialise vectors

dep.id <- duration.min <- duration.max <- pdi.min <- pdi_05 <- pdi_10 <- pdi_median <-
  pdi_mean <- pdi_95 <- duration.mid <- n_dives <- NULL

# i <- 1

x <- 1
# ix <- 28
# For each deployment (use 'TDR_deployment_id' column) do:
for(i in 1:n){
  
  dep_id <- deployments[i]
  
  for(ix in seq(28,132,20)){
    # For intervals (20 s) 28 - 132 s calculate:
    
    dep.id[x] <- dep_id
    
    duration.min[x] <- ix
    duration.max[x] <- ix + 20
    duration.mid[x] <- ix + 10
    
    pdi_sub <- dives$pdi[(dives$TDR_deployment_id == dep_id) &
                           (dives$duration_s >= ix) &
                           (dives$duration_s < (ix + 20))]
    n_dives[x] <- length(pdi_sub)
    
    #' PDI min
    pdi.min[x] <- min(pdi_sub, na.rm = TRUE)
    pdi_quants <- NA
    pdi_quants <- quantile(pdi_sub, c(0.05, 0.10, 0.50, 0.95), na.rm = TRUE)

    #' PDI 0.05 quantile
    pdi_05[x] <- pdi_quants[1]
    
    #' PDI 0.10 quantile
    pdi_10[x] <- pdi_quants[2]
    
    #' PDI 0.50 quantile (== median)
    pdi_median[x] <- pdi_quants[3]
    
    #' PDI mean
    pdi_mean[x] <- mean(pdi_sub, na.rm = TRUE)
    
    #' PDI 0.95 quantile
    pdi_95[x] <- pdi_quants[4]
    
    
    x <- x+1
    
  }  
  
  
}



plot(pdi_05~duration.mid)
plot(pdi_10~duration.mid)
plot(pdi_median~duration.mid, ylim = c(0,150))



# Combine above into a single DF together with deployment information
pdi_df <- cbind.data.frame(...)

# Make plot of above -----
#' All points (coloured by individual, symbols by order, filled/ open for treatment)
#' p2p Lines for each individual (colour by individual, and line type by )
#' After running GLMM model add fit lines too (with and without GPS)

# Peform GLMM -----
# Analyse data with model form: 
# PDI ~ dive_duration + treatment + 1|order/ID

