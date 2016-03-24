# Calculate summary statistics for diving paramaters for each deployment


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


# Load in diving data ----
dives <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_dives.*
FROM guillemots_GPS_TDR_dives
                  WHERE (((guillemots_GPS_TDR_dives.deployed)='TRUE'))
                  ORDER BY guillemots_GPS_TDR_dives.TDR_deployment_id, guillemots_GPS_TDR_dives.date_time;
                  ",
                  as.is = TRUE)


# Load in dive bout data ----
bouts <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_bouts.*
FROM guillemots_GPS_TDR_bouts
                  WHERE (((guillemots_GPS_TDR_bouts.deployed)='TRUE'))
                  ORDER BY guillemots_GPS_TDR_bouts.TDR_deployment_id, guillemots_GPS_TDR_bouts.date_time;
                  ",
                  as.is = TRUE)


# **** Make a summary table ----
# Package to summarise data
library(reshape2)

# First assemble deployment columns ---
stats.df <- tdr.deployments

# Calculate summaries for various variables -----
# For each get:
# - mean
# - median
# - 95% CI
# - SD


ci_fun_low <- function(x){
  n <- length(x)
  sd_calc <- sd(x)
  e <- qt(0.975,df=n-1)*sd_calc/sqrt(n)
  out.val <- mean(x)-e
  return(out.val)
}

x <- dives$depth_max_m[dives$TDR_deployment_id == 1]

ci_fun_high <- function(x){
  n <- length(x)
  sd_calc <- sd(x)
  e <- qt(0.975,df=n-1)*sd_calc/sqrt(n)
  out.val <- mean(x)+e
  return(out.val)
}


# Number of dives per bout
# - mean
stats.df$dives_per_bout_mean <- aggregate(dives_n ~ TDR_deployment_id,
                                       data = bouts,
                                       FUN = mean)[,2]

# - median
stats.df$dives_per_bout_median <- aggregate(dives_n ~ TDR_deployment_id,
                                         data = bouts,
                                         FUN = median)[,2]

# - 95% CI
stats.df$dives_per_bout_ci_low <- aggregate(dives_n ~ TDR_deployment_id,
                                         data = bouts,
                                         FUN = ci_fun_low)[,2]
stats.df$dives_per_bout_ci_high <- aggregate(dives_n ~ TDR_deployment_id,
                                          data = bouts,
                                          FUN = ci_fun_high)[,2]

# - SD
stats.df$dives_per_bout_sd <- aggregate(dives_n ~ TDR_deployment_id,
                                     data = bouts,
                                     FUN = sd)[,2]

# Dive depth (maximum)
# - mean
stats.df$depth_max_m_mean <- aggregate(depth_max_m ~ TDR_deployment_id,
                     data = dives,
                     FUN = mean)[,2]

# - median
stats.df$depth_max_m_median <- aggregate(depth_max_m ~ TDR_deployment_id,
                                       data = dives,
                                       FUN = median)[,2]

# - 95% CI
stats.df$depth_max_m_ci_low <- aggregate(depth_max_m ~ TDR_deployment_id,
                                       data = dives,
                                       FUN = ci_fun_low)[,2]
stats.df$depth_max_m_ci_high <- aggregate(depth_max_m ~ TDR_deployment_id,
                                         data = dives,
                                         FUN = ci_fun_high)[,2]

# - SD
stats.df$depth_max_m_sd <- aggregate(depth_max_m ~ TDR_deployment_id,
                                         data = dives,
                                         FUN = sd)[,2]


# Descent and Ascent rates
# - mean
stats.df$velo_down_ms_mean <- aggregate(velo_down_ms ~ TDR_deployment_id,
                                       data = dives,
                                       FUN = mean)[,2]

# - median
stats.df$velo_down_ms_median <- aggregate(velo_down_ms ~ TDR_deployment_id,
                                         data = dives,
                                         FUN = median)[,2]

# - 95% CI
stats.df$velo_down_ms_ci_low <- aggregate(velo_down_ms ~ TDR_deployment_id,
                                         data = dives,
                                         FUN = ci_fun_low)[,2]
stats.df$velo_down_ms_ci_high <- aggregate(velo_down_ms ~ TDR_deployment_id,
                                          data = dives,
                                          FUN = ci_fun_high)[,2]

# - SD
stats.df$velo_down_ms_sd <- aggregate(velo_down_ms ~ TDR_deployment_id,
                                     data = dives,
                                     FUN = sd)[,2]


# - mean
stats.df$velo_up_ms_mean <- aggregate(velo_up_ms ~ TDR_deployment_id,
                                        data = dives,
                                        FUN = mean)[,2]

# - median
stats.df$velo_up_ms_median <- aggregate(velo_up_ms ~ TDR_deployment_id,
                                          data = dives,
                                          FUN = median)[,2]

# - 95% CI
stats.df$velo_up_ms_ci_low <- aggregate(velo_up_ms ~ TDR_deployment_id,
                                          data = dives,
                                          FUN = ci_fun_low)[,2]
stats.df$velo_up_ms_ci_high <- aggregate(velo_up_ms ~ TDR_deployment_id,
                                           data = dives,
                                           FUN = ci_fun_high)[,2]

# - SD
stats.df$velo_up_ms_sd <- aggregate(velo_up_ms ~ TDR_deployment_id,
                                      data = dives,
                                      FUN = sd)[,2]


# Proportion of bottom time during dives
dives$bottom_dur_prop <- dives$bottom_dur_s/ dives$duration_s

# hist(dives$bottom_dur_prop)

# - mean
stats.df$bottom_dur_prop_mean <- aggregate(bottom_dur_prop ~ TDR_deployment_id,
                                      data = dives,
                                      FUN = mean)[,2]

# - median
stats.df$bottom_dur_prop_median <- aggregate(bottom_dur_prop ~ TDR_deployment_id,
                                        data = dives,
                                        FUN = median)[,2]

# - 95% CI
stats.df$bottom_dur_prop_ci_low <- aggregate(bottom_dur_prop ~ TDR_deployment_id,
                                        data = dives,
                                        FUN = ci_fun_low)[,2]
stats.df$bottom_dur_prop_ci_high <- aggregate(bottom_dur_prop ~ TDR_deployment_id,
                                         data = dives,
                                         FUN = ci_fun_high)[,2]

# - SD
stats.df$bottom_dur_prop_sd <- aggregate(bottom_dur_prop ~ TDR_deployment_id,
                                    data = dives,
                                    FUN = sd)[,2]




# Output this to new DB table and save R binnary object ----

# Save to new DB table
sqlSave(gps.db, stats.df,
        tablename = "guillemots_GPS_TDR_deploy_stats",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL)

save(stats.df, file = "dive_stats_summary.RData")

