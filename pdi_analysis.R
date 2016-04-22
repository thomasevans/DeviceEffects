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
  pdi_mean <- pdi_95 <- pdi_25 <-  duration.mid <- n_dives <- NULL

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
    pdi_quants <- quantile(pdi_sub, c(0.05, 0.10, 0.50, 0.95, 0.25), na.rm = TRUE)

    #' PDI 0.05 quantile
    pdi_05[x] <- pdi_quants[1]
    
    #' PDI 0.10 quantile
    pdi_10[x] <- pdi_quants[2]
    
    #' PDI 0.25 quantile
    pdi_25[x] <- pdi_quants[5]
    
    #' PDI 0.50 quantile (== median)
    pdi_median[x] <- pdi_quants[3]
    
    #' PDI mean
    pdi_mean[x] <- mean(pdi_sub, na.rm = TRUE)
    
    #' PDI 0.95 quantile
    pdi_95[x] <- pdi_quants[4]
    
    
    x <- x+1
    
  }  
  
  
}


nf <- n_dives > 4
plot(pdi_05[nf]~duration.mid[nf], col = as.factor(dep.id))
plot(pdi_10[nf]~duration.mid[nf], col = as.factor(dep.id))
plot(pdi_median[nf]~duration.mid[nf], ylim = c(0,150), col = as.factor(dep.id))
plot(pdi.min[nf]~duration.mid[nf], ylim = c(0,100), col = as.factor(dep.id))
# ?plot


# Combine above into a single DF together with deployment information
pdi_df <- cbind.data.frame(dep.id , duration.min , duration.max , pdi.min ,
                          pdi_05 , pdi_10 , pdi_25, pdi_median, pdi_mean , pdi_95 ,
                          duration.mid , n_dives)

# Add deployment info to pdi df (GPS/ TDR and order of deployment) ----
tdr.deployments$type <- NA
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 1] <- "GPS"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 2] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 1] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 2] <- "GPS"

names(pdi_df)[1] <- "TDR_deployment_id"

pdi_df_comb <- merge(pdi_df, tdr.deployments,
                        by = "TDR_deployment_id")

pdi_df_comb <- pdi_df_comb[pdi_df_comb$TDR_deployment_id != 1,]

# Make plot of above -----
#' All points (coloured by individual, symbols by order, filled/ open for treatment)
#' p2p Lines for each individual (colour by individual, and line type by )
#' After running GLMM model add fit lines too (with and without GPS)

library("ggplot2")
library(scales)

# Remove those points where values are based on few dives
# hist(pdi_df_comb$n_dives, breaks = 40, xlim = (c(0,100)))

pdi_df_comb$pdi_25_log <- log(pdi_df_comb$pdi_25)


pdi_df_comb_sample_10 <- pdi_df_comb
pdi_df_comb_sample_10[pdi_df_comb$n_dives < 10 ,c(4:7)] <- NA

# pdi_df_comb_sample_10$pdi_25_log <- log(pdi_df_comb_sample_10$pdi_25)

pdi_df_comb_sample_10$duration.mid_new <- (pdi_df_comb_sample_10$duration.mid + 2.25 -(.5*as.numeric(as.factor(pdi_df_comb_sample_10$ring_number))))
p <- ggplot(pdi_df_comb_sample_10, aes(duration.mid_new, pdi_25, col = factor(ring_number),
                             shape= as.factor(type))) +
  geom_line(aes(lty =  as.factor(type)), lwd = 1.5, alpha = 0.3,
             show.legend = TRUE)+
  geom_point(alpha=0.6,
             size=3) 
p <- p + theme_bw()
# p + ylim(0,120)
p
# p <- p  + labs(list(title = "Individual mass changes", x = "Deployment event number", y =  "Mass (g)"))






# Peform GLMM -----
# Analyse data with model form: 
# PDI ~ dive_duration + treatment + 1|order/ID

library(lme4)

mod.1 <- glmer(pdi_25_log ~ duration.mid + type + (1|GPS_TDR_order/ring_number),
               data = pdi_df_comb_sample_10)
summary(mod.1)

mod.1
anova(mod.1)

drop1(mod.1, test = "Chisq")

plot(mod.1)
str(mod.1)

residuals(mod.1) < 1
qqnorm(resid(mod.1))


pdi_df_comb_sample_10$all_predict_25 <- predict(mod.1,pdi_df_comb_sample_10)
resid_calc <- pdi_df_comb_sample_10$all_predict_25 - pdi_df_comb_sample_10$pdi_25_log
plot(resid_calc)
pdi_df_comb_sample_10_no_resid <- pdi_df_comb_sample_10[resid_calc > -2,]
mod.1.no_resid <- glmer(pdi_25_log ~ duration.mid + type + (1|GPS_TDR_order/ring_number),
               data = pdi_df_comb_sample_10_no_resid)
summary(mod.1.no_resid)

mod.1.no_resid
anova(mod.1.no_resid)

drop1(mod.1.no_resid, test = "Chisq")
plot(mod.1.no_resid)

pdi_df_comb_sample_10$all_predict_25_no_resid <- predict(mod.1.no_resid,pdi_df_comb_sample_10)


# newdata <- with(pdi_df_comb_sample_10, expand.grid(type=unique(type), duration.mid = unique(duration.mid)))
p2 <- predict(mod.1,pdi_df_comb_sample_10,re.form=NA)
pdi_df_comb_sample_10$mod.1_pred <- p2
pdi_df_comb_sample_10_pred <- unique(pdi_df_comb_sample_10[,c("duration.mid",
                            "type", "mod.1_pred")])
pdi_df_comb_sample_10_pred$mod.1_pred_antilog <- exp(pdi_df_comb_sample_10_pred$mod.1_pred)


p.mod <- p + geom_line(data = pdi_df_comb_sample_10_pred,
                       aes(duration.mid, mod.1_pred_antilog,
                           col = "Model",
                           lty =  as.factor(type)),
                       lwd = 1.5, alpha = 0.6, show.legend = FALSE,
                       col = "black")
p.mod <- p.mod + geom_point(data = pdi_df_comb_sample_10_pred,
                       aes(duration.mid, mod.1_pred_antilog,
                           col = "Model"),
                       alpha = 0.8, show.legend = FALSE,
                       col = "black", size=4)
p.mod
ggsave("pdi_plot_thing.pdf")
# ?exp

length(unique(p2))
# ?glmer


# mod.1 <- glmer(pdi_25_log ~ duration.mid + type + 1|ring_number,
#                data = pdi_df_comb)
# summary(mod.1)
