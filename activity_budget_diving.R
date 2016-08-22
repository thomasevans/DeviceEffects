# Looking at activity budgets


# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Load in deployment information -----

# Read in deployment data
# Load in deployment information
# (need to label by device order and device type deployed)
tdr.deployments <- sqlQuery(gps.db,
                            query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_events.GPS_TDR_event_id, guillemots_GPS_TDR_events.date_time_rel_utc, guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.GPS_TDR_event, guillemots_GPS_TDR_events.GPS_TDR_order
                            FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_ON = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                            ",
                            as.is = TRUE)

tdr.deployments_off <- sqlQuery(gps.db,
                                query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_events.GPS_TDR_event_id, guillemots_GPS_TDR_events.date_time_cap_utc
FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_OFF = guillemots_GPS_TDR_events.GPS_TDR_event_id;",
                                as.is = TRUE)

names(tdr.deployments)[names(tdr.deployments)%in% "date_time_rel_utc"] <- "date_time_ON"
names(tdr.deployments_off)[names(tdr.deployments_off)%in% "date_time_cap_utc"] <- "date_time_OFF"

deployments <- merge(tdr.deployments, tdr.deployments_off,  by = "TDR_deployment_id")

# ?merge

str(deployments)

deployments$date_time_OFF <- as.POSIXct(deployments$date_time_OFF,
                              tz = "UTC")

deployments$date_time_ON <- as.POSIXct(deployments$date_time_ON,
                                        tz = "UTC")

t.dur <- deployments$date_time_OFF - deployments$date_time_ON


# Load in dive data
dives <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_dives.*, guillemots_GPS_TDR_dives_details.dive_bout_id, guillemots_GPS_TDR_dives_details.divetype, guillemots_GPS_TDR_dives_details.ndive, guillemots_GPS_TDR_dives_details.pdi, guillemots_GPS_TDR_dives_details.sun_elevation, guillemots_GPS_TDR_dives_details.day_period
                  FROM guillemots_GPS_TDR_dives INNER JOIN guillemots_GPS_TDR_dives_details ON guillemots_GPS_TDR_dives.dive_id = guillemots_GPS_TDR_dives_details.dive_id
                  ORDER BY guillemots_GPS_TDR_dives.TDR_deployment_id, guillemots_GPS_TDR_dives.date_time;
                  ",
                  as.is = TRUE)
dives$date_time <- as.POSIXct(dives$date_time,
                              tz = "UTC")


# Make table of days with full calendar day data available -----
# day of June
dives$june_day <- as.numeric(format(dives$date_time, "%d"))
hist(dives$june_day)

deployments$june_day_start <- as.numeric(format(deployments$date_time_ON, "%d"))
deployments$june_day_end <- as.numeric(format(deployments$date_time_OFF, "%d"))

deployments$june_day_full_day_on <- deployments$june_day_start + 1
deployments$june_day_full_day_off <- deployments$june_day_end - 1
deployments$june_day_n <- deployments$june_day_full_day_off - deployments$june_day_full_day_on + 1



# Summarise available data by deployments
# - how many full days of data area available per deployment?
summary(as.factor(deployments$june_day_n))

# Drop deployment for excluded bird
deployments <- deployments[-1,]

# Make a table for this info -----
xTDR_deployment_id <- rep(deployments$TDR_deployment_id, deployments$june_day_n)
xring_numuber  <- rep(deployments$ring_number, deployments$june_day_n)
day.df <- cbind.data.frame(xTDR_deployment_id, xring_numuber)
names(day.df) <- c("TDR_deployment_id", "ring_number")

dep_id <- unique(day.df$TDR_deployment_id)

day.df$june_day <- NA

deployments$TDR_deployment_id == dep_id
# i <- 1
for(i in 1:length(dep_id)){
  day.df$june_day[day.df$TDR_deployment_id == dep_id[i]] <- deployments$june_day_full_day_on[i]:deployments$june_day_full_day_off[i]
}


  
# For each full day calculate: ------

#sunrise/set times
library("maptools")
# Island centre
lon <- 17.972
lat <- 57.285
coord <- matrix(c(lon, lat), nrow = 1)
sunrise <- sunriset(coord, dives$date_time, direction = "sunrise",
                    POSIXct.out = TRUE)[,2]
sunset <- sunriset(coord, dives$date_time, direction = "sunset",
                   POSIXct.out = TRUE)[,2]
td_90 <- as.difftime(90, units = "mins")
sr_90 <- sunrise + td_90
ss_90 <- sunset - td_90

fun_time <- function(sun_r, sun_s, t){
  if(t<sun_r) x <- "NIGHT" else{
    if(t<sun_s) x <- "DAY" else{
      x <- "NIGHT"
    }
  }
  return(x)
}

dives$time_of_day <- mapply(fun_time, sun_r = sr_90, sun_s = ss_90, t = dives$date_time)
summary(as.factor(dives$time_of_day))

# Check this looks correct
plot(dives$sun_elevation~dives$date_time, col = as.factor(dives$time_of_day))

# * p(diving) etc -----
library(plyr)
# dives$duration_s
summary.df <- ddply(dives, .(TDR_deployment_id, june_day),
                    summarise,
                    dive_time_total = sum(duration_s, na.rm = TRUE),
                    dive_time_p = dive_time_total/(60*60*24),
                    pdi_time_total = sum(pdi[!(divetype %in% c("final", "single")) &
                                               pdi <250]),
                    pdi_time_p = pdi_time_total/(60*60*24),
                    dive_pdi_p = dive_time_p + pdi_time_p,
                    
                    # Time of day
                    dive_time_total_day = sum(duration_s[time_of_day == "DAY"],
                                              na.rm = TRUE),
                    dive_time_total_night = sum(duration_s[time_of_day == "NIGHT"],
                                                na.rm = TRUE),
                    pdi_time_day = sum(pdi[time_of_day == "DAY" &
                                             !(divetype %in% c("final", "single")) &
                                               pdi <250],
                                       na.rm = TRUE),
                    pdi_time_night = sum(pdi[time_of_day == "NIGHT" &
                                               !(divetype %in% c("final", "single")) &
                                               pdi <250],
                                         na.rm = TRUE),
                    dive_day_p = dive_time_total_day / (60*60*24),
                    dive_night_p = dive_time_total_night / (60*60*24),
                    pdi_day_p = pdi_time_day / (60*60*24),
                    pdi_night_p = pdi_time_night / (60*60*24),
                    dive_pdi_p_day = ((dive_day_p + pdi_day_p)/
                      (dive_night_p + pdi_night_p + dive_day_p + pdi_day_p))
                    
)

day.df.comb <- merge(day.df, summary.df, by = c("TDR_deployment_id", "june_day"))

day.df.comb <- merge(day.df.comb, deployments, by = c("TDR_deployment_id"))



day.df.comb$type <- NA
day.df.comb$type[day.df.comb$GPS_TDR_order == "GPS_first" & day.df.comb$GPS_TDR_event == 1] <- "GPS"
day.df.comb$type[day.df.comb$GPS_TDR_order == "GPS_first" & day.df.comb$GPS_TDR_event == 2] <- "TDR"
day.df.comb$type[day.df.comb$GPS_TDR_order == "TDR_first" & day.df.comb$GPS_TDR_event == 1] <- "TDR"
day.df.comb$type[day.df.comb$GPS_TDR_order == "TDR_first" & day.df.comb$GPS_TDR_event == 2] <- "GPS"



boxplot(day.df.comb$dive_pdi_p~day.df.comb$type+ day.df.comb$GPS_TDR_order)

boxplot(day.df.comb$dive_pdi_p_day~day.df.comb$type+ day.df.comb$GPS_TDR_order)


# Make some plots -----
col_3 <- c("#1b9e77","#d95f02", "#7570b3", "grey60")

library("ggplot2")
library(scales)
library(cowplot)


theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        legend.key.width = unit(3, "lines"),
        legend.title = element_blank()
  )

ggplot(day.df.comb,
       aes(x = type, fill = GPS_TDR_order, y = dive_pdi_p*100))+
  geom_boxplot(outlier.size = 0, alpha = 0.7) +
  geom_point(pch = 21, position = position_jitterdodge(),
             alpha = 0.6)+
  labs(list(x = "Group",
            y = "% day diving + PDI",
            fill = "")) +
  theme_new +
  scale_colour_manual(values=col_3[1:3]) +
  scale_fill_manual(values=col_3[1:3]) +
  # theme(legend.position = c(0.4, 1)) +
  theme(legend.position = "none")


ggsave(filename = "activty_dive_pdi_boxplot_no_legend.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_boxplot_no_legend.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_boxplot_no_legend.pdf", width = 4, height = 4,
       units = "in")



ggplot(day.df.comb,
       aes(x = type, fill = GPS_TDR_order, y = dive_pdi_p_day*100))+
  geom_boxplot(outlier.size = 0, alpha = 0.7) +
  geom_point(pch = 21, position = position_jitterdodge(),
             alpha = 0.6)+
  labs(list(x = "Group",
            y = "P(day active) %",
            fill = "")) +
  theme_new +
  scale_colour_manual(values=col_3[1:3]) +
  scale_fill_manual(values=col_3[1:3]) +
  # theme(legend.position = c(0.4, 1)) +
  theme(legend.position = "none")


ggsave(filename = "activty_dive_pdi_boxplot_no_legend_day.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_boxplot_no_legend_day.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_boxplot_no_legend_day.pdf", width = 4, height = 4,
       units = "in")





hist(day.df.comb$dive_pdi_p, breaks = 10)

hist(day.df.comb$dive_pdi_p_day, breaks = 10)


library(scales)


p <- ggplot(day.df.comb, aes(june_day, dive_pdi_p,
                             colour = factor(GPS_TDR_order),
                             shape = type)) +
  scale_colour_manual(values=col_3) +
  geom_line(alpha = 0.6,
            lwd = 0.8,
            aes(group = factor(ring_number.x),
                lty = factor(ring_number.x)),
            # shape = NA,
            show.legend = FALSE) +
  geom_point(
    alpha=0.8,
    size=3,
    show.legend =TRUE) +
  theme_bw()  + guides(group=FALSE, lty=FALSE) 
# p <- p  + scale_colour_manual(values=col_3)
p <- p  + labs(list(x = "Date (day of June)", y =  "Dive + PDI time (% of day)", shape = "Device", col = "Order", fill = "Order"))
p <- p + theme(legend.key.width=unit(2,"line"))
p +scale_y_continuous(labels = percent,
                                       breaks = seq(0,.30, .05),
                      limits = c(0,0.3))
# ?scale_y_continuous
ggsave(filename = "activty_dive_pdi_date.svg", width = 6, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_date.png", width = 6, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_date.pdf", width = 6, height = 4,
       units = "in")




# Propotion day active

p <- ggplot(day.df.comb, aes(june_day, dive_pdi_p_day,
                             colour = factor(GPS_TDR_order),
                             shape = type)) +
  scale_colour_manual(values=col_3) +
  geom_line(alpha = 0.6,
            lwd = 0.8,
            aes(group = factor(ring_number.x),
                lty = factor(ring_number.x)),
            # shape = NA,
            show.legend = FALSE) +
  geom_point(
    alpha=0.8,
    size=3,
    show.legend =TRUE) +
  theme_bw()  + guides(group=FALSE, lty=FALSE) 
# p <- p  + scale_colour_manual(values=col_3)
p <- p  + labs(list(x = "Date (day of June)", y =  "P(day active)", shape = "Device", col = "Order", fill = "Order"))
p <- p + theme(legend.key.width=unit(2,"line"))
p +scale_y_continuous(labels = percent,
                      breaks = seq(0,1, .1),
                      limits = c(0,1))
# ?scale_y_continuous
ggsave(filename = "activty_dive_pdi_date_day_P.svg", width = 6, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_date_day_P.png", width = 6, height = 4,
       units = "in")
ggsave(filename = "activty_dive_pdi_date_day_P.pdf", width = 6, height = 4,
       units = "in")


p <- ggplot(day.df.comb, aes(june_day, dive_time_p*100,
                             colour = GPS_TDR_order,
                             group = ring_number.x, shape = type)) +
  geom_point(
    alpha=0.8,
    size=3,
    show.legend =TRUE) +
  geom_line() +
  theme_bw()
p <- p  + scale_colour_manual(values=col_3)
p <- p  + labs(list(x = "Date (day of June)", y =  "Dive time (% of day)", shape = "Device", col = "Order", fill = "Order"))
p <- p + theme(legend.key.width=unit(2,"line"))
p

ggsave(filename = "activty_dive_date.svg", width = 6, height = 4,
       units = "in")
ggsave(filename = "activty_dive_date.png", width = 6, height = 4,
       units = "in")
ggsave(filename = "activty_dive_date.pdf", width = 6, height = 4,
       units = "in")

# Statistical analysis of PDI time -----



library(lme4)
library(arm)
library(lattice)
library(MuMIn)
library(pbkrtest)

KRSumFun <- function(object, objectDrop, ...) {
  krnames <- c("ndf","ddf","Fstat","p.value","F.scaling")
  r <- if (missing(objectDrop)) {
    setNames(rep(NA,length(krnames)),krnames)
  } else {
    krtest <- KRmodcomp(object,objectDrop)
    unlist(krtest$stats[krnames])
  }
  attr(r,"method") <- c("Kenward-Roger via pbkrtest package")
  r
}



models <- list()
# p(day) ~ device status + order + status:order + sex + julian date + (1|ring_number)


models[[1]] <- glmer(dive_pdi_p ~
                       GPS_TDR_order*type +
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

# day.df.comb$ring_number.x

models[[2]] <- glmer(dive_pdi_p ~
                       GPS_TDR_order + type +
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[3]] <- glmer(dive_pdi_p ~
                       GPS_TDR_order +
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[4]] <- glmer(dive_pdi_p ~
                       type +
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[5]] <- glmer(dive_pdi_p ~
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[6]] <- glmer(dive_pdi_p ~
                       1 +
                       (1|ring_number.x),
                     data = day.df.comb)



# Summarise information from the models
models.aicc <- sapply(models, AICc)
models.aicc.dif <- models.aicc-min(models.aicc)
models.r2m <- sapply(models, r.squaredGLMM)
t(models.r2m)

models.fit.df <- cbind.data.frame(c(1:length(models)),models.aicc,
                                  models.aicc.dif,
                                  t(models.r2m))
names(models.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")


# Significance for dropped terms
drop1(models[[2]], test = "user", sumFun = KRSumFun)

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
model_va_coef <- summary(models[[2]])$coef[, 1]
model_va_ci <- confint(models[[2]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])

summary(models[[1]])
# Check model behaviour
plot(models[[1]])
qqmath(models[[1]])
  


# P(activity) daytime ------

models <- list()
# p(day) ~ device status + order + status:order + sex + julian date + (1|ring_number)


models[[1]] <- glmer(dive_pdi_p_day ~
                       GPS_TDR_order*type +
                       june_day +
                       (1|ring_number.x),
                     # family = binomial,
                     data = day.df.comb)
# plot(models[[1]])


# day.df.comb$ring_number.x

models[[2]] <- glmer(dive_pdi_p_day ~
                       GPS_TDR_order + type +
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[3]] <- glmer(dive_pdi_p_day ~
                       GPS_TDR_order +
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[4]] <- glmer(dive_pdi_p_day ~
                       type +
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[5]] <- glmer(dive_pdi_p_day ~
                       june_day +
                       (1|ring_number.x),
                     data = day.df.comb)

models[[6]] <- glmer(dive_pdi_p_day ~
                       1 +
                       (1|ring_number.x),
                     data = day.df.comb)



# Summarise information from the models
models.aicc <- sapply(models, AICc)
models.aicc.dif <- models.aicc-min(models.aicc)
models.r2m <- sapply(models, r.squaredGLMM)
t(models.r2m)

models.fit.df <- cbind.data.frame(c(1:length(models)),models.aicc,
                                  models.aicc.dif,
                                  t(models.r2m))
names(models.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")
models.fit.df

# Significance for dropped terms
drop1(models[[5]], test = "user", sumFun = KRSumFun)

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
model_va_coef <- summary(models[[2]])$coef[, 1]
model_va_ci <- confint(models[[2]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])

summary(models[[1]])
# Check model behaviour
plot(models[[1]])
qqmath(models[[1]])

plot(models[[6]])
qqmath(models[[6]])

