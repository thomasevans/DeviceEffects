# Dive depth model


# Read in diving data


# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Load in dive data
dives <- sqlQuery(gps.db,
                  query = "SELECT guillemots_GPS_TDR_dives.*, guillemots_GPS_TDR_dives_details.dive_bout_id, guillemots_GPS_TDR_dives_details.divetype, guillemots_GPS_TDR_dives_details.ndive, guillemots_GPS_TDR_dives_details.pdi, guillemots_GPS_TDR_dives_details.sun_elevation, guillemots_GPS_TDR_dives_details.day_period
FROM guillemots_GPS_TDR_dives INNER JOIN guillemots_GPS_TDR_dives_details ON guillemots_GPS_TDR_dives.dive_id = guillemots_GPS_TDR_dives_details.dive_id
ORDER BY guillemots_GPS_TDR_dives.TDR_deployment_id, guillemots_GPS_TDR_dives.date_time;
",
                  as.is = TRUE)
dives$date_time <- as.POSIXct(dives$date_time,
                              tz = "UTC")



# Read in deployment data
# Load in deployment information
# (need to label by device order and device type deployed)
tdr.deployments <- sqlQuery(gps.db,
                            query = "SELECT guillemots_GPS_TDR_deployments_TDR.TDR_deployment_id, guillemots_GPS_TDR_deployments_TDR.TDR_ID, guillemots_GPS_TDR_events.GPS_TDR_event_id, guillemots_GPS_TDR_events.date_time_rel_utc, guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.GPS_TDR_event, guillemots_GPS_TDR_events.GPS_TDR_order
                            FROM guillemots_GPS_TDR_deployments_TDR INNER JOIN guillemots_GPS_TDR_events ON guillemots_GPS_TDR_deployments_TDR.GPS_TDR_event_id_ON = guillemots_GPS_TDR_events.GPS_TDR_event_id;
                            ",
                            as.is = TRUE)


# Combine above two
# I.e. so can see if with GPS+TDR or TDR alone + deployment order
# Add deployment info to pdi df (GPS/ TDR and order of deployment) ----
tdr.deployments$type <- NA
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 1] <- "GPS"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "GPS_first" & tdr.deployments$GPS_TDR_event == 2] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 1] <- "TDR"
tdr.deployments$type[tdr.deployments$GPS_TDR_order == "TDR_first" & tdr.deployments$GPS_TDR_event == 2] <- "GPS"



dives_df <- merge(dives, tdr.deployments,
                     by = "TDR_deployment_id")

# Remove record for murre where both sets of data were not collected
dives_df <- dives_df[dives_df$TDR_deployment_id != 1,]

names(dives_df)[names(dives_df)=="ring_number.x"] <- "ring_number"


# Get breeding data ----
chick.dates <- sqlQuery(gps.db,
                            query = "SELECT guillemots_GPS_TDR_breeding.ring_number, guillemots_GPS_TDR_breeding.hatch, guillemots_GPS_TDR_breeding.hatch_unc, guillemots_GPS_TDR_breeding.hatch_june_day, guillemots_GPS_TDR_breeding.chick_age_day_june_conv, guillemots_GPS_TDR_breeding.notes
FROM guillemots_GPS_TDR_breeding
ORDER BY guillemots_GPS_TDR_breeding.ring_number;

                            ",
                            as.is = TRUE)



dives_df <- merge(dives_df, chick.dates,
                  by = "ring_number")



# Day of june -----
# day of June
dives_df$june_day <- as.numeric(format(dives_df$date_time, "%d"))
hist(dives_df$june_day)

dives_df$chick_age <- dives_df$june_day-dives_df$hatch_june_day
hist(dives_df$chick_age, breaks = 100)

# t <- dives_df[dives_df$chick_age < 3,]

# Statistical models ----

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

hist(dives_df$depth_max_m)
hist(log10(dives_df$depth_max_m))
hist(sqrt(dives_df$depth_max_m))
# x<-dives[which(dives$depth_max_m>90),]
names(dives_df)[names(dives_df)=="ring_number.x"] <- "ring_number"

dives_df$depth_max_m_log10 <- log10(dives_df$depth_max_m)
dives_df$depth_max_m_ln <- log(dives_df$depth_max_m)
dives_df$depth_max_m_sqrt <- sqrt(dives_df$depth_max_m)


models <- list()


models[[1]] <- glmer(depth_max_m_log10 ~
                       GPS_TDR_order*type +
                       june_day +
                       day_period +
                          (1|ring_number/june_day/dive_bout_id),
                          data = dives_df)

# models[[2]] <- glmer(depth_max_m_log10 ~
#                        GPS_TDR_order*type +
#                        type*chick_age +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df)
# 
# AIC(models[[1]])
# AIC(models[[2]])

# models[[2]] <- glmer(depth_max_m_log10 ~
#                        GPS_TDR_order*type +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day:dive_bout_id),
#                      data = dives_df)
# summary(models[[1]])
# summary(models[[2]])


models[[2]] <- glmer(depth_max_m_log10 ~
                       GPS_TDR_order+type +
                       june_day +
                       day_period +
                       (1|ring_number/june_day/dive_bout_id),
                     data = dives_df)

models[[3]] <- glmer(depth_max_m_log10 ~
                       GPS_TDR_order +
                       june_day +
                       day_period +
                       (1|ring_number/june_day/dive_bout_id),
                     data = dives_df)

models[[4]] <- glmer(depth_max_m_log10 ~
                       type +
                       june_day +
                       day_period +
                       (1|ring_number/june_day/dive_bout_id),
                     data = dives_df)

models[[5]] <- glmer(depth_max_m_log10 ~
                       june_day +
                       day_period +
                       (1|ring_number/june_day/dive_bout_id),
                     data = dives_df)

models[[6]] <- glmer(depth_max_m_log10 ~
                       1 +
                       (1|ring_number/june_day/dive_bout_id),
                     data = dives_df)



# Summarise information from the models
models.aicc <- sapply(models, AICc)
models.aicc.dif <- models.aicc-min(models.aicc)
models.r2m <- sapply(models, r.squaredGLMM)
t(models.r2m)
MuMIn::r.squaredGLMM
models.fit.df <- cbind.data.frame(c(1:length(models)),models.aicc,
                                       models.aicc.dif,
                                       t(models.r2m))
names(models.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")


# Significance for dropped terms
drop1(models[[1]], test = "user", sumFun = KRSumFun)

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
model_va_coef <- summary(models[[1]])$coef[, 1]
model_va_ci <- confint(models[[1]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])

summary(models[[1]])
# Check model behaviour
plot(models[[1]])
qqmath(models[[1]])
#   
# library(influence.ME)
# infl <- influence(models[[1]], obs = TRUE)
# # (cooks.distance(infl))>0.06
# plot(infl, which = "cook")

# ??qqmath

hist(dives_df$depth_max_m)

range(dives_df$depth_max_m)


# means etc -----
library(plyr)

se <- function(x){
  x <- x[!is.na(x)]
  sqrt(var(x)/length(x))
} 

type.means <- ddply(dives_df, .(GPS_TDR_order, type),
                      summarise,
                      depth_mean = mean(depth_max_m, na.rm = TRUE),
                      depth_SE = se(depth_max_m),
                      depth_log_mean = mean(depth_max_m_log10, na.rm = TRUE),
                      depth_log_SE = se(depth_max_m_log10)
                      
)
type.means

type.means.ind <- ddply(dives_df, .(GPS_TDR_order, type, ring_number),
                    summarise,
                    depth_mean = mean(depth_max_m, na.rm = TRUE),
                    depth_SE = se(depth_max_m),
                    depth_log_mean = mean(depth_max_m_log10, na.rm = TRUE),
                    depth_log_SE = se(depth_max_m_log10)
                    
)
type.means.ind


# Plot above ------


# Colour scheme from http://colorbrewer2.org/, 3-class Dark2 qualitative, print and colour blind freindly
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
# 
# ggplot(type.means, aes(x=type, y=depth_log_mean,
#                        group= GPS_TDR_order)) +
#   geom_line() +
#   # geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), colour="red") +
#   geom_errorbar(width=.1, aes(ymin=type.means$depth_log_mean-type.means$depth_log_SE,
#                               ymax=type.means$depth_log_mean+type.means$depth_log_SE)) +
#   geom_point(shape=21, size=3, fill="white") +
#   ylim(min(type.means$depth_log_mean),max(type.means$depth_log_mean))
# 
# 
# 
# ggplot(dives_df,
#        aes(depth_max_m_log10, type, group = GPS_TDR_order)) +
#   geom_jitter(alpha = I(1/4), aes(colour = GPS_TDR_order)) 
# 
# 
# ggplot(data=dives_df, aes(x= GPS_TDR_order, y=depth_max_m, fill=type)) + 
#   geom_boxplot() +
#   theme_new
# 
# ggplot(data=type.means, aes(x= type, y=depth_log_mean,
#                             group = GPS_TDR_order, colour = GPS_TDR_order)) + 
#   geom_errorbar(aes(ymin=depth_log_mean-depth_log_SE, ymax=depth_log_mean+depth_log_SE), width=.1) +
#   geom_line() +
#   geom_point() +
#   scale_colour_manual(values=col_3[1:2]) +
#   scale_fill_manual(values=col_3[1:2]) +
#   theme_new

type.means$ring_number <- c(1,1,2,2)

ggplot(data=type.means.ind, aes(x= type, y=depth_mean,
                            group = ring_number, colour = GPS_TDR_order)) + 
  geom_errorbar(aes(ymin=depth_mean-depth_SE, ymax=depth_mean+depth_SE),
                width=.1, alpha = 0.3) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3) +
  geom_errorbar(data = type.means, aes(ymin=depth_mean-depth_SE,
                                       ymax=depth_mean+depth_SE),
                width=.2, alpha = 0.8, lwd = 1) +
  geom_line(data = type.means, alpha = 0.8, lwd = 1) +
  geom_point(data = type.means, alpha = 0.8)+
  scale_colour_manual(values=col_3[1:2]) +
  scale_fill_manual(values=col_3[1:2]) +
  labs(list(x = "Device type",
            y = "Dive depth (m)",
            fill = "Order")) +
  # ylim(c(0,50))+
  theme_new +
  theme(legend.position = "top")

ggsave(filename = "change_depth_mean_se.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_depth_mean_se.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_depth_mean_se.pdf", width = 4, height = 4,
       units = "in")



hist(dives_df$depth_max_m, breaks = 100)
hist(log10(dives_df$depth_max_m), breaks = 100)
abline(v=c(13,50), col = "red")

# 3-types of dive plot -----
dive_type_fun <- function(x){
  if(x <13) return("s") else{
    if(x<50) return("m") else return("d")
  }
}

dives_df$dive_type <- sapply(dives_df$depth_max_m, dive_type_fun)
summary(as.factor(dives_df$dive_type))

ggplot(data=dives_df, aes(x=type, fill=dive_type)) + 
  geom_bar() + facet_wrap(~ GPS_TDR_order + ring_number, nrow = 2) +
  theme_new +
  theme(legend.position = c(1,0.4))
ggsave(filename = "p_types_of_dive.svg", width = 8, height = 5,
       units = "in")
ggsave(filename = "p_types_of_dive.png", width = 8, height = 5,
       units = "in")
ggsave(filename = "p_types_of_dive.pdf", width = 8, height = 5,
       units = "in")


m <- ggplot(dives_df, aes(x = depth_max_m, ..density..))
m <- m + geom_histogram(colour = "dark grey",
                        fill = "grey", binwidth = 2,)
m <- m + facet_grid(type ~ GPS_TDR_order)
m <- m + geom_vline(xintercept = c(13, 50),
                    lty = 2, lwd = 1.5, alpha = 0.6, col = "red")
m + theme_new +  labs(list(x = "Dive depth (m)"))
ggsave(filename = "dive_depth_hist_density.svg", width = 6, height = 5,
       units = "in")
ggsave(filename = "dive_depth_hist_density.png", width = 6, height = 5,
       units = "in")
ggsave(filename = "dive_depth_hist_density.pdf", width = 6, height = 5,
       units = "in")

# ??geom_histogram


# Logistic models for dives type -------
dives_df$dive_type_bool <- dives_df$dive_type
# Ignore shallow dives
dives_df$dive_type_bool[dives_df$dive_type_bool == "s"] <- NA
# Change to binnary, deep - true, medium - false
dives_df$dive_type_bool[dives_df$dive_type_bool == "d"] <- TRUE
dives_df$dive_type_bool[dives_df$dive_type_bool == "m"] <- FALSE
dives_df$dive_type_bool <- as.factor(dives_df$dive_type_bool)


# Stats models

# glmer(got_eps~(1|ring_number), family=binomial(link='logit'), data=trips)


models_logist <- list()

dives_df_NAN <- dives_df[!is.na(dives_df$dive_type_bool),]
dives_df_NAN$idx <- 1:nrow(dives_df_NAN)

models_logist[[1]] <- glmer(dive_type_bool ~
                       GPS_TDR_order*type +
                       june_day +
                       day_period +
                         chick_age*type +
                         (1|idx)+
                       (1|ring_number/june_day/dive_bout_id),
                     data = dives_df_NAN, family=binomial(link='logit'))

models_logist[[2]] <- glmer(dive_type_bool ~
                              GPS_TDR_order+type +
                              june_day +
                              day_period +
                              chick_age*type +
                              (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))

models_logist[[3]] <- glmer(dive_type_bool ~
                              GPS_TDR_order*type +
                              june_day +
                              day_period +
                              chick_age +
                              (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))


models_logist[[4]] <- glmer(dive_type_bool ~
                              june_day +
                              day_period +
                              chick_age*type +
                              (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))

models_logist[[5]] <- glmer(dive_type_bool ~
                              GPS_TDR_order+type +
                              june_day +
                              day_period +
                              chick_age +
                              (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))

models_logist[[6]] <- glmer(dive_type_bool ~
                              GPS_TDR_order +
                              june_day +
                              day_period +
                              chick_age +
                              (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))

models_logist[[7]] <- glmer(dive_type_bool ~
                              type +
                              june_day +
                              day_period +
                              chick_age +
                              (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))

models_logist[[8]] <- glmer(dive_type_bool ~
                              june_day +
                              day_period +
                              chick_age +
                              (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))

models_logist[[9]] <- glmer(dive_type_bool ~
                              1 + (1|idx)+
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_df_NAN, family=binomial(link='logit'))

# 
# models_logist[[2]] <- glmer(dive_type_bool ~
#                        GPS_TDR_order+type +
#                        june_day +
#                        day_period + (1|idx) +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_NAN, family=binomial(link='logit'))
# 
# models_logist[[3]] <- glmer(dive_type_bool ~
#                        GPS_TDR_order +
#                        june_day +
#                        day_period +(1|idx) +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_NAN, family=binomial(link='logit'))
# 
# models_logist[[4]] <- glmer(dive_type_bool ~
#                        type +
#                        june_day +
#                        day_period +(1|idx) +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_NAN, family=binomial(link='logit'))
# 
# models_logist[[5]] <- glmer(dive_type_bool ~
#                        june_day +
#                        day_period +(1|idx) +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_NAN, family=binomial(link='logit'))
# 
# models_logist[[6]] <- glmer(dive_type_bool ~
#                        1 +(1|idx) +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_NAN, family=binomial(link='logit'))

# # Based on this: https://ecologyforacrowdedplanet.wordpress.com/2013/08/27/r-squared-in-mixed-models-the-easy-way/
# r2fun <- function(x){
#   Vf <- var(model.matrix(x) %*% fixef(x))
#   Vr <- sum(unlist(print(VarCorr(x),comp="Variance")))
#   vresid <- var(resid(x))
#   # Vdist <- pi^2 / 3
#   Vtotal <- Vf + Vr + vresid
#   R2m <- Vf / (Vtotal)
#   R2c <- (Vf + Vr) / (Vtotal)
#   return(c(R2m,R2c))
# }

# Summarise information from the models_logist
models_logist.aicc <- sapply(models_logist, AICc)
models_logist.aicc.dif <- models_logist.aicc-min(models_logist.aicc)
models_logist.r2m <- sapply(models_logist, r.squaredGLMM)
# t(models_logist.r2m)
models_logist.fit.df <- cbind.data.frame(c(1:length(models_logist)),models_logist.aicc,
                                  models_logist.aicc.dif,
                                  t(models_logist.r2m))
names(models_logist.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

summary(models_logist[[1]])
# r.squaredGLMM(models_logist[[1]])

anova(models_logist[[1]],
      models_logist[[3]])

# Significance for dropped terms
drop1(models_logist[[3]], test = "user", sumFun = KRSumFun)

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models_logist–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/



# Confidence intervals + coeficients
model_va_coef <- summary(models_logist[[3]])$coef[, 1]
model_va_ci <- confint(models_logist[[3]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])

summary(models_logist[[3]])
# Check model behaviour
plot(models_logist[[3]])
qqmath(models_logist[[3]])
plot(models_logist[[3]])



# **** Post dive interval ------
# Following a similar approach to that for dive depth, though this time
# including dive depth in model, as PDI will depend on this

# Filter to not include final dives, nor single dives, only those before end of dive bout
dives_pdi <- dives_df[!(dives_df$divetype %in% c("last", "single")),]
hist(dives_pdi$pdi, breaks = 100)
hist(dives_pdi$pdi, breaks = 1000, xlim = c(0,400))

# Remove extreme values
dives_pdi <- dives_pdi[dives_pdi$pdi < 300 &
                         dives_pdi$depth_max_m > 10,]

ggplot(dives_pdi, aes(x = duration_s, y = log(pdi)))+
  geom_point(alpha = 0.3) +
  geom_density_2d()
  
ggplot(dives_pdi, aes(x = depth_max_m, y = log(pdi),
                      group = GPS_TDR_order, colour = GPS_TDR_order))+
  geom_point(alpha = 0.3) +
  geom_density_2d()



ggplot(dives_pdi, aes(x = depth_max_m, y = pdi,
                      group = type, colour = type))+
  geom_point(alpha = 0.3) +
  geom_density_2d()

ggplot(dives_pdi, aes(x = depth_max_m, y = duration_s,
                      group = type, colour = type))+
  geom_point(alpha = 0.3) +
  geom_density_2d()



ggplot(dives_pdi, aes(x = (depth_max_m), y = bottom_dur_s/(duration_s+pdi),
                      group = type, colour = type))+
  geom_point(alpha = 0.3) +
  geom_density_2d()



ggplot(dives_pdi, aes(x = sqrt(depth_max_m), y = descent_dur_s,
                      group = type, colour = type))+
  geom_point(alpha = 0.3) +
  geom_density_2d() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(dives_pdi, aes(x = sqrt(depth_max_m), y = ascent_dur_s,
                      group = type, colour = type))+
  geom_point(alpha = 0.3) +
  geom_density_2d() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(dives_pdi, aes(x = (depth_max_m), y = (pdi),
                      group = type, colour = type))+
  geom_point(alpha = 0.3) +
  geom_density_2d() +
  geom_smooth(method = "lm", se = TRUE)


str(dives_pdi)

# dives_pdi$duration_s

str(dives_df)



# Statistical models for dive efficiency ----
dives_pdi <- dives_df[!(dives_df$divetype %in% c("last", "single")),]
hist(dives_pdi$pdi, breaks = 100)
hist(dives_pdi$pdi, breaks = 1000, xlim = c(0,400))

# Remove extreme values for pdi and shallow dives
dives_pdi <- dives_pdi[dives_pdi$pdi < 250 &
                         dives_pdi$depth_max_m > 10,]

# Calculate dive efficiency
dives_pdi$dive_efficiency <- dives_pdi$bottom_dur_s/(dives_pdi$duration_s+dives_pdi$pdi)
hist(dives_pdi$dive_efficiency)

# Stats models
models_dive_efficiency <- list()

# models[[1]] <- glmer(dive_efficiency ~
#                        GPS_TDR_order*type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# # r.squaredGLMM(models[[1]])

models_dive_efficiency[[1]] <- glmer(dive_efficiency ~
                              GPS_TDR_order*type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_dive_efficiency[[2]] <- glmer(dive_efficiency ~
                              GPS_TDR_order+type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_dive_efficiency[[3]] <- glmer(dive_efficiency ~
                              GPS_TDR_order*type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)


models_dive_efficiency[[4]] <- glmer(dive_efficiency ~
                              june_day + depth_max_m +
                              day_period +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_dive_efficiency[[5]] <- glmer(dive_efficiency ~
                              GPS_TDR_order+type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_dive_efficiency[[6]] <- glmer(dive_efficiency ~
                              GPS_TDR_order +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_dive_efficiency[[7]] <- glmer(dive_efficiency ~
                              type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_dive_efficiency[[8]] <- glmer(dive_efficiency ~
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_dive_efficiency[[9]] <- glmer(dive_efficiency ~
                              1 + 
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

# models_dive_efficiency[[10]] <- glmer(dive_efficiency ~
#                                        # june_day +
#                                        day_period +
#                                        chick_age +
#                                        
#                                        (1|ring_number/june_day/dive_bout_id),
#                                      data = dives_pdi)
# 
# # 

# 
# models[[2]] <- glmer(dive_efficiency ~
#                        GPS_TDR_order+type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models[[3]] <- glmer(dive_efficiency ~
#                        GPS_TDR_order +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models[[4]] <- glmer(dive_efficiency ~
#                        type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models[[5]] <- glmer(dive_efficiency ~
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models[[6]] <- glmer(dive_efficiency ~
#                        1 +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)



# Summarise information from the models
models.aicc <- sapply(models_dive_efficiency, AICc)
models.aicc.dif <- models.aicc-min(models.aicc)
models.r2m <- sapply(models_dive_efficiency, r.squaredGLMM)
# models.bic <- sapply(models_dive_efficiency, BIC)
# models.bic.dif <- models.bic-min(models.bic)
# t(models.r2m)
# MuMIn::r.squaredGLMM
models.fit.df <- cbind.data.frame(c(1:length(models_dive_efficiency)),models.aicc,
                                  models.aicc.dif,
#                                   models.bic,
#                                   models.bic.dif,
                                  t(models.r2m))
names(models.fit.df) <- c("mod", "AICc", "dAICc",
                          # "BIC", "dBIC",
                          "R2m", "R2c")

# anova(models[[4]],models[[5]])

# Significance for dropped terms
drop1(models_dive_efficiency[[8]], test = "user", sumFun = KRSumFun)

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
model_va_coef <- summary(models_dive_efficiency[[8]])$coef[, 1]
model_va_ci <- confint(models_dive_efficiency[[8]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])

summary(models_dive_efficiency[[8]])
# Check model behaviour
plot(models_dive_efficiency[[8]])
qqmath(models_dive_efficiency[[8]])


ggplot(dives_pdi, aes(x = (depth_max_m), y = dive_efficiency,
                      group = type, colour = type))+
  geom_point(alpha = 0.3) +
  geom_density_2d()

#






# Statistical models for pdi ----
dives_pdi <- dives_df[!(dives_df$divetype %in% c("last", "single")),]
hist(dives_pdi$pdi, breaks = 100)
hist(dives_pdi$pdi, breaks = 1000, xlim = c(0,400))

# Remove extreme values for pdi and shallow dives
dives_pdi <- dives_pdi[dives_pdi$pdi < 300 &
                         dives_pdi$depth_max_m > 10,]
dives_pdi <- dives_pdi[!is.na(dives_pdi$pdi),]
dives_pdi <- dives_pdi[(dives_pdi$pdi) != 0,]

range(dives_pdi$pdi)


# Stats models
models_pdi <- list()

# 
# models[[1]] <- glmer(pdi~
#                        GPS_TDR_order*type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# # r.squaredGLMM(models[[1]])



models_pdi[[1]] <- glmer(pdi ~
                              GPS_TDR_order*type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_pdi[[2]] <- glmer(pdi ~
                              GPS_TDR_order+type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_pdi[[3]] <- glmer(pdi ~
                              GPS_TDR_order*type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

# models_pdi.log.3 <- glmer(log(pdi) ~
#                            GPS_TDR_order*type +
#                            june_day + depth_max_m +
#                            day_period +
#                            chick_age +
#                            
#                            (1|ring_number/june_day/dive_bout_id),
#                          data = dives_pdi)

models_pdi[[4]] <- glmer(pdi ~
                              june_day + depth_max_m +
                              day_period +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_pdi[[5]] <- glmer(pdi ~
                              GPS_TDR_order+type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_pdi[[6]] <- glmer(pdi ~
                              GPS_TDR_order +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_pdi[[7]] <- glmer(pdi ~
                              type +
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_pdi[[8]] <- glmer(pdi ~
                              june_day + depth_max_m +
                              day_period +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

models_pdi[[9]] <- glmer(pdi ~
                              1 + 
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_pdi)

# models_pdi[[10]] <- glmer(pdi ~
#                            # june_day +
#                            day_period +
#                            chick_age +
#                            
#                            (1|ring_number/june_day/dive_bout_id),
#                          data = dives_pdi)
# 
# 
# models[[2]] <- glmer(pdi~
#                        GPS_TDR_order+type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models[[3]] <- glmer(pdi~
#                        GPS_TDR_order +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models[[4]] <- glmer(pdi~
#                        type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models[[5]] <- glmer((pdi) ~
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# # 
# # models[[7]] <- glmer(pdi~
# #                        depth_max_m +
# #                        june_day +
# #                        day_period +
# #                        (1|ring_number/dive_bout_id),
# #                      data = dives_pdi)
# # 
# 
# # Would some transformation help?
# # MASS::boxcox(lm(pdi~depth_max_m,data=dives_pdi))
# # Doesn't look like it!
# 
# models[[6]] <- glmer(pdi~
#                        1 +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)




# # Summarise information from the models

models.aicc <- sapply(models_pdi, AICc)
models.aicc.dif <- models.aicc-min(models.aicc)
models.r2m <- sapply(models_pdi, r.squaredGLMM)
# models.aic <- sapply(models_pdi, AIC)
# models.aic.dif <- models.aic-min(models.aic)
# t(models.r2m)
# MuMIn::r.squaredGLMM
models.fit.df <- cbind.data.frame(c(1:length(models_pdi)),models.aicc,
                                  models.aicc.dif,
                                  # models.aic,
                                  # models.aic.dif,
                                  t(models.r2m))
names(models.fit.df) <- c("mod", "AICc", "dAICc",
                          # "AIC", "dAIC",
                          "R2m", "R2c")

anova(models_pdi[[1]], models_pdi[[3]])


# 
# # Summarise information from the models
# models.aicc <- sapply(models, AICc)
# models.aicc.dif <- models.aicc-min(models.aicc)
# models.r2m <- sapply(models, r.squaredGLMM)
# t(models.r2m)
# # MuMIn::r.squaredGLMM
# models.fit.df <- cbind.data.frame(c(1:length(models)),models.aicc,
#                                   models.aicc.dif,
#                                   t(models.r2m))
# names(models.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

# anova(models[[1]],models[[5]])
# anova(models[[1]],models[[4]])
# anova(models[[1]],models[[3]])
# anova(models[[1]],models[[2]])
# anova(models[[1]],models[[6]])

# Significance for dropped terms
drop1(models_pdi[[3]], test = "user", sumFun = KRSumFun)


# hist(log(dives_pdi$pdi))

# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
model_va_coef <- summary(models_pdi[[3]])$coef[, 1]
model_va_ci <- confint(models_pdi[[3]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])

summary(models_pdi[[3]])
# Check model behaviour
plot(models_pdi[[3]])
qqmath(models_pdi[[3]])

# plot(models_pdi.log.3 )
# qqmath(models_pdi.log.3 )

hist(residuals(models_pdi[[3]]))

# ggplot(dives_pdi, aes(x = (depth_max_m), y = (pdi),
#                       group = day_period, colour = day_period))+
#   geom_point(alpha = 0.3) +
#   geom_density_2d()
# 
# ggplot(dives_pdi, aes(x = (june_day), y = (pdi)))+
#   geom_point(alpha = 0.3) +
#   geom_density_2d()
# 
# j_day <- dives_pdi$june_day[!(is.na(dives_pdi$dive_bout_id))]
# test.df <- cbind.data.frame(residuals(models[[7]]), j_day)
# names(test.df) <- c("resid", "date")
# 
# ggplot(test.df, aes(x = (date), y = (resid)))+
#   geom_point(alpha = 0.3) +
#   geom_density_2d() +
#   geom_smooth(method = "lm", se = TRUE) +
#   ylim(c(-50,50))
# 
# t.m <- lm(test.df$resid~test.df$date)
# summary(t.m)


# dives_pdi.new <- dives_pdi[!(is.na(dives_pdi))]







# Statistical models for descent rate ----
dives_pdi <- dives_df[!(dives_df$divetype %in% c("last", "single")),]
hist(dives_pdi$pdi, breaks = 100)
hist(dives_pdi$pdi, breaks = 1000, xlim = c(0,400))

# Remove extreme values for pdi and shallow dives
dives_pdi <- dives_pdi[dives_pdi$pdi < 300 &
                         dives_pdi$depth_max_m > 10,]
# dives_pdi <- dives_pdi[!is.na(dives_pdi$pdi),]
# dives_pdi <- dives_pdi[(dives_pdi$pdi) != 0,]

# range(dives_pdi$pdi)

# 
# # Stats models
# models_d_rate <- list()
# 
# # str(dives_pdi)
# # models[[1]] <- glmer(descent_dur_s~
# #                        GPS_TDR_order*type +
# #                        depth_max_m +
# #                        june_day +
# #                        day_period +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# # r.squaredGLMM(models[[1]])
# 
# 
# 
# models_d_rate[[1]] <- glmer(descent_dur_s ~
#                               GPS_TDR_order*type +
#                               june_day +
#                               day_period +
#                               chick_age*type +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# models_d_rate[[2]] <- glmer(descent_dur_s ~
#                               GPS_TDR_order+type +
#                               june_day +
#                               day_period +
#                               chick_age*type +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# models_d_rate[[3]] <- glmer(descent_dur_s ~
#                               GPS_TDR_order*type +
#                               june_day +
#                               day_period +
#                               chick_age +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# 
# models_d_rate[[4]] <- glmer(descent_dur_s ~
#                               june_day +
#                               day_period +
#                               chick_age*type +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# models_d_rate[[5]] <- glmer(descent_dur_s ~
#                               GPS_TDR_order+type +
#                               june_day +
#                               day_period +
#                               chick_age +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# models_d_rate[[6]] <- glmer(descent_dur_s ~
#                               GPS_TDR_order +
#                               june_day +
#                               day_period +
#                               chick_age +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# models_d_rate[[7]] <- glmer(descent_dur_s ~
#                               type +
#                               june_day +
#                               day_period +
#                               chick_age +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# models_d_rate[[8]] <- glmer(descent_dur_s ~
#                               june_day +
#                               day_period +
#                               chick_age +
#                               
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# models_d_rate[[9]] <- glmer(descent_dur_s ~
#                               1 + 
#                               (1|ring_number/june_day/dive_bout_id),
#                             data = dives_pdi)
# 
# 
# 
# 
# 
# # 
# # models[[2]] <- glmer(descent_dur_s~
# #                        GPS_TDR_order+type +
# #                        depth_max_m +
# #                        june_day +
# #                        day_period +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# # 
# # # models[[7]] <- glmer(descent_dur_s~
# # #                        GPS_TDR_order+
# # #                        type*depth_max_m +
# # #                        june_day +
# # #                        day_period +
# # #                        (1|ring_number/june_day/dive_bout_id),
# # #                      data = dives_pdi)
# # 
# # models[[3]] <- glmer(descent_dur_s~
# #                        GPS_TDR_order +
# #                        depth_max_m +
# #                        june_day +
# #                        day_period +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# # 
# # models[[4]] <- glmer(descent_dur_s~
# #                        type +
# #                        depth_max_m +
# #                        june_day +
# #                        day_period +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# # 
# # models[[5]] <- glmer((descent_dur_s) ~
# #                        depth_max_m +
# #                        june_day +
# #                        day_period +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# # 
# # # models[[7]] <- glmer(descent_dur_s~
# # #                        type +
# # #                        depth_max_m +
# # #                        june_day +
# # #                        # day_period +
# # #                        (1|ring_number/june_day/dive_bout_id),
# # #                      data = dives_pdi)
# # 
# # # Would some transformation help?
# # # MASS::boxcox(lm(pdi~depth_max_m,data=dives_pdi))
# # # Doesn't look like it!
# # 
# # models[[6]] <- glmer(descent_dur_s~
# #                        1 +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# 
# 
# 
# # Summarise information from the models
# models.aicc <- sapply(models_d_rate, AICc)
# models.aicc.dif <- models.aicc-min(models.aicc)
# models.r2m <- sapply(models_d_rate, r.squaredGLMM)
# t(models.r2m)
# # models.aic.wt <-  exp(-0.5 * models.aicc.dif)
# # models.aic.wt <-  models.aic.wt/sum(models.aic.wt)
# # format(models.aic.wt)
# # MuMIn::r.squaredGLMM
# models.fit.df <- cbind.data.frame(c(1:length(models_d_rate)),models.aicc,
#                                   models.aicc.dif,
#                                   # models.aic.wt,
#                                   t(models.r2m))
# names(models.fit.df) <- c("mod", "AICc", "dAICc",
#                           # "wAICc",
#                           "R2m", "R2c")
# 
# # anova(models[[2]],models[[3]])
# # anova(models[[2]],models[[4]])
# # anova(models[[2]],models[[5]])
# # anova(models[[4]],models[[5]])
# 
# # Significance for dropped terms
# drop1(models_d_rate[[2]], test = "user", sumFun = KRSumFun)
# 
# 
# # Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.
# 
# # "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# # From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/
# 
# 
# # Confidence intervals + coeficients
# model_va_coef <- summary(models_d_rate[[2]])$coef[, 1]
# model_va_ci <- confint(models_d_rate[[2]], method="Wald")
# model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])
# 
# summary(models_d_rate[[2]])
# # Check model behaviour
# plot(models_d_rate[[2]])
# qqmath(models_d_rate[[2]])
# 
# hist(residuals(models_d_rate[[2]]))
# 
# ggplot(dives_pdi, aes(x = (depth_max_m), y = (ascent_dur_s),
#                       group = type, colour = type))+
#   geom_point(alpha = 0.3) +
#   geom_density_2d()+
#   geom_smooth(method = "lm", se = TRUE)



# Descent rate ------

dives_df$descent_rate <- dives_df$descent_dur_s/(0.75*dives_df$depth_max_m)
# hist(dives_df$descent_rate, breaks = 100, xlim = c(0, 5))
# hist(log(dives_df$descent_rate + 1))
# plot(dives_df$descent_rate~dives_df$depth_max_m)     
#      
dives_drate <- dives_df[dives_df$depth_max_m>10,]

hist(dives_drate$descent_rate, breaks = 100, xlim = c(0, 5))
hist(log(dives_drate$descent_rate),100)

# Stats models.2
mods.drate <- list()




mods.drate[[1]] <- glmer(descent_rate ~
                              GPS_TDR_order*type +
                              june_day +
                              day_period +
                              depth_max_m +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)

mods.drate[[2]] <- glmer(descent_rate ~
                              GPS_TDR_order+type +
                              june_day +
                              day_period +                               depth_max_m +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)

mods.drate[[3]] <- glmer(descent_rate ~
                              GPS_TDR_order*type +
                              june_day +
                              day_period +                               depth_max_m +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)


mods.drate[[4]] <- glmer(descent_rate ~
                              june_day +
                              day_period +                               depth_max_m +
                              chick_age*type +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)

mods.drate[[5]] <- glmer(descent_rate ~
                              GPS_TDR_order+type +
                              june_day +
                              day_period +                               depth_max_m +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)

mods.drate[[6]] <- glmer(descent_rate ~
                              GPS_TDR_order +
                              june_day +
                              day_period +                               depth_max_m +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)

mods.drate[[7]] <- glmer(descent_rate ~
                              type +
                              june_day +
                              day_period +                               depth_max_m +
                              chick_age +
                              
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)

mods.drate[[8]] <- glmer(descent_rate ~
                              june_day +
                              day_period +
                              depth_max_m +
                              chick_age +
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)

mods.drate[[9]] <- glmer(descent_rate ~
                              1 + 
                              (1|ring_number/june_day/dive_bout_id),
                            data = dives_drate)


# 
# mods.drate[[10]] <- glmer(log(descent_rate) ~
#                            june_day +
#                            day_period +
#                            depth_max_m +
#                            chick_age +
#                            (1|ring_number/june_day/dive_bout_id),
#                          data = dives_drate)
# 
# # 
# # str(dives_pdi)
# models.2[[1]] <- glmer(descent_rate~
#                        GPS_TDR_order*type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# # r.squaredGLMM(models.2[[1]])
# 
# models.2[[2]] <- glmer(descent_rate~
#                        GPS_TDR_order+type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# # models.2[[7]] <- glmer(descent_dur_s/depth_max_m~
# #                        GPS_TDR_order+
# #                        type*depth_max_m +
# #                        june_day +
# #                        day_period +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# 
# models.2[[3]] <- glmer(descent_rate~
#                        GPS_TDR_order +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models.2[[4]] <- glmer(descent_rate~
#                        type +
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 
# models.2[[5]] <- glmer(descent_rate~
#                        depth_max_m +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# # 
# # models.2[[7]] <- glmer(descent_rate~
# #                          type*divetype +
# #                          june_day +
# #                          day_period +
# #                          (1|ring_number/june_day/dive_bout_id),
# #                        data = dives_pdi)
# # dives_pdi$divetype
# # models.2[[7]] <- glmer(descent_dur_s/depth_max_m~
# #                        type +
# #                        depth_max_m +
# #                        june_day +
# #                        # day_period +
# #                        (1|ring_number/june_day/dive_bout_id),
# #                      data = dives_pdi)
# 
# # Would some transformation help?
# # MASS::boxcox(lm(pdi~depth_max_m,data=dives_pdi))
# # Doesn't look like it!
# 
# models.2[[6]] <- glmer(descent_rate~
#                        1 +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_pdi)
# 




# Summarise information from the models.2
models.2.aicc <- sapply(mods.drate, AICc)
models.2.aicc.dif <- models.2.aicc-min(models.2.aicc)
models.2.r2m <- sapply(mods.drate, r.squaredGLMM)
# t(models.2.r2m)
# models.2.aic.wt <-  exp(-0.5 * models.2.aicc.dif)
# models.2.aic.wt <-  models.2.aic.wt/sum(models.2.aic.wt)
# format(models.2.aic.wt)
# MuMIn::r.squaredGLMM
models.2.fit.df <- cbind.data.frame(c(1:length(mods.drate)),models.2.aicc,
                                  models.2.aicc.dif,
                                  # models.2.aic.wt,
                                  t(models.2.r2m))
names(models.2.fit.df) <- c("mod", "AICc", "dAICc",
                          # "wAICc",
                          "R2m", "R2c")
# 
# anova(models.2[[2]],models.2[[3]])
# anova(models.2[[2]],models.2[[4]])
# anova(models.2[[2]],models.2[[5]])
# anova(models.2[[4]],models.2[[5]])

# Significance for dropped terms
drop1(mods.drate[[8]], test = "user", sumFun = KRSumFun)


# Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models.2–the R package pbkrtest. Journal of Statistical Software 59, 1–30.

# "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/


# Confidence intervals + coeficients
model_va_coef <- summary(mods.drate[[8]])$coef[, 1]
model_va_ci <- confint(mods.drate[[8]], method="Wald")
model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])

summary(mods.drate[[8]])
# Check model behaviour
plot(mods.drate[[8]])
qqmath(mods.drate[[8]])
# qqmath(mods.drate[[10]])
# 
#        hist(dives_df$descent_rate, breaks = 100)
#        hist(log(dives_df$descent_rate), breaks = 100)
#        hist(sqrt(dives_df$descent_rate), breaks = 100)
#        hist((dives_df$descent_rate)^2, breaks = 100)
#        
       
       
ggplot(dives_pdi, aes(x = (depth_max_m), y = descent_rate,
                      group = day_period, colour = day_period))+
  geom_point(alpha = 0.3) +
  geom_density_2d()+
  geom_smooth(method = "lm", se = TRUE) +
  ylim(c(0.3,1.5))




plot(dives_df$pdi~ dives_df$duration_s,
     ylim = c(0,200))


summary(as.factor(dives_df$day_period))
hist(dives$sun_elevation, breaks = 40)
median(dives$sun_elevation)
plot(dives$sun_elevation~dives$date_time)


# 
# # SST --------
# 
# range(dives_df$temp_c_start)
# hist(dives_df$temp_c_start, breaks = 100)
# summary(dives_df$temp_c_start)
# 
# # Exclude small number of dives with extreme high values
# dives_df_sst <- dives_df[dives_df$temp_c_start< 20 &
#                            dives_df$pause_pre_s < 200 &
#                            dives_df$pause_pre_s != 0,]
# 
# hist(dives_df_sst$pause_pre_s, xlim = c(0,100), breaks = 10000)
# summary(dives_df_sst$pause_pre_s <15)
# 
# f <- dives_df_sst$pause_pre_s <30
# par(mfrow=c(2,1))
# hist(dives_df_sst$temp_c_start[f], xlim = c(8,20), breaks = 100)
# hist(dives_df_sst$temp_c_start[!f], xlim = c(8,20), breaks = 100)
# summary(dives_df_sst$temp_c_start)
# summary(dives_df_sst$temp_c_start[!f])
# summary(dives_df_sst$temp_c_start[f])
# 
# summary(lm(dives_df_sst$temp_c_start~dives_df_sst$pause_pre_s))
# par(mfrow=c(1,1))
# plot(dives_df_sst$temp_c_start~dives_df_sst$pause_pre_s)
# lines(lowess(dives_df_sst$temp_c_start~dives_df_sst$pause_pre_s, f =0.05), col="blue") 
# # ?lowess
# models <- list()
# 
# 
# models[[1]] <- glmer(temp_c_start ~
#                        GPS_TDR_order*type +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_sst)
# 
# 
# 
# models[[2]] <- glmer(temp_c_start ~
#                        GPS_TDR_order+type +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_sst)
# 
# models[[3]] <- glmer(temp_c_start ~
#                        GPS_TDR_order +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_sst)
# 
# models[[4]] <- glmer(temp_c_start ~
#                        type +
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_sst)
# 
# models[[5]] <- glmer(temp_c_start ~
#                        june_day +
#                        day_period +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_sst)
# 
# models[[6]] <- glmer(temp_c_start ~
#                        1 +
#                        (1|ring_number/june_day/dive_bout_id),
#                      data = dives_df_sst)
# 
# 
# 
# # Summarise information from the models
# models.aicc <- sapply(models, AICc)
# models.aicc.dif <- models.aicc-min(models.aicc)
# 
# models.aic <- sapply(models, AIC)
# models.aic.dif <- models.aic-min(models.aic)
# 
# 
# models.r2m <- sapply(models, r.squaredGLMM)
# t(models.r2m)
# # MuMIn::r.squaredGLMM
# models.fit.df <- cbind.data.frame(c(1:length(models)),models.aicc,
#                                   models.aicc.dif,
#                                   models.aic,
#                                   models.aic.dif,
#                                   t(models.r2m))
# names(models.fit.df) <- c("mod", "AICc", "dAICc",
#                           "AIC", "dAIC", "R2m", "R2c")
# 
# 
# # Significance for dropped terms
# drop1(models[[1]], test = "user", sumFun = KRSumFun)
# 
# # Cite: Halekoh, U., and Højsgaard, S. (2014). A kenward-roger approximation and parametric bootstrap methods for tests in linear mixed models–the R package pbkrtest. Journal of Statistical Software 59, 1–30.
# 
# # "The result could reported as a Kenward-Roger corrected test with F(1, 118.5) = 16.17, p = .0001024"
# # From: https://seriousstats.wordpress.com/tag/kenward-roger-approximation/
# 
# 
# # Confidence intervals + coeficients
# model_va_coef <- summary(models[[1]])$coef[, 1]
# model_va_ci <- confint(models[[1]], method="Wald")
# model_va_par_df <- cbind.data.frame(model_va_coef,model_va_ci[-c(1:4),])
# 
# summary(models[[1]])
# # Check model behaviour
# plot(models[[1]])
# qqmath(models[[1]])
# 


# Post vs. pre interval ------

# Post
dives_pdi <- dives_df[!(dives_df$divetype %in% c("last", "single")),]
hist(dives_pdi$pdi, breaks = 100)
hist(dives_pdi$pdi, breaks = 1000, xlim = c(0,400))

# Remove extreme values for pdi and shallow dives
dives_pdi <- dives_pdi[dives_pdi$pdi < 300 &
                         dives_pdi$depth_max_m > 10,]
dives_pdi <- dives_pdi[!is.na(dives_pdi$pdi),]
dives_pdi <- dives_pdi[(dives_pdi$pdi) != 0,]

range(dives_pdi$pdi)


# Stats models
# models <- list()


model.post <- glmer(pdi~
                       GPS_TDR_order*type +
                       depth_max_m +
                       june_day +
                       day_period +
                       (1|ring_number/june_day/dive_bout_id),
                     data = dives_pdi)


# Pre


# Post
dives_pdi_pre <- dives_df[!(dives_df$divetype %in% c("last", "single", "first")),]
hist(dives_pdi_pre$pause_pre_s, breaks = 100)
hist(dives_pdi_pre$pause_pre_s, breaks = 1000, xlim = c(0,400))

# Remove extreme values for pdi and shallow dives
dives_pdi_pre <- dives_pdi[dives_pdi$pause_pre_s < 300 &
                         dives_pdi$depth_max_m > 10,]
dives_pdi_pre <- dives_pdi_pre[!is.na(dives_pdi_pre$pause_pre_s),]
dives_pdi_pre <- dives_pdi_pre[(dives_pdi_pre$pause_pre_s) != 0,]

model.pre <- glmer(pause_pre_s~
                      GPS_TDR_order*type +
                      depth_max_m +
                      june_day +
                      day_period +
                      (1|ring_number/june_day/dive_bout_id),
                    data = dives_pdi_pre)

# dives_pdi$

r.squaredGLMM(model.post)
r.squaredGLMM(model.pre)
