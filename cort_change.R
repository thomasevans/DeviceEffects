

# Read in raw data ----
deployments <- read.csv("all_caps_weight_cort.csv", header = TRUE)

str(deployments)

deployments$Date <- as.Date(deployments$Date, tz = "UTC")

# Make calculations ----

# day of June
deployments$june_day <- as.numeric(format(deployments$Date, "%d"))

# Residual mass (following seasonal correction)
# deployments$Mass_resid <- deployments$Mass-973.8 + 3.8*deployments$june_day
# hist(deployments$Mass_resid)

deployments$CORT_log <- log10(deployments$CORT)


# 1st caps only
dep.1 <- deployments[deployments$Capture == 1,]
# Summary stats for CORT
summary(dep.1$CORT)
sd(dep.1$CORT)


# Change in CORT accros chick-rearing -----
mod.cort <- lm(dep.1$CORT_log~dep.1$june_day)
plot(mod.cort)
summary(mod.cort)

plot(dep.1$CORT~dep.1$june_day, log = "y")
abline(mod.cort, lty = 2)


# Change in mass/ Cort
# From last event
dep.ord <- order(deployments$Metal, deployments$Capture)
deployments <- deployments[dep.ord,]



# From last
deployments$CORT_change_last <- NA

# 1 to 2
deployments$CORT_change_last[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$CORT[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$CORT[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$CORT_change_last[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$CORT[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$CORT[deployments$TYPE == "device" & deployments$Capture == 2]


# From start
deployments$CORT_change_start <- NA

# 1 to 2
deployments$CORT_change_start[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$CORT[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$CORT[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$CORT_change_start[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$CORT[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$CORT[deployments$TYPE == "device" & deployments$Capture == 1]

# From last
deployments$CORT_log_log_change_last <- NA

# 1 to 2
deployments$CORT_log_change_last[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$CORT_log[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$CORT_log[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$CORT_log_change_last[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$CORT_log[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$CORT_log[deployments$TYPE == "device" & deployments$Capture == 2]


# From start
deployments$CORT_log_change_start <- NA

# 1 to 2
deployments$CORT_log_change_start[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$CORT_log[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$CORT_log[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$CORT_log_change_start[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$CORT_log[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$CORT_log[deployments$TYPE == "device" & deployments$Capture == 1]





# days since last
deployments$days_dep <- NA

# 1 to 2
deployments$days_dep[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$june_day[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$june_day[deployments$TYPE != "background" & deployments$Capture == 1]

# 2 to 3
deployments$days_dep[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$june_day[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$june_day[deployments$TYPE == "device" & deployments$Capture == 2]


# Changes per day 
deployments$CORT_change_start_day <- deployments$CORT_change_start / deployments$days_dep
deployments$CORT_change_last_day <- deployments$CORT_change_last/ deployments$days_dep
deployments$CORT_log_change_start_day <- deployments$CORT_log_change_start/ deployments$days_dep
deployments$CORT_log_change_last_day <- deployments$CORT_log_change_last/ deployments$days_dep



# Make new data-frame ----

# Save to RData file
save(deployments, file = "deployments_mass_calc_cort.RData")

# Save to csv file
write.csv(deployments, file = "deployments_mass_calc_cort.csv")

# Make ggplot figures for mass changes -----

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

# Give groups labels
deployments$group <- NA
levels(deployments$group) <-  c("+G1", "+G2", "C", "CB")
deployments$group[deployments$TYPE == "background"] <- "CB"
deployments$group[deployments$TYPE == "CORTcontrol"] <- "C"

first_device <- deployments$Metal[deployments$TYPE == "device" &
                                    deployments$Capture == 2 &
                                    deployments$device.status == "GPS+TDR"] 
second_device <- deployments$Metal[deployments$TYPE == "device" &
                                     deployments$Capture == 3 &
                                     deployments$device.status == "GPS+TDR"] 
deployments$group[deployments$Metal %in% first_device] <- "+G1"
deployments$group[deployments$Metal %in% second_device] <- "+G2"

deployments$group <- as.factor(deployments$group)

# - Log CORT ----
p <- ggplot(deployments, aes(june_day, CORT_log,
                             colour = group, group = Metal, shape = group)) +
  geom_point(aes(june_day, CORT_log),
             alpha=0.8,
             size=3,
             show.legend =TRUE) +
  geom_line() +
  theme_bw()
p <- p  + scale_colour_manual(values=col_3)
p <- p  + labs(list(x = "Date (day of June)", y =  expression("CORT"~~(LOG[10])~~""), shape = "Group", col = "Group", fill = "Treatment"))
p <- p + theme(legend.key.width=unit(2,"line"))
p <- p + geom_abline(intercept = 973.791, slope = -3.829,
                     alpha = 0.5,lwd = 2,
                     lty = 2, col = "grey60")

p
ggsave(filename = "change_CORT_date.svg", width = 6, height = 4,
       units = "in")
ggsave(filename = "change_CORT_date.png", width = 6, height = 4,
       units = "in")
ggsave(filename = "change_CORT_date.pdf", width = 6, height = 4,
       units = "in")



# LMM models -----
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





# Actual CORT levels -----
mods <- list()
mods[1] <- lmer( CORT_log ~
                   device.status*group +june_day +
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[2] <- lmer( CORT_log ~
                   device.status+group + june_day +
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[3] <- lmer( CORT_log ~
                   device.status + june_day + 
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[4] <- lmer( CORT_log ~
                   group + june_day +
                   1 +
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[5] <- lmer( CORT_log ~
                   june_day +
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

# deployments$june_day
mods[6] <- lmer( CORT_log ~
                   1 +
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])
# 
# mods[7] <- lmer( CORT_log ~
#                    device.status*group+ june_day +
#                    (1|Metal),
#                  data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

# Summarise information from the models
mods.aicc <- sapply(mods, AICc)
mods.aicc.dif <- mods.aicc-min(mods.aicc)
mods.r2m <- sapply(mods, r.squaredGLMM)
t(mods.r2m)
# MuMIn::r.squaredGLMM
mods.fit.df <- cbind.data.frame(c(1:length(mods)),mods.aicc,
                                mods.aicc.dif,
                                t(mods.r2m))
names(mods.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

mods.fit.df
summary(mods[[1]])
plot(mods[[4]])
qqmath(mods[[4]])


# Device deployments only ----
mods <- list()
mods[1] <- lmer( CORT_log_change_last_day ~
                       device.status*group +
                       (1|Metal),
                     data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[2] <- lmer( CORT_log_change_last_day ~
                        device.status+group +
                        (1|Metal),
                      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[3] <- lmer( CORT_log_change_last_day ~
                        device.status +
                        (1|Metal),
                      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[4] <- lmer( CORT_log_change_last_day ~
                        1 +
                        (1|Metal),
                      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[5] <- lmer( CORT_log_change_last_day ~
                        group +
                        (1|Metal),
                      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

# deployments$june_day
mods[6] <- lmer( CORT_log_change_last_day ~
                    june_day +
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[7] <- lmer( CORT_log_change_last_day ~
                   device.status*group+ june_day +
                   (1|Metal),
                 data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

# Summarise information from the models
mods.aicc <- sapply(mods, AICc)
mods.aicc.dif <- mods.aicc-min(mods.aicc)
mods.r2m <- sapply(mods, r.squaredGLMM)
t(mods.r2m)
# MuMIn::r.squaredGLMM
mods.fit.df <- cbind.data.frame(c(1:length(mods)),mods.aicc,
                                  mods.aicc.dif,
                                  t(mods.r2m))
names(mods.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")



summary(mod.devices)

AICc(mod.devices) -a
AICc(mod.devices2) -a
AICc(mod.devices3) -a
a <- AICc(mod.devices4)
AICc(mod.devices5) -a 

r.squaredGLMM(mod.devices)
r.squaredGLMM(mod.devices2)
r.squaredGLMM(mod.devices3)
r.squaredGLMM(mod.devices4)
r.squaredGLMM(mod.devices5)


ggplot(aes(y = CORT_log_change_last_day, x = device.status,
           fill = group),
       data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])+
  geom_boxplot(outlier.size = 0, alpha = 0.7) +
  geom_point(pch = 21, position = position_jitterdodge(),
             alpha = 0.6)+
  labs(list(x = "Device type",
            y = expression(Delta~~"CORT"~~(Log[10])~~"per day"),
            fill = "Order")) +
  theme_new +
  scale_colour_manual(values=col_3[1:2]) +
  scale_fill_manual(values=col_3[1:2]) +
  theme(legend.position = c(0.7, 1))

ggsave(filename = "change_CORT_log_date_boxplots.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_CORT_log_date_boxplots.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_CORT_log_date_boxplots.pdf", width = 4, height = 4,
       units = "in")
# deployments$group




# P values for gull model
drop1(mod.devices, test="user", sumFun=KRSumFun)

mod.devices_coef <- summary(mod.devices)$coef[, 1]
mod.devices_ci <- confint(mod.devices, method="Wald")
mod.devices_par_df <- cbind.data.frame(mod.devices_coef,mod.devices_ci[-c(1:2),])

# 
# 
# mod.devices_coef <- summary(mod.devices2)$coef[, 1]
# mod.devices_ci <- confint(mod.devices2, method="Wald")
# mod.devices_par_df <- cbind.data.frame(mod.devices_coef,mod.devices_ci[-c(1:2),])






# deployments$device.status
deployments$CORT_log_change_last

# All 1st deployment only ----
mod.devices <- lm( CORT_log_change_last_day ~
                     group,
                   data = deployments[deployments$TYPE != "background" & deployments$Capture == 2,])
summary(mod.devices)
anova(mod.devices)


mod.devices <- lm( CORT_log_change_last ~
                     group,
                   data = deployments[deployments$TYPE != "background" & deployments$Capture == 2,])
summary(mod.devices)

# ggplot(deployments[deployments$TYPE != "background" & deployments$Capture == 2,],
#        aes(x = group, y = mass_resid_change_last_day)) +
#   geom_boxplot() +
#   geom_point()



ggplot(deployments[deployments$TYPE != "background" & deployments$Capture == 2,],
       aes(x = group, fill = group, y = CORT_log_change_last_day))+
  geom_boxplot(outlier.size = 0, alpha = 0.7, show.legend = FALSE) +
  geom_point(pch = 21, position = position_jitter(),
             alpha = 0.6, show.legend = FALSE)+
  labs(list(x = "Group",
            y = expression(Delta~~"CORT"~~(Log[10])~~"per day"),
            fill = "")) +
  theme_new +
  scale_colour_manual(values=col_3[1:3]) +
  scale_fill_manual(values=col_3[1:3]) +
  theme(legend.position = c(0.4, 1))

ggsave(filename = "change_CORT_log_date_boxplots2.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_CORT_log_date_boxplots2.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_CORT_log_date_boxplots2.pdf", width = 4, height = 4,
       units = "in")



# 
# plot( mass_resid_change_last_day ~
#         group,
#       data = deployments[deployments$TYPE != "background" & deployments$Capture == 2,])
# str(deployments$group)
# 
# 
# 
# summary(mod.devices)
# 
# AICc(mod.devices)

# deployments$group
# 
# 
# # Comparing controls (2nd cap) with deviced (3rd cap)
# control_mass <- deployments$mass_resid_change_start_day[deployments$group == "C" & deployments$Capture == 2]
# deviced_mass <- deployments$mass_resid_change_start_day[deployments$group != "C" & deployments$Capture == 3]
# t.test(control_mass, deviced_mass)
# 
# boxplot(control_mass, deviced_mass)
