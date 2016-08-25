

# Read in raw data ----
deployments <- read.csv("all_caps_weight_cort.csv", header = TRUE)

str(deployments)

deployments$Date <- as.Date(deployments$Date, tz = "UTC")

# Make calculations ----

# day of June
deployments$june_day <- as.numeric(format(deployments$Date, "%d"))

# Residual mass (following seasonal correction)
deployments$Mass_resid <- deployments$Mass-973.8 + 3.8*deployments$june_day
# hist(deployments$Mass_resid)

# Change in mass/ Cort
# From last event
dep.ord <- order(deployments$Metal, deployments$Capture)
deployments <- deployments[dep.ord,]


# From last
deployments$mass_change_last <- NA

# 1 to 2
deployments$mass_change_last[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$Mass[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$Mass[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$mass_change_last[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$Mass[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$Mass[deployments$TYPE == "device" & deployments$Capture == 2]


# From start
deployments$mass_change_start <- NA

# 1 to 2
deployments$mass_change_start[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$Mass[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$Mass[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$mass_change_start[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$Mass[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$Mass[deployments$TYPE == "device" & deployments$Capture == 1]



# From last
deployments$mass_resid_change_last <- NA

# 1 to 2
deployments$mass_resid_change_last[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$Mass_resid[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$Mass_resid[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$mass_resid_change_last[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$Mass_resid[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$Mass_resid[deployments$TYPE == "device" & deployments$Capture == 2]


# From start
deployments$mass_resid_change_start <- NA

# 1 to 2
deployments$mass_resid_change_start[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$Mass_resid[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$Mass_resid[deployments$TYPE != "background" & deployments$Capture == 1]
levels(deployments$TYPE)
# 2 to 3
deployments$mass_resid_change_start[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$Mass_resid[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$Mass_resid[deployments$TYPE == "device" & deployments$Capture == 1]




# days since last
deployments$days_dep <- NA

# 1 to 2
deployments$days_dep[deployments$TYPE != "background" & deployments$Capture == 2] <- deployments$june_day[deployments$TYPE != "background" & deployments$Capture == 2] - deployments$june_day[deployments$TYPE != "background" & deployments$Capture == 1]

# 2 to 3
deployments$days_dep[deployments$TYPE == "device" & deployments$Capture == 3] <- deployments$june_day[deployments$TYPE == "device" & deployments$Capture == 3] - deployments$june_day[deployments$TYPE == "device" & deployments$Capture == 2]


# Changes per day 
deployments$mass_change_start_day <- deployments$mass_change_start / deployments$days_dep
deployments$mass_change_last_day <- deployments$mass_change_last/ deployments$days_dep
deployments$mass_resid_change_start_day <- deployments$mass_resid_change_start/ deployments$days_dep
deployments$mass_resid_change_last_day <- deployments$mass_resid_change_last/ deployments$days_dep



# Make new data-frame ----

# Save to RData file
save(deployments, file = "deployments_mass_calc.RData")

# Save to csv file
write.csv(deployments, file = "deployments_mass_calc.csv")

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

# - Actual mass ----
p <- ggplot(deployments, aes(june_day, Mass,
                             colour = group, group = Metal, shape = group)) +
  geom_point(aes(june_day, Mass),
             alpha=0.8,
             size=3,
             show.legend =TRUE) +
  geom_line() +
   theme_bw()
p <- p  + scale_colour_manual(values=col_3)
p <- p  + labs(list(x = "Date (day of June)", y =  "Mass (g)", shape = "Group", col = "Group", fill = "Treatment"))
p <- p + theme(legend.key.width=unit(2,"line"))
p <- p + geom_abline(intercept = 973.791, slope = -3.829,
                     alpha = 0.5,lwd = 2,
                     lty = 2, col = "grey60")

p
ggsave(filename = "change_mass_date.svg", width = 6, height = 4,
       units = "in")
ggsave(filename = "change_mass_date.png", width = 6, height = 4,
       units = "in")
ggsave(filename = "change_mass_date.pdf", width = 6, height = 4,
       units = "in")
# ?ggsave


# - Residual mass -----
p <- ggplot(deployments, aes(june_day, Mass_resid,
                             colour = group, group = Metal, shape = group)) +
  geom_point(aes(june_day, Mass_resid),
             alpha=0.8,
             size=3,
             show.legend =TRUE) +
  geom_line() +
  theme_bw()
p <- p  + scale_colour_manual(values=col_3)
p <- p  + labs(list(x = "Date (day of June)", y =  "Residual mass (g)", shape = "Group", col = "Group", fill = "Treatment"))
p <- p + theme(legend.key.width=unit(2,"line"))
p <- p + geom_abline(intercept = 0, slope = 0,
                     alpha = 0.5,lwd = 2,
                     lty = 2, col = "grey60")

p
ggsave(filename = "change_mass_resid_date.svg", width = 6, height = 4,
       units = "in")
ggsave(filename = "change_mass_resid_date.png", width = 6, height = 4,
       units = "in")
ggsave(filename = "change_mass_resid_date.pdf", width = 6, height = 4,
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


# deployments$device.status
# Device deployments only ----
mods <- list()
mods[1] <- lmer(mass_resid_change_last_day ~
                        device.status*group +
        (1|Metal),
      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[2] <- lmer( mass_resid_change_last_day ~
                       device.status+group +
                       (1|Metal),
                     data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

# anova(mods[[1]], mods[[2]])

mods[3] <- lmer( mass_resid_change_last_day ~
                        device.status +
                        (1|Metal),
                      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[4] <- lmer( mass_resid_change_last_day ~
                        1 +
                        (1|Metal),
                      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

mods[5] <- lmer( mass_resid_change_last_day ~
                        group +
                        (1|Metal),
                      data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])

summary(mods[[1]])

# a <- AICc(mod.devices)
# AICc(mod.devices2) -a
# AICc(mod.devices3) -a
# AICc(mod.devices4) - a
# AICc(mod.devices5) -a 
# 
# r.squaredGLMM(mod.devices)
# r.squaredGLMM(mod.devices2)
# r.squaredGLMM(mod.devices3)
# r.squaredGLMM(mod.devices4)
# r.squaredGLMM(mod.devices5)

# Summarise information from the models
mods.aicc <- sapply(mods, AICc)
mods.aicc.dif <- mods.aicc-min(mods.aicc)
mods.r2m <- sapply(mods, r.squaredGLMM)
t(mods.r2m)

mods.fit.df <- cbind.data.frame(c(1:length(mods)),mods.aicc,
                                         mods.aicc.dif,
                                         t(mods.r2m))
names(mods.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

# Check model behaviour
plot(mods[[1]])
qqmath(mods[[1]])

ggplot(aes(y = mass_resid_change_last_day, x = device.status,
           fill = group),
       data = deployments[deployments$TYPE == "device" & deployments$Capture != 1,])+
  geom_boxplot(outlier.size = 0, alpha = 0.7) +
  geom_point(pch = 21, position = position_jitterdodge(),
             alpha = 0.6)+
  labs(list(x = "Device type",
            y = expression(Delta~~"Mass per day"~~(g.day^-1)~~""),
            fill = "Order")) +
  theme_new +
  scale_colour_manual(values=col_3[1:2]) +
  scale_fill_manual(values=col_3[1:2]) +
  theme(legend.position = c(0.4, 1))

ggsave(filename = "change_mass_resid_date_boxplots.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_mass_resid_date_boxplots.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_mass_resid_date_boxplots.pdf", width = 4, height = 4,
       units = "in")
# deployments$group




# P values for gull model
drop1(mod.devices, test="user", sumFun=KRSumFun)

mod.devices_coef <- summary(mod.devices)$coef[, 1]
mod.devices_ci <- confint(mod.devices, method="Wald")
mod.devices_par_df <- cbind.data.frame(mod.devices_coef,mod.devices_ci[-c(1:2),])



mod.devices_coef <- summary(mod.devices2)$coef[, 1]
mod.devices_ci <- confint(mod.devices2, method="Wald")
mod.devices_par_df <- cbind.data.frame(mod.devices_coef,mod.devices_ci[-c(1:2),])






# deployments$device.status


# All 1st deployment only ----
mod.devices <- lm( mass_resid_change_last_day ~
                       group,
                     data = deployments[deployments$TYPE != "background" & deployments$Capture == 2,])
summary(mod.devices)
anova(mod.devices)

# ggplot(deployments[deployments$TYPE != "background" & deployments$Capture == 2,],
#        aes(x = group, y = mass_resid_change_last_day)) +
#   geom_boxplot() +
#   geom_point()



ggplot(deployments[deployments$TYPE != "background" & deployments$Capture == 2,],
       aes(x = group, fill = group, y = mass_resid_change_last_day))+
  geom_boxplot(outlier.size = 0, alpha = 0.7, show.legend = FALSE) +
  geom_point(pch = 21, position = position_jitter(),
             alpha = 0.6, show.legend = FALSE)+
  labs(list(x = "Group",
            y = expression(Delta~~"Mass per day"~~(g.day^-1)~~""),
            fill = "")) +
  theme_new +
  scale_colour_manual(values=col_3[1:3]) +
  scale_fill_manual(values=col_3[1:3]) +
  theme(legend.position = c(0.4, 1))

ggsave(filename = "change_mass_resid_date_boxplots2.svg", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_mass_resid_date_boxplots2.png", width = 4, height = 4,
       units = "in")
ggsave(filename = "change_mass_resid_date_boxplots2.pdf", width = 4, height = 4,
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
