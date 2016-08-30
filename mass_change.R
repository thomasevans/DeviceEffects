

# Read in raw data ----
deployments <- read.csv("all_caps_weight_cort.csv", header = TRUE)

deployments$Date <- as.Date(deployments$Date, tz = "UTC")

unique_dep <- deployments[deployments$Capture ==1,]
summary(unique_dep$TYPE)
length(unique_dep$TYPE)

str(deployments)

# Background mass change ----

unique_dep$june_day <- as.numeric(format(unique_dep$Date, "%d"))

mod.mass <- lm(unique_dep$Mass~unique_dep$june_day)
summary(mod.mass)

confint(mod.mass)

plot(mod.mass)

mean(unique_dep$Mass)
sd(unique_dep$Mass)

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




# Summary mass change figure ---------

# First figure - all birds actual masses by date
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





# Make plot with base graphics -----
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

par(ps = 14, cex = 1.5, cex.lab = 2)
svg("mass_change.svg",
    width = 6, height = 4, family = "serif")
# ?cairo_ps
# Plot base map
par(mfrow = c(1,1))
par(mar=c(4, 5, 1, 1) + 0.1)   
# ?par
plot(deployments$Mass~deployments$june_day,
     type = "n",
     ylab = "Mass (g)",
     xlab = "Date (day of June)",
     las = 1,
     ylim = c(800, 1026),
     xlim = c(7, 26),
     cex.lab = 1.3
)
grid(lty = 5)

# Control birds
points(deployments$Mass[!bird.exp]~
         deployments$june_day[!bird.exp],
       pch = 21,
       col = "dark grey",
       bg = "grey")

# Add Experimental points
bird.exp <- deployments$group %in% c("+G1", "+G2")
points(deployments$Mass[bird.exp]~
         deployments$june_day[bird.exp],
       pch = 21,
       bg = "black")


deployments <- deployments[order(deployments$Metal),]

# With GPS lines
segments(deployments$june_day[deployments$group == "+G1" &
                                deployments$Capture == 1],
         deployments$Mass[deployments$group == "+G1" &
                            deployments$Capture == 1],
         deployments$june_day[deployments$group == "+G1" &
                                deployments$Capture == 2],
         deployments$Mass[deployments$group == "+G1" &
                            deployments$Capture == 2],
         lwd = 2
         )
segments(deployments$june_day[deployments$group == "+G2" &
                                deployments$Capture == 2],
         deployments$Mass[deployments$group == "+G2" &
                            deployments$Capture == 2],
         deployments$june_day[deployments$group == "+G2" &
                                deployments$Capture == 3],
         deployments$Mass[deployments$group == "+G2" &
                            deployments$Capture == 3],
         lwd = 2
)

# With TDR lines
segments(deployments$june_day[deployments$group == "+G1" &
                                deployments$Capture == 2],
         deployments$Mass[deployments$group == "+G1" &
                            deployments$Capture == 2],
         deployments$june_day[deployments$group == "+G1" &
                                deployments$Capture == 3],
         deployments$Mass[deployments$group == "+G1" &
                            deployments$Capture == 3],
         lty = 2,
         lwd = 2
)
segments(deployments$june_day[deployments$group == "+G2" &
                                deployments$Capture == 1],
         deployments$Mass[deployments$group == "+G2" &
                            deployments$Capture == 1],
         deployments$june_day[deployments$group == "+G2" &
                                deployments$Capture == 2],
         deployments$Mass[deployments$group == "+G2" &
                            deployments$Capture == 2],
         lty = 2,
         lwd = 2
)

# Controls
segments(deployments$june_day[deployments$group == "C" &
                                deployments$Capture == 1],
         deployments$Mass[deployments$group == "C" &
                            deployments$Capture == 1],
         deployments$june_day[deployments$group == "C" &
                                deployments$Capture == 2],
         deployments$Mass[deployments$group == "C" &
                            deployments$Capture == 2],
         col = "dark grey",
         lty = 2,
         lwd = 2
)

points(deployments$Mass[bird.exp & deployments$Capture == 1]~
         deployments$june_day[bird.exp & deployments$Capture == 1],
       pch = 21,
       bg = "grey",
       col = "black",
       lwd = 2)

# Colony level mass loss
abline(coef = c(973.791, -3.829),
       lwd = 5,
       col = add.alpha("dark grey", 0.5),
       lty = 3
)


# Adding key
legend(x = 21, y = 1028,
       legend = c(
         "Exp (1st cap)",
         "Exp (GPS+TDR)",
                  "Exp (TDR)",
                  "Control",
                  "Colony trend"),
       col = c("black", "black", "black", "grey", "grey"),
       pt.bg = c("grey", "black", "black", "dark grey", NA),
       lty = c(NA, 1, 2, 2, 3),
       lwd = c(2, 2, 2, 2, 5),
       pt.lwd = c(1,1,1,1,1),
       # bg = c("black", "grey"),
       pch = c(21, 21, 21, 21, NA),
       y.intersp = 0.9,
       seg.len = 3,
       cex = 0.7
         )
# ?legend
dev.off()




# Plot of mass changes ----------

# Colony level trend
cap1 <- deployments[deployments$Capture == 1,]
col.mass <- lm(cap1$Mass~cap1$june_day)
summary(col.mass)
confint(col.mass)
# 
# > confint(col.mass)
# 2.5 %       97.5 %
#   (Intercept)   938.08957 1024.3210114
# cap1$june_day  -6.37186   -0.8736247


fun_summary <- function(x){
  m <- mean(x)
  se <- sd(x)/sqrt(length(x))
  e <- qnorm(0.975)*se
  return(c(m,m-e,m+e,m-se,m+se))
}


# GPS+TDR
# GPS first
gps.1.gps <- fun_summary(deployments$mass_change_last_day[deployments$group == "+G1" &
                                        deployments$Capture == 2])
# -10.933333 -15.552840  -6.313827 -13.290268  -8.576399
gps.1.gps.points <- deployments$mass_change_last_day[deployments$group == "+G1" &
                                                       deployments$Capture == 2]

# TDR first
gps.2.gps <- fun_summary(deployments$mass_change_last_day[deployments$group == "+G2" &
                                             deployments$Capture == 3])
# -9.250000 -13.374859  -5.125141 -11.354559  -7.145441
gps.2.gps.points <- deployments$mass_change_last_day[deployments$group == "+G2" &
                                                       deployments$Capture == 3]

# TDR only
# GPS first
gps.1.tdr <- fun_summary(deployments$mass_change_last_day[deployments$group == "+G1" &
                                             deployments$Capture == 3])
# 0.9166667 -3.5640652  5.3973985 -1.3694630  3.2027963
gps.1.tdr.points <- deployments$mass_change_last_day[deployments$group == "+G1" &
                                                       deployments$Capture == 3]

# TDR first
gps.2.tdr <- fun_summary(deployments$mass_change_last_day[deployments$group == "+G2" &
                                             deployments$Capture == 2])
# -6.2222222 -17.7395523   5.2951079 -12.0985190  -0.3459254
gps.2.tdr.points <- deployments$mass_change_last_day[deployments$group == "+G2" &
                                                      deployments$Capture == 2]
  
# Controls
contols <- fun_summary(deployments$mass_change_last_day[deployments$group == "C" &
                                                          deployments$Capture == 2])
# -1.666667 -7.459203  4.125870 -4.622097  1.288764
controls.points <- deployments$mass_change_last_day[deployments$group == "C" &
                                                      deployments$Capture == 2]


# range(deployments$mass_change_last_day, na.rm = TRUE)


par(ps = 14, cex = 1.5, cex.lab = 2)
svg("mass_change_coef.svg",
    width = 6, height = 4, family = "serif")
# ?cairo_ps
# Plot base map
par(mfrow = c(1,1))
par(mar=c(4, 6, 1, 1) + 0.1)   
# ?par
plot(c(0,8)~c(-22,17),
     type = "n",
     xlab = expression("Change in mass ("~g.day^-1~")"),
     ylab = "",
     las = 1,
     cex.lab = 1.3,
     yaxt = "n"
)
# ?grid
# Add zero line
abline(v = 0, col = "grey70")



# Add colony level trend line
rect(-6.37186, -10, -0.8736247, 20,
     col = "light grey", border = NULL,
     lty = 1, lwd = NA
     )
abline(v=-3.623,
       lwd = 5,
       col = "dark grey",
       lty = 1)
abline(v=c(-6.37186, -0.8736247),
       lwd = 2,
       col = "dark grey",
       lty = 3)


# Add grid lines (vertical only)
abline(v = seq(-20,15,5), lty = 5, col = "grey")

#redraw box
box()

# m,-95,+95,m-se,m+se

# Add GPS
# GPS1 group
segments(gps.1.gps[2],7,gps.1.gps[3],7,
         lwd = 3, col = add.alpha("black", 0.3))
segments(gps.1.gps[4],7,gps.1.gps[5],7,
         lwd = 6, col = add.alpha("black", 0.3))
points(gps.1.gps[1],7,
       pch = 18,
       col = add.alpha("black", 0.5),
       cex = 2)
points(gps.1.gps.points,rep(7,length(gps.1.gps.points)),
       pch = 18,
       col = add.alpha("black", 0.5))

# GPS2 group
segments(gps.2.gps[2],6,gps.2.gps[3],6,
         lwd = 3, col = add.alpha("black", 0.3))
segments(gps.2.gps[4],6,gps.2.gps[5],6,
         lwd = 6, col = add.alpha("black", 0.3))
points(gps.2.gps[1],6,
       pch = 15,
       col = add.alpha("black", 0.5),
       cex = 2)
points(gps.2.gps.points,rep(6,length(gps.2.gps.points)),
       pch = 15,
       col = add.alpha("black", 0.5))


# Add TDR
# GPS1 group
segments(gps.1.tdr[2],4,gps.1.tdr[3],4,
         lwd = 3, col = add.alpha("black", 0.3))
segments(gps.1.tdr[4],4,gps.1.tdr[5],4,
         lwd = 6, col = add.alpha("black", 0.3))
points(gps.1.tdr[1],4,
       pch = 18,
       col = add.alpha("black", 0.5),
       cex = 2)
points(gps.1.tdr.points,rep(4,length(gps.1.tdr.points)),
       pch = 18,
       col = add.alpha("black", 0.5))

# GPS2 group
segments(gps.2.tdr[2],3,gps.2.tdr[3],3,
         lwd = 3, col = add.alpha("black", 0.3))
segments(gps.2.tdr[4],3,gps.2.tdr[5],3,
         lwd = 6, col = add.alpha("black", 0.3))
points(gps.2.tdr[1],3,
       pch = 15,
       col = add.alpha("black", 0.5),
       cex = 2)
points(gps.2.tdr.points,rep(3,length(gps.2.tdr.points)),
       pch = 15,
       col = add.alpha("black", 0.5))



# Control group
segments(contols[2],1,contols[3],1,
         lwd = 3, col = add.alpha("dark grey", 0.8))
segments(contols[4],1,contols[5],1,
         lwd = 6, col = add.alpha("dark grey", 0.8))
points(contols[1],1,
       pch = 21,
       col = add.alpha("black", 0.5),
       bg =  add.alpha("dark grey", 0.5),
       cex = 2)
points(controls.points,rep(1,length(controls.points)),
       pch = 21,
       col = add.alpha("black", 0.5),
       bg =  add.alpha("dark grey", 0.5))





# Adding key
legend(x = 4, y = 8,
       legend = c(
         "Exp (GPS+TDR -> TDR)",
         "Exp (TDR -> GPS+TDR)",
         "Control"),
       col = c("black", "black", "black"),
       pt.bg = c(NA, NA, "dark grey"),
       # lty = c(NA, 1, 2, 2, 3),
       # lwd = c(2, 2, 2, 2, 5),
       # pt.lwd = c(1,1,1,1,1),
       # bg = c("black", "grey"),
       pch = c(18, 15, 21),
       y.intersp = 0.9,
       seg.len = 2,
       cex = 0.7
)



mtext("GPS + TDR", side = 2,
      las = 1, at = 6.5, line = 5, adj = 0.2,
      cex = 1.3)
mtext("TDR", side = 2,
      las = 1, at = 3.5, line = 5, adj = 0.2,
      cex = 1.3)
mtext("Control", side = 2,
      las = 1, at = 1, line = 5, adj = 0.2,
      cex = 1.3)

dev.off()