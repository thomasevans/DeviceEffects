# Analyse and plot the weight data to compare body mass changes between deployments

# Get data from DB ------
library("RODBC")
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Get GPS data
tag.events <- sqlQuery(gps.db,
                      query = "SELECT guillemots_GPS_TDR_events.*
FROM guillemots_GPS_TDR_events
                      ORDER BY guillemots_GPS_TDR_events.ring_number, guillemots_GPS_TDR_events.GPS_TDR_event;",
                      as.is = TRUE)

# Drop record with missing data
tag.events <- tag.events[tag.events$ring_number != "AAZ988",]

# Do some calculations on this -----
# Weight changes (from start, and from previous occasion)
tag.events$weight_change_start <- NULL

tag.events$mass_change_start[tag.events$GPS_TDR_event == 1] <- tag.events$mass[tag.events$GPS_TDR_event == 1] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$mass_change_start[tag.events$GPS_TDR_event == 2] <- tag.events$mass[tag.events$GPS_TDR_event == 2] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$mass_change_start[tag.events$GPS_TDR_event == 3] <- tag.events$mass[tag.events$GPS_TDR_event == 3] - tag.events$mass[tag.events$GPS_TDR_event == 1]

# Weight changes (from from previous occasion)
tag.events$weight_change_previous <- NULL

tag.events$weight_change_previous[tag.events$GPS_TDR_event == 1] <- tag.events$mass[tag.events$GPS_TDR_event == 1] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$weight_change_previous[tag.events$GPS_TDR_event == 2] <- tag.events$mass[tag.events$GPS_TDR_event == 2] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$weight_change_previous[tag.events$GPS_TDR_event == 3] <- tag.events$mass[tag.events$GPS_TDR_event == 3] - tag.events$mass[tag.events$GPS_TDR_event == 2]


# Some base plots ------
boxplot(tag.events$mass_change_start~tag.events$GPS_TDR_event+tag.events$GPS_TDR_order)

boxplot(tag.events$weight_change_previous~tag.events$GPS_TDR_event+tag.events$GPS_TDR_order)


# Make some plots with ggplot -------
library("ggplot2")
# Get data into suitable format
str(tag.events)
tag.events$ring_number <- as.factor(tag.events$ring_number)
tag.events$GPS_TDR_order <- as.factor(tag.events$GPS_TDR_order)
tag.events$GPS_TDR_event <- as.factor(tag.events$GPS_TDR_event)



# Make an initial plot
p <- ggplot(tag.events, aes(GPS_TDR_event, mass))
p <- p + geom_boxplot()
p <- p + geom_line(aes(group = ring_number), colour = "red", alpha = 0.5)


tag.events$adjusted <- 0.2
tag.events$adjusted[tag.events$GPS_TDR_order == "GPS_first"] <- -0.2


# More fancy

# Actual mass
ggplot(tag.events, aes(GPS_TDR_event, mass, fill = GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted ,mass,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted ,mass, colour = GPS_TDR_order),
              alpha=0.8,
              size=3,
              show_guide=FALSE) +
  theme_bw()

# Change in mass sinse last event
tag.events2 <- tag.events[tag.events$GPS_TDR_event != 1,]
ggplot(tag.events2, aes(GPS_TDR_event, weight_change_previous, fill = GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , weight_change_previous,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,weight_change_previous, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=FALSE) +
  theme_bw()

# Change in mass sinse start
ggplot(tag.events2, aes(GPS_TDR_event, mass_change_start, fill = GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , mass_change_start,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,mass_change_start, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=FALSE) +
  theme_bw()


# Some stats ----
library(lme4)

mod <- glmer(mass~ GPS_TDR_event * GPS_TDR_order + (1|ring_number), data=tag.events)

summary(mod)
anova(mod)
drop1(mod, test="Chi")
