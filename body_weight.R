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


# Get Cort data
cort.events <- sqlQuery(gps.db,
                       query = "SELECT guillemots_GPS_TDR_events_cort.*
                       FROM guillemots_GPS_TDR_events_cort
                       ORDER BY guillemots_GPS_TDR_events_cort.bird_id, guillemots_GPS_TDR_events_cort.Date;",
                       as.is = TRUE)
 

# Drop record with missing data
tag.events <- tag.events[tag.events$ring_number != "AAZ988",]

# Only include cort samples in tag.events
cort.events <- cort.events[cort.events$bird_id %in% tag.events$ring_number,]


# Combine the two tables ----
cort.events <- cort.events[order(cort.events$bird_id, cort.events$Date),]
tag.events <- tag.events[order(tag.events$ring_number, tag.events$date_time_cap_utc),]

all.equal(cort.events$bird_id, tag.events$ring_number)

tag.events <- cbind.data.frame(tag.events, cort.events$CORTAssay_plate, cort.events$CORT)
names(tag.events)[16:17] <- c("cort_assay_plate", "cort")

# Do some calculations on this -----
# Weight changes (from start, and from previous occasion)
tag.events$weight_change_start <- NULL

tag.events$mass_change_start[tag.events$GPS_TDR_event == 1] <- tag.events$mass[tag.events$GPS_TDR_event == 1] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$mass_change_start[tag.events$GPS_TDR_event == 2] <- tag.events$mass[tag.events$GPS_TDR_event == 2] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$mass_change_start[tag.events$GPS_TDR_event == 3] <- tag.events$mass[tag.events$GPS_TDR_event == 3] - tag.events$mass[tag.events$GPS_TDR_event == 1 & tag.events$GPS_TDR_order != "Control"]

# Weight changes (from from previous occasion)
tag.events$weight_change_previous <- NULL

tag.events$weight_change_previous[tag.events$GPS_TDR_event == 1] <- tag.events$mass[tag.events$GPS_TDR_event == 1] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$weight_change_previous[tag.events$GPS_TDR_event == 2] <- tag.events$mass[tag.events$GPS_TDR_event == 2] - tag.events$mass[tag.events$GPS_TDR_event == 1]

tag.events$weight_change_previous[tag.events$GPS_TDR_event == 3] <- tag.events$mass[tag.events$GPS_TDR_event == 3] - tag.events$mass[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order != "Control"]




# Fix date_time thing
tag.events$date_time_rel_utc <- as.POSIXct(tag.events$date_time_rel_utc,
                                           tz = "UTC")
tag.events$date_time_cap_utc <- as.POSIXct(tag.events$date_time_cap_utc,
                                           tz = "UTC")
# Days sinse previous
tag.events$days_sinse_previous <- NULL

tag.events$days_sinse_previous[tag.events$GPS_TDR_event == 1] <- tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1] - tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1]

tag.events$days_sinse_previous[tag.events$GPS_TDR_event == 2] <- tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 2] - tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1]

tag.events$days_sinse_previous[tag.events$GPS_TDR_event == 3] <- tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 3] - tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order != "Control"]

tag.events$type_prev <- "Control"
tag.events$type_prev[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order == "GPS_first"] <- "GPS"
tag.events$type_prev[tag.events$GPS_TDR_event == 3 & tag.events$GPS_TDR_order == "GPS_first"] <- "TDR"
tag.events$type_prev[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order == "TDR_first"] <- "TDR"
tag.events$type_prev[tag.events$GPS_TDR_event == 3 & tag.events$GPS_TDR_order == "TDR_first"] <- "GPS"

tag.events$days_sinse_start <- NULL

tag.events$days_sinse_start[tag.events$GPS_TDR_event == 1] <- tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1] - tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1]

tag.events$days_sinse_start[tag.events$GPS_TDR_event == 2] <- tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 2] - tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1]

tag.events$days_sinse_start[tag.events$GPS_TDR_event == 3] <- tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 3] - tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1 & tag.events$GPS_TDR_order != "Control"]


# Colours -----
# From: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# The palette with grey:
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# col_3 <- cbPalette[c(1:3)]

# Colour scheme from http://colorbrewer2.org/, 3-class Dark2 qualitative, print and colour blind freindly
col_3 <- c("#1b9e77","#d95f02", "#7570b3")

# Some base plots ------
# boxplot(tag.events$mass_change_start~tag.events$GPS_TDR_event+tag.events$GPS_TDR_order)
# 
# boxplot(tag.events$weight_change_previous~tag.events$GPS_TDR_event+tag.events$GPS_TDR_order)


# Make some plots with ggplot -------
library("ggplot2")
library(scales)
# Get data into suitable format
str(tag.events)
tag.events$ring_number <- as.factor(tag.events$ring_number)
tag.events$GPS_TDR_order <- as.factor(tag.events$GPS_TDR_order)
tag.events$GPS_TDR_event <- as.factor(tag.events$GPS_TDR_event)

tag.events$GPS_TDR_order <- factor(tag.events$GPS_TDR_order,
                                   levels = levels(tag.events$GPS_TDR_order)[c(2,3,1)])
tag.events$GPS_TDR_order2 <- tag.events$GPS_TDR_order

levels(tag.events$GPS_TDR_order2) <-  c("+G1", "+G2", "C")
# Make an initial plot
p <- ggplot(tag.events, aes(GPS_TDR_event, mass))
p <- p + geom_boxplot()
p <- p + geom_line(aes(group = ring_number), colour = "red", alpha = 0.5)
p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)



# More fancy
tag.events$adjusted <- -0.25
tag.events$adjusted[tag.events$GPS_TDR_order == "TDR_first"] <- 0
tag.events$adjusted[tag.events$GPS_TDR_order == "Control"] <- 0.25

tag.events$adjusted[tag.events$GPS_TDR_order == "TDR_first" & tag.events$GPS_TDR_event == 3] <- 0.19
tag.events$adjusted[tag.events$GPS_TDR_order == "GPS_first" & tag.events$GPS_TDR_event == 3] <- -0.19

# Actual mass
p <- ggplot(tag.events, aes(GPS_TDR_event, mass, fill = GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event)+ adjusted  ,mass,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event)  + adjusted,mass, colour = GPS_TDR_order),
              alpha=0.6,
              size=3,
              show_guide=FALSE) +
  theme_bw()
p <- p  + labs(list(title = "Individual mass changes", x = "Deployment event number", y =  "Mass (g)"))
p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
p


# Cort
p <- ggplot(tag.events, aes(GPS_TDR_event, cort, fill = GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event)+ adjusted  ,cort,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event)  + adjusted,cort, colour = GPS_TDR_order),
             alpha=0.6,
             size=3,
             show_guide=FALSE) +
  theme_bw()
p <- p  + labs(list(title = "Individual cort changes", x = "Deployment event number", y =  "Cort"))
p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
p



tag.events$type <- "Control"
tag.events$type[tag.events$GPS_TDR_event == 1 & tag.events$GPS_TDR_order == "GPS_first"] <- "GPS & TDR"
tag.events$type[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order == "GPS_first"] <- "TDR"
tag.events$type[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order == "TDR_first"] <- "GPS & TDR"
tag.events$type[tag.events$GPS_TDR_event == 1 & tag.events$GPS_TDR_order == "TDR_first"] <- "TDR"


# Reorder tag.events
tag.events <- tag.events[,c(1:15, 18:25, 16:17)]

library("reshape2")
weight.segments.df <- dcast(tag.events, formula = ring_number ~ GPS_TDR_event,
                            sum, value.var = "mass")
weight.segments.df <- merge(weight.segments.df, dcast(tag.events, formula = ring_number ~ GPS_TDR_event,
                            value.var = "date_time_rel_utc"), by = "ring_number")
names(weight.segments.df) <- c("ring_number", "mass.1", "mass.2", "mass.3",
                               "date.1", "date.2", "date.3")
weight.segments.df <- merge(weight.segments.df, tag.events[tag.events$GPS_TDR_event == 1,c(4,20,22)], by = "ring_number")
weight.segments.df <- merge(weight.segments.df, tag.events[tag.events$GPS_TDR_event == 2,c(4,22)], by = "ring_number")
names(weight.segments.df)[9] <- "Deployment_type"

weight.segments.df$Deployment_type <- as.factor(weight.segments.df$Deployment_type)
weight.segments.df$Deployment_type <- factor(weight.segments.df$Deployment_type,
                                   levels = levels(weight.segments.df$Deployment_type)[c(2,3,1)])

names(tag.events)
# weight.segments.df$GPS_TDR_order2 <- NA
levels(tag.events$GPS_TDR_order2)
# Actual mass date
p <- ggplot(tag.events, aes(date_time_rel_utc, mass, colour = GPS_TDR_order2, shape=GPS_TDR_order2)) +
  # geom_boxplot(outlier.size=0, alpha = 0.5) +
#   geom_line(aes(date_time_rel_utc  ,mass, lty = GPS_TDR_order,
#                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(date_time_rel_utc, mass),
             alpha=0.8,
             size=3,
             show.legend =TRUE) +
  geom_segment(aes(x = as.POSIXct(date.1, origin="1970-01-01", tz = "UTC"),
                   y = mass.1,
                   xend = as.POSIXct(date.2, origin="1970-01-01", tz = "UTC"),
                   yend = mass.2, linetype = Deployment_type), lwd = 1.5, alpha = 0.6,
               data = weight.segments.df, show.legend =TRUE) +
  geom_segment(aes(x = as.POSIXct(date.2, origin="1970-01-01", tz = "UTC"),
                   y = mass.2,
                   xend = as.POSIXct(date.3, origin="1970-01-01", tz = "UTC"),
                   yend = mass.3, linetype  = type.y), lwd = 1.5, alpha = 0.6,
               data = weight.segments.df, show.legend =FALSE) +
  ylim(790,980) +
  theme_bw()
p <- p  + scale_colour_manual(values=col_3)
p <- p + scale_x_datetime(breaks = date_breaks("1 days"), labels = date_format("%d"))
# p <- p + theme(panel.grid.minor = element_line(colour = "light grey"))
p <- p  + labs(list(title = "Individual mass changes", x = "Date (day of June)", y =  "Mass (g)", shape = "Group", col = "Group", fill = "Treatment"))
# p + geom_text(aes(label = "Treatment"))
p <- p + theme(legend.key.width=unit(3,"line"))
plot_indivual_date <- p
p
# ?labs
# ?scale_x_continuous

# Change in mass sinse last event
tag.events2 <- tag.events[tag.events$GPS_TDR_event != 1,]
p <- ggplot(tag.events2, aes(GPS_TDR_event, weight_change_previous, fill = GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , weight_change_previous,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,weight_change_previous, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=FALSE) +
  theme_bw()
p <- p  + labs(list(title = "Mass change from start", x = "Deployment event number", y =  expression(Delta~~"Mass (g) from start")))
p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
p

# Change in mass sinse last event/ day
tag.events2 <- tag.events[tag.events$GPS_TDR_event != 1,]
p <- ggplot(tag.events2, aes(GPS_TDR_event, weight_change_previous/days_sinse_previous, fill = GPS_TDR_order, shape=GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , weight_change_previous/days_sinse_previous,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,weight_change_previous/days_sinse_previous, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=FALSE) +
  theme_bw()
p <- p  + labs(list(title = "Mass change during deployment", x = "Deployment event number", y =  expression(Delta~~"Mass per day (g/day)")))
p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
p
plot_mass_deployment <- p



# by treatment  # Needs some fixing!!
tag.events2 <- tag.events[tag.events$GPS_TDR_event != 1,]
p <- ggplot(tag.events2, aes(type_prev, weight_change_previous/days_sinse_previous, fill = GPS_TDR_order, shape=GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,
                weight_change_previous/days_sinse_previous,
                group = ring_number, colour = GPS_TDR_order),
            lwd = 1.5, alpha = 0.3)+
  geom_point(aes((GPS_TDR_event) ,
                 weight_change_previous/days_sinse_previous, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=FALSE) +
  theme_bw()
p <- p  + labs(list(title = "Mass change during deployment", x = "Deployment event number", y =  expression(Delta~~"Mass per day (g/day)")))
p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
p
plot_mass_deployment <- p






# Change in mass sinse start
p <- ggplot(tag.events2, aes(GPS_TDR_event, mass_change_start, fill = GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , mass_change_start,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,mass_change_start, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=FALSE) +
  theme_bw()
p <- p  + labs(list(title = "Mass change from start", x = "Deployment event number", y =  expression(Delta~~"Mass (g) from start")))
p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
p



tag.events2$mass_change_start_day <- tag.events2$mass_change_start/tag.events2$days_sinse_start

# Change in mass sinse start/ day
 p <- ggplot(tag.events2, aes(GPS_TDR_event, mass_change_start_day, fill = GPS_TDR_order, shape=GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
   theme(text =element_text(debug = FALSE, margin =margin() ))+
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , mass_change_start_day,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,mass_change_start_day, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show.legend = FALSE) +
  theme_bw()
 p <- p  + labs(list(title = "Mass change from start", x = "Deployment event number", y =  expression(Delta~~"Mass per day (g/day)")))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 plot_mass_start <- p
 # p <- p + theme(axis.title.x=element_blank(), axis.title.y=element_text(size=0))

 
 
 # Change in mass per day as percentage of body mass at start ----
 mass_start_df <- tag.events[tag.events$GPS_TDR_event == 1,c(4,7)]
 tag.events <- merge(tag.events, mass_start_df, by = "ring_number")
 names(tag.events)[7] <- "mass"
 names(tag.events)[ncol(tag.events)] <- "mass_start"
 
 # library(lme4)
 tag.events$perc_mass_loss_day_previous <- tag.events$weight_change_previous/tag.events$days_sinse_previous/tag.events$mass_start*100
 
 tag.events$perc_mass_change_start <- tag.events$mass_change_start/tag.events$mass_start*100
 
 
 tag.events2 <- tag.events[tag.events$GPS_TDR_event != 1,]
 
 # Relative to start of each deployment 
 p <- ggplot(tag.events2, aes(GPS_TDR_event, perc_mass_loss_day_previous, fill = GPS_TDR_order, shape=GPS_TDR_order)) +
   geom_boxplot(outlier.size=0, alpha = 0.5) +
   theme(text =element_text(debug = FALSE, margin =margin() ))+
   geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , perc_mass_loss_day_previous,
                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
   geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,perc_mass_loss_day_previous, colour = GPS_TDR_order),
              alpha=0.8,
              size=3,
              show.legend = FALSE) +
   theme_bw()
 p <- p  + labs(list(title = "Mass change during deployment", x = "Deployment event number", y =  expression(Delta~~"Mass (% of start mass/day)")))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 p
 plot_mass_dep_perc <- p
 # p <- p + theme(axis.title.x=element_blank(), axis.title.y=element_text(size=0))
 
 
 
 # Relative to start of each deployment 
 p <- ggplot(tag.events2, aes(GPS_TDR_event, perc_mass_loss_day_previous, fill = GPS_TDR_order, shape=GPS_TDR_order)) +
   geom_boxplot(outlier.size=0, alpha = 0.5) +
   theme(text =element_text(debug = FALSE, margin =margin() ))+
   geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , perc_mass_loss_day_previous,
                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
   geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,perc_mass_loss_day_previous, colour = GPS_TDR_order),
              alpha=0.8,
              size=3,
              show.legend = FALSE) +
   theme_bw()
 p <- p  + labs(list(title = "Mass change during deployment", x = "Deployment event number", y =  expression(Delta~~"Mass (% of start mass/day)")))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 p
 plot_mass_dep_perc <- p
 
 
 # By deployment type ---
 as.numeric(as.factor(tag.events2$type_prev))
 tag.events2$adjusted2 <- 0
 
 tag.events2$adjusted2[tag.events2$GPS_TDR_order2 == "+G1"] <- -0.19
 tag.events2$adjusted2[tag.events2$GPS_TDR_order2 == "+G2"] <- 0.19
 
 tag.events2$type_prev[tag.events2$type_prev == "TDR"] <- "-G"
 tag.events2$type_prev[tag.events2$type_prev == "GPS"] <- "+G"
 tag.events2$type_prev[tag.events2$type_prev == "Control"] <- "C"
 
 
 # Relative to start of each deployment 
 p <- ggplot(tag.events2, aes(as.factor(type_prev), perc_mass_loss_day_previous, fill = GPS_TDR_order2, shape=GPS_TDR_order2)) +
   geom_boxplot(outlier.size=0, alpha = 0.5) +
   geom_line(aes((as.numeric(as.factor(type_prev)) + adjusted2 ) , perc_mass_loss_day_previous,
                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
   geom_point(aes((as.numeric(as.factor(type_prev)) + adjusted2)  ,perc_mass_loss_day_previous, colour = GPS_TDR_order),
              alpha=0.8,
              size=3,
              show.legend = FALSE) +
   theme_bw()
 p <- p  + labs(list(title = "Mass change during deployment", x = "Deployment type", y =  expression(Delta~~"Mass (% of start mass/day)")))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 p
 plot_mass_dep_perc <- p
 
 
 
 
 
 
 
 p <- ggplot(tag.events2, aes(as.factor(GPS_TDR_event), perc_mass_change_start, fill = GPS_TDR_order2, shape=GPS_TDR_order2)) +
   geom_boxplot(outlier.size=0, alpha = 0.5) +
   geom_line(aes(((as.numeric(GPS_TDR_event) + adjusted -1) ) , perc_mass_change_start,
                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
   geom_point(aes(((as.numeric(GPS_TDR_event)  + adjusted + -1) )  ,perc_mass_change_start, colour = GPS_TDR_order),
              alpha=0.8,
              size=3,
              show.legend = FALSE) +
   theme_bw()
 p <- p  + labs(list(title = "Mass change from 1st capture", x = "Capture event", y =  expression(Delta~~"Mass from start (%)")))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 p
 plot_mass_perc_original <- p
 
 
 
 
 
 
 
 
 
 
 # Relative to start
 p <- ggplot(tag.events2, aes(GPS_TDR_event, perc_mass_loss_day_start, fill = GPS_TDR_order, shape=GPS_TDR_order)) +
   geom_boxplot(outlier.size=0, alpha = 0.5) +
   theme(text =element_text(debug = FALSE, margin =margin() ))+
   geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , perc_mass_loss_day_start,
                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
   geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,perc_mass_loss_day_start, colour = GPS_TDR_order),
              alpha=0.8,
              size=3,
              show.legend = FALSE) +
   theme_bw()
 p <- p  + labs(list(title = "Mass change from first capture", x = "Deployment event number", y =  expression(Delta~~"Mass (% of start mass/day)")))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 p
 plot_mass_start_perc <- p
 
 
 # Mass on first tagging occassion ----
 first.tags <- tag.events[tag.events$GPS_TDR_event == 1,]
 # first.tags$GPS_TDR_order <- factor(first.tags$GPS_TDR_order,
                                    # levels = levels(first.tags$GPS_TDR_order)[c(3,2,1)])
 
 first.tags$days_june1 <- as.numeric(difftime(first.tags$date_time_rel_utc, as.POSIXct("2015-06-01 00:01", tz = "UTC")))
 first.tags.out <- first.tags[-1,]
 
 
 p <- ggplot(first.tags, aes(days_june1, mass)) +
   # geom_boxplot(outlier.size=0, alpha = 0.5) +
   #   geom_line(aes(date_time_rel_utc  ,mass, lty = GPS_TDR_order,
   #                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
   geom_point(aes(days_june1,mass, colour = GPS_TDR_order2, shape=GPS_TDR_order2),
              alpha=0.8,
              size=3,
              show.legend = TRUE) +
   geom_smooth(method = "lm", se = FALSE, lwd = 1, col = "dark grey") +
   geom_smooth(data = first.tags.out, method = "lm", se = FALSE, lwd = 1, col = "red")
 
 p <- p + theme_bw()
 p <- p  + labs(list(title = "Mass at first capture", x = "Days since 1st June", y =  "Mass (g)", shape = "Group", col = "Group"))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 p <- p + scale_x_continuous(breaks = 1:100)
 # p <- p + labs(list(title = "Individual mass changes", x = "Date (day of June)", y =  "Mass (g)", shape = "Group", col = "Group", fill = "Treatment"))
 # ?geom_line
 plot_mass_first_cap <- p
 p
 
 ggsave(filename = "mass_at_first_cap_01.png", width = 6, height = 4)
 
# Plots to export ------
 

 library(gridExtra)
  library(grid)
 library(cowplot)
 library(scales)
 
 # ?pdf
 pdf("weight_change6.pdf",width = 8, height = 7,  pointsize = 10)

 # win.metafile(filename = "weight_change.wmf", width = 10, height = 7)
 png(filename = "weight_change7.png",
     width = 8, height = 7, units = "in", pointsize = 10,
     bg = "white", res = 600, family = "", restoreConsole = TRUE,
     type = "cairo-png")
 
 ggdraw() +
   draw_plot(plot_indivual_date, 0, .5, 1, .5) +
   draw_plot((plot_mass_perc_original + theme(legend.position="none")), 0, 0, .5, .5) +
   draw_plot((plot_mass_dep_perc + theme(legend.position="none")), .5, 0, .5, .5) +
   draw_plot_label(c("A", "B", "C"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 15)
 
 
  dev.off()
  
  
  
 
# Some stats ----
library(lme4)

mod <- glmer(mass~ GPS_TDR_event * GPS_TDR_order + (1|ring_number), data=tag.events)

summary(mod)
anova(mod)
drop1(mod, test="Chi")
# levels(tag.events$GPS_TDR_order)[c(3,2,1)]
tag.events$GPS_TDR_order <- factor(tag.events$GPS_TDR_order,
       levels = levels(tag.events$GPS_TDR_order)[c(3,2,1)])

library(nlme)
# Does weight change differ between 3 groups at second capture (paired test)?
summary(lme(mass ~ GPS_TDR_order, data = tag.events[tag.events$GPS_TDR_event != 3,], random = ~ 1 | ring_number))
# NS

# Does weight change differ between GPS or TDR and device order 
summary(lme(mass ~ GPS_TDR_order + type, data = tag.events[tag.events$GPS_TDR_order != "Control",], random = ~ 1 | ring_number))
# 

# Does weight change differ between GPS+TDR and TDR only deployments, and does the order matter?
summary(lme(mass_change_start/days_sinse_start ~ GPS_TDR_order + type, data = tag.events[tag.events$GPS_TDR_order != "Control" & tag.events$GPS_TDR_event != 1,], random = ~ 1 | ring_number))
# Yes, 

summary(aov(weight_change_previous/days_sinse_previous~GPS_TDR_order,tag.events[tag.events$GPS_TDR_event == 2,]))

summary(lme(weight_change_previous/days_sinse_previous ~ GPS_TDR_order + type, data = tag.events[tag.events$GPS_TDR_order != "Control" & tag.events$GPS_TDR_event != 1,], random = ~ 1 | ring_number))

library(lme4)
mod <- lmer(weight_change_previous/days_sinse_previous ~ GPS_TDR_order + type_prev + (1|ring_number), tag.events[tag.events$GPS_TDR_order != "Control" & tag.events$GPS_TDR_event != 1,])
anova(mod)
summary(mod)
drop1(mod, test = "Chisq")



# Redo stats ----
# Compare control birds weight loss to zero (one-sample t-test) ----
weight.change <- tag.events$weight_change_previous/tag.events$days_sinse_previous
weight.change.controls <- weight.change[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order2 == "C"]
t.test(weight.change.controls)
# NS
# data:  weight.change.controls
# t = -0.51831, df = 4, p-value = 0.6316
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -9.586995  6.570659
# sample estimates:
#   mean of x 
# -1.508168 



# Does mass decline at first capture accross period? -----
mass.1 <- tag.events$mass[tag.events$GPS_TDR_event == 1]
group <- tag.events$GPS_TDR_order2[tag.events$GPS_TDR_event == 1]
date_tagged <- tag.events$date_time_rel_utc[tag.events$GPS_TDR_event == 1]

days.1june <- difftime(date_tagged, as.POSIXct("2015-06-01 00:00", tz = "UTC"))

mod01 <- lm(mass.1~days.1june)
summary(mod01)

# Without outlier point
mod02 <- lm(mass.1[-1]~days.1june[-1])
summary(mod02)



mass_start_df <- tag.events[tag.events$GPS_TDR_event == 1,c(4,7)]
tag.events <- merge(tag.events, mass_start_df, by = "ring_number")
names(tag.events)[7] <- "mass"
names(tag.events)[ncol(tag.events)] <- "mass_start"

library(lme4)
tag.events$perc_mass_loss_day_previous <- tag.events$weight_change_previous/tag.events$days_sinse_previous/tag.events$mass_start*100

mod <- lmer(perc_mass_loss_day_previous ~ GPS_TDR_order + type_prev + (1|ring_number), tag.events[tag.events$GPS_TDR_order != "Control" & tag.events$GPS_TDR_event != 1,])
anova(mod)
summary(mod)
drop1(mod, test = "Chisq")





# Compare mass loss per day between with and without GPS ----




# Get data into correct format (to avoid confusion!)




# Compare mass change per day for GPS/ TDR birds only (2nd and 3rd captures)
# Paired test

# Compare mass change per day for all for second occasion
# one-way Anova

# Compare overall mass loss per day for experimentals vs. controls
# one-way Anova