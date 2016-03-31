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
tag.events$GPS_TDR_order <- factor(tag.events$GPS_TDR_order,
                                   levels = c("+G1", "+G2", "C"))
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


tag.events$type <- "Control"
tag.events$type[tag.events$GPS_TDR_event == 1 & tag.events$GPS_TDR_order == "GPS_first"] <- "GPS & TDR"
tag.events$type[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order == "GPS_first"] <- "TDR"
tag.events$type[tag.events$GPS_TDR_event == 2 & tag.events$GPS_TDR_order == "TDR_first"] <- "GPS & TDR"
tag.events$type[tag.events$GPS_TDR_event == 1 & tag.events$GPS_TDR_order == "TDR_first"] <- "TDR"

library("reshape2")
weight.segments.df <- dcast(tag.events, formula = ring_number ~ GPS_TDR_event,
                            sum, value.var = "mass")
weight.segments.df <- merge(weight.segments.df, dcast(tag.events, formula = ring_number ~ GPS_TDR_event,
                            value.var = "date_time_rel_utc"), by = "ring_number")
names(weight.segments.df) <- c("ring_number", "mass.1", "mass.2", "mass.3",
                               "date.1", "date.2", "date.3")
weight.segments.df <- merge(weight.segments.df, tag.events[tag.events$GPS_TDR_event == 1,c(4,9,20)], by = "ring_number")
weight.segments.df <- merge(weight.segments.df, tag.events[tag.events$GPS_TDR_event == 2,c(4,20)], by = "ring_number")
names(weight.segments.df)[9] <- "Deployment_type"

weight.segments.df$Deployment_type <- as.factor(weight.segments.df$Deployment_type)
weight.segments.df$Deployment_type <- factor(weight.segments.df$Deployment_type,
                                   levels = levels(weight.segments.df$Deployment_type)[c(2,3,1)])




# Actual mass date
p <- ggplot(tag.events, aes(date_time_rel_utc, mass, fill=factor(GPS_TDR_order,labels=c("+G1","+G2","C")) , shape=GPS_TDR_order)) +
  # geom_boxplot(outlier.size=0, alpha = 0.5) +
#   geom_line(aes(date_time_rel_utc  ,mass, lty = GPS_TDR_order,
#                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(date_time_rel_utc,mass, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=TRUE) +
  geom_segment(aes(x = as.POSIXct(date.1, origin="1970-01-01", tz = "UTC"),
                   y = mass.1,
                   xend = as.POSIXct(date.2, origin="1970-01-01", tz = "UTC"),
                   yend = mass.2, linetype = Deployment_type,
                   col = GPS_TDR_order), lwd = 1.5, alpha = 0.6, data = weight.segments.df) +
  geom_segment(aes(x = as.POSIXct(date.2, origin="1970-01-01", tz = "UTC"),
                   y = mass.2,
                   xend = as.POSIXct(date.3, origin="1970-01-01", tz = "UTC"),
                   yend = mass.3, linetype  = type.y,
                   col = GPS_TDR_order), lwd = 1.5, alpha = 0.6, data = weight.segments.df) +
  ylim(790,980) +
  theme_bw()
p <- p  + labs(list(title = "Individual mass changes", x = "Date (day of June)", y =  "Mass (g)", fill="Treatment"))
p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
p <- p + scale_x_datetime(breaks = date_breaks("1 days"), labels = date_format("%d"))
# p <- p + theme(panel.grid.minor = element_line(colour = "light grey"))
plot_indivual_date <- p
p

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

# Change in mass sinse start/ day
 p <- ggplot(tag.events2, aes(GPS_TDR_event, mass_change_start/days_sinse_start, fill = GPS_TDR_order, shape=GPS_TDR_order)) +
  geom_boxplot(outlier.size=0, alpha = 0.5) +
  geom_line(aes(as.numeric(GPS_TDR_event) + adjusted - 1 , mass_change_start/days_sinse_start,
                group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
  geom_point(aes(as.numeric(GPS_TDR_event) + adjusted - 1 ,mass_change_start/days_sinse_start, colour = GPS_TDR_order),
             alpha=0.8,
             size=3,
             show_guide=FALSE) +
  theme_bw()
 p <- p  + labs(list(title = "Mass change from start", x = "Deployment event number", y =  expression(Delta~~"Mass per day (g/day)")))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 plot_mass_start <- p
 

 
 # Mass on first tagging occassion ----
 first.tags <- tag.events[tag.events$GPS_TDR_event == 1,]
 first.tags.out <- first.tags[-1,]
 # first.tags$GPS_TDR_order <- factor(first.tags$GPS_TDR_order,
                                    # levels = levels(first.tags$GPS_TDR_order)[c(3,2,1)])
 p <- ggplot(first.tags, aes(date_time_rel_utc, mass)) +
   # geom_boxplot(outlier.size=0, alpha = 0.5) +
   #   geom_line(aes(date_time_rel_utc  ,mass, lty = GPS_TDR_order,
   #                 group = ring_number, colour = GPS_TDR_order), lwd = 1.5, alpha = 0.3)+
   geom_point(aes(date_time_rel_utc,mass, colour = GPS_TDR_order, shape=GPS_TDR_order),
              alpha=0.8,
              size=3,
              show_guide=FALSE) +
   geom_smooth(method = "lm", se = FALSE, lwd = 1, col = "dark grey") +
   geom_smooth(data = first.tags.out, method = "lm", se = FALSE, lwd = 1, col = "red")
 
 p <- p + theme_bw()
 p <- p  + labs(list(title = "Mass at first capture", x = "Date", y =  "Mass (g)"))
 p <- p + scale_fill_manual(values=col_3) + scale_colour_manual(values=col_3)
 # ?geom_line
 plot_mass_first_cap <- p
 ggsave(filename = "mass_at_first_cap_01.png", width = 6, height = 4)
 
# Plots to export ------
 
#  plot_indivual_date 
#  
#  plot_indivual_date_trend <- plot_indivual_date + 
#    geom_smooth(data= first.tags, method = "lm", se = FALSE, lwd = 1, col = "dark grey") +
#    geom_smooth(data = first.tags.out, method = "lm", se = FALSE, lwd = 1, col = "red")
#  
#  draw_plot_label("A", x = 0, y = 1, hjust = -0.5, vjust = 1.5,
#                  size = 16, fontface = "bold")
#  plot_mass_deployment
#  plot_mass_start
 
 library(gridExtra)
  library(grid)
 library(cowplot)
 library(scales)
 
 # ?pdf
 pdf("weight_change3.pdf",width = 8, height = 7,  pointsize = 10)

 # win.metafile(filename = "weight_change.wmf", width = 10, height = 7)
 png(filename = "weight_change3.png",
     width = 8, height = 7, units = "in", pointsize = 10,
     bg = "white", res = 600, family = "", restoreConsole = TRUE,
     type = "cairo-png")
 
#  # Move to a new page
#  grid.newpage()
# 
#  # Create layout : nrow = 2, ncol = 2
#  pushViewport(viewport(layout = grid.layout(2, 2)))
#  
#  # A helper function to define a region on the layout
#  define_region <- function(row, col){
#    viewport(layout.pos.row = row, layout.pos.col = col)
#  } 
#  
#  # Arrange the plots
#  print(plot_indivual_date, vp=define_region(1, 1:2))
#  print((plot_mass_deployment + theme(legend.position="none")), vp = define_region(2, 1))
#  print((ggdraw(switch_axis_position((plot_mass_start+ theme(legend.position="none")), 'y'))), vp = define_region(2, 2))

 ggdraw() +
   draw_plot(plot_indivual_date, 0, .5, 1, .5) +
   draw_plot((plot_mass_deployment + theme(legend.position="none")), 0, 0, .5, .5) +
   draw_plot((ggdraw(switch_axis_position((plot_mass_start+ theme(legend.position="none")), 'y'))), .5, 0, .5, .5) +
   draw_plot_label(c("A", "B", "C"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 15)
 
  dev.off()
  
  
  

  # ?ggdraw
 
#  xlab = expression("Dives per bout (N)   "~1^st~" deployment"),
#  ylab = expression(Delta~~"Mass per day (g/day)",
#  main = "Dives per bout"
#  
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
mod <- lmer(weight_change_previous/days_sinse_previous ~ GPS_TDR_order + type + (1|ring_number), tag.events[tag.events$GPS_TDR_order != "Control" & tag.events$GPS_TDR_event != 1,])
anova(mod)
summary(mod)
drop1(mod, test = "Chisq")



# Redo stats ----
# Get data into correct format (to avoid confusion!)

# Compare mass change per day for GPS/ TDR birds only (2nd and 3rd captures)
# Paired test

# Compare mass change per day for all for second occasion
# one-way Anova

# Compare overall mass loss per day for experimentals vs. controls
# one-way Anova