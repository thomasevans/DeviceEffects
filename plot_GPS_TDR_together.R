

# Get GPS data -----
gps.points <- 


# Get TDR data (the raw tag data) -----
tdr_raw <- 

# Get start and end date for deployments ------

start_date_time
end_date_time


# Make multi-plot figure -----

# Set up 5 row figure
par(mfrow = c(5, 1))

# Set margins around plot
par(mar=c(0,4,1,2))   


# Plot GPS data

# Distance from colony
plot(gps.points$coldist/1000~
       gps.points$date_time,
     ylab = "Displacement (km)",
     xlab = "",
     xaxt = "n",
     xlim = c(start_date_time, end_date_time),
     type = "b")


# speed~time
plot(gps.points$speed_ms~
       gps.points$date_time,
     ylab = "Speed (ms-1)",
     xlab = "",
     xaxt = "n",
     xlim = c(start_date_time, end_date_time),
     type = "b")



# Plot TDR data

# Temperature (C)
plot(tdr_raw$temp_C~
       tdr_raw$date_time,
     ylab = "Temp (C)",
     xlab = "",
     xaxt = "n",
     xlim = c(start_date_time, end_date_time),
     cex = 0.5)

# Wet/dry
plot(tdr_raw$wet_dry~
       tdr_raw$date_time,
     ylab = "Wet/dry",
     xlab = "",
     xaxt = "n",
     xlim = c(start_date_time, end_date_time),
     type = "l")

# Change plot margins for final plot (so there's space for an x-axis)
par(mar=c(2,4,1,2))   

# Pressure (dB)
plot(tdr_raw$pressure_dBars_base~
       tdr_raw$date_time,
     ylab = "depth",
     xlab = "",
     xaxt = "n",
     ylim = c(80,0),
     xlim = c(start_date_time, end_date_time),
     type = "l")
axis.POSIXct(1, at=seq(start_date_time, end_date_time, by="hour"), labels= FALSE)
axis.POSIXct(1, at=seq(start_date_time, end_date_time, by="3 hour"), format="%H")
