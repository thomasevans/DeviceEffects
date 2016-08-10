# Dive depth model


# Read in diving data


# Read in deployment data


# Combine above two
# I.e. so can see if with GPS+TDR or TDR alone + deployment order



# Taking time of day into account - thinking of light level ----
# Using sun elevation function from GeoLight package
library("GeoLight")

# Get sun elevation angle for given date_time (POSIXct format)
fun_sun_elev <- function(t){
  require(GeoLight)
  
  # Solar time
  s <- solar(t)
  
  # Island centre
  lon <- 17.972
  lat <- 57.285
  
  # Zenith angle (angle below vertical)
  z <- zenith(s,lon,lat)
  
  # Calculate sun elevation (angle above horizon, negative if below)
  h <- -1*(z-90)
  
  return(h)
}

# Test above
  # Make a vector of times
  times <- seq(as.POSIXct("2015-06-21 00:00", tz = "UTC"), by = "hour", length.out = 24)
  
  # Calculate elevations
  elevs <- sapply(times, fun_sun_elev)  
  
  # See how this looks
  plot(elevs~times, type = "b")
  abline(v=times, lty = 2, col = "light grey")


# Label by day period
# - day >0 deg
# - civil twilight <0 & >-6
# - night <-6
# Alternatively could use nautical twilight, which is down to -12

# Function to label period of day based on sun elevation angle  
fun_day_per <- function(h){
  if(h< -6)return("NIGHT")else{
    if(h < 0)return("TWLGHT")else return("DAY")
  }
}

# Test this
sapply(elevs, fun_day_per)

# Day of june -----

