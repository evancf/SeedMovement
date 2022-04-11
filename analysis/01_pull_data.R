# Testing out some stuff - note that you need to individually
# accept the license terms online before downloading

# Load packages ----------------------------------------------------------------
source(list.files("./R", full.names = T))

ipak(c("tidyverse",
       "move",
       "lutz",
       "lubridate"))

# Note that you need to log into movebank - I have a 00_movebank_login.R file
# where I have stored my username and password in this format:

# curl_login <- movebankLogin(username = "username",
#                             password = "password")


# Load data --------------------------------------------------------------------

elephant_studies <- searchMovebankStudies(x="lephant", login=curl_login)

dat <- getMovebankData(study = elephant_studies[1],
                login = curl_login)

# dat1 <- getMovebankData(study = "Forest Elephant Telemetry Programme",
#                         login = curl_login)
# I'm not sure why this isn't working - will try back later when
# 

dat <- read.csv("~/Downloads/African elephants in Etosha National Park (data from Tsalyuk et al. 2018).csv")

dim(dat)
head(dat)

# Get a sense for how many individuals
dat$individual.local.identifier %>% table() %>% sort()


# Change column formats ---------------------------------------
head(dat)
str(dat)

# Time
dat$timestamp <- as.POSIXct(dat$timestamp, tz = "UTC")

local_tz <- tz_lookup_coords(lat = mean(dat$location.lat),
                             lon = mean(dat$location.long),
                             warn = F)

dat$localtime <- format(dat$timestamp, tz = local_tz, usetz = T)
dat$date <- as.Date(dat$localtime)
dat$hour <- dat$localtime %>% 
  substr(12, 13) %>% 
  as.numeric()
dat$day <- ifelse(dat$hour > 6 & dat$hour < 18,
                  T, F)


# Lat and lon - not sure if this should be altered here or below



# Manipulate to get displacement values ---------------------------------------

# Chose hypothetical frugivory events
# We'll basically do stratified sampling to get day-time timestamps
# for each individual

diurnal <- T
days_to_sample <- 20
max_days <- 5

set.seed(4)
frug_events <- dat %>% as_tibble() %>% 
  filter(day == diurnal) %>% 
  group_by(individual.local.identifier) %>% 
  sample_n(size = days_to_sample) %>% 
  pull(event.id)

dat$frug_event_id <- NA
dat$frug_event_timestamp <- as.POSIXct(NA, tz = "UTC")
dat$frug_event_lat <- NA
dat$frug_event_long <- NA
for(i in frug_events){
  event_ind <- which(dat$event.id == i)
  id <- dat$individual.local.identifier[event_ind]
  time_start <- dat$timestamp[event_ind]
  time_end <- time_start + days(max_days)
  
  track_inds <- which(dat$individual.local.identifier == id &
                        dat$timestamp >= time_start &
                        dat$timestamp <= time_end)
  
  dat$frug_event_id[track_inds] <- i
  dat$frug_event_timestamp[track_inds] <- as.POSIXct(dat$timestamp[event_ind], tz = "UTC")
  dat$frug_event_long[track_inds] <- dat$location.long[event_ind]
  dat$frug_event_lat[track_inds] <- dat$location.lat[event_ind]
  
}

# Calculate displacement values

dat$displacement <- apply(dat[,c("location.long", "location.lat",
                                 "frug_event_long", "frug_event_lat")], 
                          1, function(x){
                            distGeo(c(x[1], x[2]), 
                                  c(x[3], x[4]))
                          })
dat$time_diff_min <- apply(dat[,c("timestamp", "frug_event_timestamp")],
                           1,
                           function(x){
                             difftime(x[1], x[2],
                                      units = "mins")
                           })


pdf("./outputs/Loxodonta africana example.pdf", 
    width = 5,
    height = 8)

# Plot this
par(mfrow = c(3,1))
op <- par()
par(mar = c(2.1, 4.1, 2.1, 2.1))
plot(NA, 
     xlab = "",
     ylab = "Displacement (km)",
     xlim = c(0, (max_days * 24 * 60) / (60 * 24)),
     ylim = c(1, 100000/1000),
     #log = "y",
     las = 1,
     frame = F,
     xaxt = "n",
     )
axis(1, at = seq(0,5), labels = rep("", 6))
for(i in frug_events){
  dd <- dat %>% filter(frug_event_id == i)
  
  lines(dd$time_diff_min / (60 * 24),
        dd$displacement/1000,
        col = rgb(0,0,0, 0.3))
}



par(mar = c(5.1, 4.1, 0.1, 2.1))
plot(NA, 
     xlab = "Time since frugivory event (days)",
     ylab = "Displacement (km)",
     xlim = c(0, (max_days * 24 * 60) / (60 * 24)),
     ylim = c(0.01, 100000/1000),
     log = "y",
     las = 1,
     frame = F,
     yaxt = "n"
)
axis(2, at = c(0.01,0.1, 1, 10, 100), 
     labels = c(0.01,0.1, 1, 10, 100), 
     las = 1)
for(i in frug_events){
  dd <- dat %>% filter(frug_event_id == i)
  
  lines(dd$time_diff_min / (60 * 24),
        dd$displacement/1000,
        col = rgb(0,0,0, 0.3))
}

# Here's what the gut passage time distribution looks like for
# Loxodonta africana

(rlnorm(10000, meanlog = log(40), sdlog = 0.2) / 24) %>% hist(main = "",
                                                              freq = F,
                                                             breaks = seq(0, 5, by = 0.25),
                                                             xlab = "Gut passage time (days)",
                                                             las = 1)

dev.off()


