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

movebank_files <- list.files("./data/Movebank raw", full.names = T)


for(file in movebank_files){
  
  #file <- movebank_files[6]
  dat <- read.csv(file)
  
  # Skip if there's no data
  if(dim(dat)[1] == 0) next()
  
  # Get a sense for how many individuals
  dat$individual.local.identifier %>% table() %>% sort()
  
  # Keep only rows with long/lat location data
  dat <- dat[complete.cases(dat[,c("location.long", "location.lat")]),]
  
  # Remove duplicate time-location-individual combos
  dat <- dat[!duplicated(dat[,c("timestamp", "location.long", "location.lat",
                    "individual.local.identifier")]),]
  
  # Change column formats ---------------------------------------
  head(dat)
  str(dat)
  
  # Time
  dat$timestamp <- as.POSIXct(dat$timestamp, tz = "UTC")
  
  local_tz <- tz_lookup_coords(lat = mean(dat$location.lat, na.rm = T),
                               lon = mean(dat$location.long, na.rm = T),
                               warn = F)
  
  dat$localtime <- format(dat$timestamp, tz = local_tz, usetz = T)
  dat$date <- as.Date(dat$localtime)
  dat$hour <- dat$localtime %>% 
    substr(12, 13) %>% 
    as.numeric()
  dat$day <- ifelse(dat$hour > 6 & dat$hour < 18,
                    T, F)
  
  
  # Sort by time for each individual
  dat <- dat %>% arrange(individual.local.identifier, timestamp)
  
  
  # Manipulate to get displacement values ---------------------------------------
  
  # Chose hypothetical frugivory events
  # We'll basically do stratified sampling to get day-time timestamps
  # for each individual
  
  diurnal <- T
  days_to_sample <- 20
  max_frug_events_per_individ <- 5
  max_days <- 2
  
  set.seed(4)
  # # Here's one way to chose random frugivory events for each individual.
  # # The downside is that it's possible for frugivory event plus the subsequent
  # # monitoring period to overlap with another one. The for loop version below
  # # corrects for that, although it's really clunky...
  # frug_events <- dat %>% as_tibble() %>%
  #   filter(day == diurnal) %>%
  #   group_by(individual.local.identifier) %>%
  #   sample_n(size = days_to_sample) %>%
  #   pull(event.id)
  
  frug_events <- c()
  for(id in unique(dat$individual.local.identifier)){ # id <- unique(dat$individual.local.identifier)[1]
    id_dat <- dat %>% filter(individual.local.identifier == id)
    
    
    fe_dat <- tibble(event.id = id_dat$event.id,
                     time = as.numeric(difftime(id_dat$timestamp, 
                                                min(id_dat$timestamp),
                                                units = "mins")),
                     event = 0)
    
    # Sample one event just to get things started
    fe_dat$event[sample(1:nrow(fe_dat), 1)] <- 1
    
    # A couple objects for the while loop
    choose_events <- T
    counter <- 1
    
    # While loop that runs while either there are potential (sort of)
    # non-independent monitoring periods or there is the max number of
    # monitoring periods per individual.
    while(choose_events == T){
      
      # Figure out how much time before or time after another frugivory
      # event
      fe_dat <- fe_dat %>%
        mutate(tmpG = cumsum(c(FALSE, as.logical(diff(event))))) %>%
        mutate(tmp_a = c(0, diff(time)) * !event,
               tmp_b = c(diff(time), 0) * !event) %>%
        group_by(tmpG) %>%
        mutate(tae = cumsum(tmp_a),
               tbe = rev(cumsum(rev(tmp_b)))) %>%
        ungroup() %>%
        dplyr::select(-c(tmp_a, tmp_b, tmpG))
      # This treats the first and last rows as there's an event there.
      # Want to avoid not choosing those event.ids due to that, so fill
      # in some big time after event and time before events there
      first_event <- which(fe_dat$event == 1)[1]
      last_event <- which(fe_dat$event == 1) %>% tail(1)
      if(first_event > 1){
        fe_dat$tae[1:(first_event-1)] <- 10^5
      }
      if(last_event < nrow(fe_dat)){
        fe_dat$tbe[(last_event+1):nrow(fe_dat)] <- 10^5
      }
      # Now actually chose the next frugivory event
      
      possible_frugivory_events <- which(fe_dat$tae > (max_days * 60 * 24) &
                                           fe_dat$tbe > (max_days * 60 * 24))
      
      # If there are potential frugivory events, sample one and go
      # to next iteration of the while loop. Will only do this until there
      # are the maximum number of events per individual. And if there aren't
      # other potential events to chose, move on to the next individual in
      # the for loop
      if(length(possible_frugivory_events) > 0){
        
        fe_dat$event[sample(possible_frugivory_events, 1)] <- 1
        
        counter <- counter + 1
        
        if(counter == max_frug_events_per_individ){
          choose_events <- F
        }
        
      } else{
        choose_events <- F
      }
      
    } # End of while loop
    
    frug_events <- c(frug_events, fe_dat$event.id[which(fe_dat$event == 1)])
    
  } # End of for loop
  
  
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
  
  # Will keep only data associated with the frugivory events and monitoring 
  # period
  dat <- dat %>% filter(!is.na(displacement))
  
  # Plot the individuals from this study
  par(mfrow = c(2,1))
  op <- par()
  par(mar = c(2.1, 4.1, 2.1, 2.1))
  plot(NA, 
       xlab = "",
       ylab = "Displacement (m)",
       xlim = c(0, (max_days * 24 * 60)),
       ylim = c(1, max(dat$displacement, na.rm = T)),
       las = 1,
       frame = F,
       main = dat$individual.taxon.canonical.name[1]
  )
  for(i in frug_events){
    dd <- dat %>% filter(frug_event_id == i)
    
    lines(dd$time_diff_min,
          dd$displacement,
          col = rgb(0,0,0, 0.3))
  }
  
  
  
  par(mar = c(5.1, 4.1, 0.1, 2.1))
  plot(NA, 
       xlab = "Time since frugivory event (minutes)",
       ylab = "Displacement (m)",
       xlim = c(0, (max_days * 24 * 60)),
       ylim = c(0.1, max(dat$displacement, na.rm = T)),
       log = "y",
       las = 1,
       frame = F
  )
  for(i in frug_events){
    dd <- dat %>% filter(frug_event_id == i)
    
    # So these can be plotted nicely on the log scale
    dd$displacement[which(dd$displacement == 0)] <- 0.1
    
    lines(dd$time_diff_min,
          dd$displacement,
          col = rgb(0,0,0, 0.3))
  }
  
  
  # Save the displacement data to csv? Probably want to reduce the number
  # of columns
  

  
}








# Plot some examples -----------------------------------------------------------

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
  print(i)
  Sys.sleep(2)#stop()#
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


