# Testing out some stuff - note that you need to individually
# accept the license terms online before downloading

# Load packages ----------------------------------------------------------------
sapply(list.files("./R", full.names = T), source)

ipak(c("tidyverse",
       "move",
       "lutz",
       "lubridate",
       "data.table",
       "taxize",
       "traitdata",
       "RCurl"))

# Note that you need to log into movebank - I have a 00_movebank_login.R file
# where I have stored my username and password in this format:

# curl_login <- movebankLogin(username = "username",
#                             password = "password")



# Get some info about sensors from the API
opts <- curlOptions(userpwd = "evanfricke:Hello_567vbv") 

sensor_ids <- getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=tag_type",
                     .opts = opts)
sensor_ids <- read.csv(text = sensor_ids)

sensors_to_use <- sensor_ids %>% 
  filter(is_location_sensor == "true")

short_term_sensors <- c(653,673,2365682,1239574236)
long_term_sensors <- c(397,82798,3886361)


# Load data --------------------------------------------------------------------

movebank_files <- list.files("./data/Movebank raw", full.names = T)
movebank_files_short <- list.files("./data/Movebank raw", full.names = F)
manipulated_files <- list.files("./data/tidy/Movebank manipulated", full.names = T)
manipulated_files_short <- list.files("./data/tidy/Movebank manipulated", full.names = F)

movebank_files <- movebank_files[!(movebank_files_short %in% manipulated_files_short)]


hyphen_to_dot <- function(x){
  gsub("-", ".", x, fixed = T)
}
underscore_to_dot <- function(x){
  gsub("_", ".", x, fixed = T)
}



problems <- c()

for(file in movebank_files){
  
  #file <- movebank_files[6]
  dat <- fread(file) %>% as.data.frame()
  
  if(dim(dat)[1] < 2) next()
  
  # Column manipulation because of how data were initially ingested...
  colnames(dat) <- colnames(dat) %>% hyphen_to_dot() %>% underscore_to_dot()
  
  # if("sensor.type.id" %in% colnames(id_dat)){
  #   if(id_dat$sensor.type.id[1] == "3886361"){
  #     old_max_days <- max_days
  #     max_days <- 4
  #   }
  # }
  # 
  # if("sensor.type" %in% colnames(id_dat)){
  #   if(grepl("geolocator",id_dat$sensor.type[1])){
  #     old_max_days <- max_days
  #     max_days <- 4
  #   }
  # }
  # 
  # # Reset max days if it's a geolocator study.
  # if(!is.na(old_max_days)){
  #   max_days <- old_max_days
  # }
  
  
  # It's easier to treat event id as numeric rather than int64
  dat$event.id <- as.numeric(dat$event.id)
  
  # Skip if there's no data
  if(dim(dat)[1] == 0) next()
  
  # Get a sense for how many individuals
  dat$individual.local.identifier %>% table() %>% sort()
  
  # Keep only rows with long/lat location data
  dat <- dat[complete.cases(dat[,c("location.long", "location.lat")]),] %>% 
    filter(location.lat < 90 & location.lat > -90) %>% 
    filter(location.long < 360 & location.long > -360) %>% 
    filter(!grepl("calibration", individual.local.identifier))
  
  # # Remove duplicate time-location-individual combos
  # dat <- dat[!duplicated(dat[,c("timestamp", "location.long", "location.lat",
  #                   "individual.local.identifier")]),]
  
  
  
  
  # Some files we just want to skip
  
  if(file %in% c("./data/Movebank raw/Feral cats and fenced reserves.csv",
                 "./data/Movebank raw/GPS calibration data (global).csv",
                 "./data/Movebank raw/Homing Pigeons-Magnetic sense2.csv",
                 "./data/Movebank raw/ICARUS Bass-Rock Gannets.csv",
                 "./data/Movebank raw/ICARUS Bhutan Nawang Norbu.csv",
                 "./data/Movebank raw/ICARUS Canada 2021.csv",
                 "./data/Movebank raw/ICARUS Cuckoo South Korea.csv",
                 "./data/Movebank raw/ICARUS Latvia.csv",
                 "./data/Movebank raw/ICARUS marine Oregon.csv",
                 "./data/Movebank raw/ICARUS Ocean Drifter.csv",
                 "./data/Movebank raw/ICARUS Pinyon Jay.csv",
                 "./data/Movebank raw/Lesser Kestrel Rehab Ramat Hanadiv.csv",
                 "./data/Movebank raw/ICARUS South Africa 2021.csv",
                 "./data/Movebank raw/ICARUS South Africa Ear tags.csv",
                 "./data/Movebank raw/Larus canus.csv",
                 "./data/Movebank raw/Peregrine Falcon Hungary - Comparing bird and human soaring strategies.csv", # Uncertain if falcon or person gps data
                 "./data/Movebank raw/Local flight paths of nocturnally migrating birds.csv", # No species specific data
                 "./data/Movebank raw/MAP-PUBLIC-PART-RESTR.csv", # Not enough metadata to interpret
                 "./data/Movebank raw/MAP-PUBLIC.csv", # Same as above
                 "./data/Movebank raw/milsar-feed-test-1.csv",
                 "./data/Movebank raw/Movement Workshop Eagle Practice Data.csv", # No permission
                 "./data/Movebank raw/Movement Ecology of Campo Miners Geositta poeciloptera.csv",
                 "./data/Movebank raw/Pintail in Olonne Marsh (France).csv",
                 "./data/Movebank raw/Plastic bottle tracking .csv",
                 "./data/Movebank raw/Vulture_CTT_CapeMay.csv")){
    # Apparently no deployed data here...
    next()
  }
  
  if(grepl("Bewick's Swan        Eileen Rees.csv", file, fixed = T)){
    next()
  }
  
  if(grepl("Blue_crane_Overberg_South_Africa", file, fixed = T)){
    next()
  }
  
  if(grepl("Central Valley shorebird movements and wetland connectivity", file, fixed = T)){
    # Apparently no deployed data here...
    next()
  }
  
  if(grepl("Estimating encounter location distributions from animal tracking data", file, fixed = T)){
    # Apparently no deployed data here...
    next()
  }
  
  if(grepl("DLC Pilot ACC, Lemur catta", file, fixed = T)){
    # Not sure what's happening here...
    next()
  }
  
  if(grepl("Druidtest_sufan", file, fixed = T)){
    # Not sure what's happening here...
    next()
  }
  
  
  

  

  
  # Do a little data cleaning (more issues will certainly come up)
  
  if(grepl("Variability of White Stork flight", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Ciconia ciconia"
  }
  
  if(grepl("St. Louis Box Turtle Project", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Terrapene mexicana"
  }
  
  if(grepl("Pernis_apivorus_Byholm", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Pernis apivorus"
  }
  
  if(grepl("Ovis aries di Virgilio Patagonia", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Ovis aries"
  }
  
  if(grepl("ortolan light logger tracking", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Emberiza hortulana"
  }
  
  if(grepl("Movement data from a Malayan krait", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Bungarus candidus"
  }
  
  if(grepl("Monitoring of Capra ibex (Bovidae) populations", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Capra ibex"
  }
  
  if(grepl("Mangrove Cuckoo (Coccyzus minor) home range and seasonal movements", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Coccyzus minor"
  }
  
  if(grepl("Green python Morelia viridis", file)){
    dat$individual.taxon.canonical.name <- "Morelia viridis"
  }
  
  if(grepl("Galapagos Tortoise Hatchling Study", file)){
    dat$individual.taxon.canonical.name <- "Chelonoidis niger"
  }

  if(grepl("Eastern Box Turtle Tracking", file)){
    dat$individual.taxon.canonical.name <- "Terrapene carolina"
  }
  
  if(grepl("Caspian Terns, Columbia Plateau, WA, 2016", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Hydroprogne caspia"
  }
  
  if(grepl("Calloselasma.rhodostoma_JG.Hilll_Sakaerat", file)){
    dat$individual.taxon.canonical.name <- "Calloselasma rhodostoma"
  }
  
  if(grepl("Canada geese (Branta canadensis).csv", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Branta canadensis"
  }
  
  if(grepl("Blue whales Eastern North Pacific 2003 State-space model output", file)){
    dat$individual.taxon.canonical.name <- "Balaenoptera musculus"
  }
  
  if(grepl("./data/Movebank raw/Foraging ecology of masked boobies (Sula dactylatra) in the world’s largest “oceanic desert”.csv", file)){
    dat$individual.taxon.canonical.name <- "Sula dactylatra"
  }
  
  if(grepl("./data/Movebank raw/ECOPATH, Indian yellow-nosed albatross, Boulinier et al., Amsterdam Island.csv", file)){
    dat$individual.taxon.canonical.name <- "Thalassarche carteri"
  }
  
  if(grepl("./data/Movebank raw/ECOPATH, Brown skua, Boulinier et al., Amsterdam Island.csv", file)){
    dat$individual.taxon.canonical.name <- "Stercorarius antarcticus"
  }
  
  if(grepl("./data/Movebank raw/Biology of birds practical.csv", file)){
    dat$individual.taxon.canonical.name <- "Anser albifrons"
  }
  
  if(grepl("./data/Movebank raw/BfRw Petrogale lateralis.csv", file)){
    dat$individual.taxon.canonical.name <- "Petrogale lateralis"
  }

  if(grepl("Andean Condor Vultur gryphus Bariloche, Argentina, 2013-2018", file)){
    dat$individual.taxon.canonical.name <- "Vultur gryphus"
  }
  
  if(grepl("BCI Agouti GPS test", file)){
    dat$individual.taxon.canonical.name <- "Dasyprocta punctata"
  }
  
  
  # Older ones that were rechecked
  
  if(grepl("North America and the Atlantic Ocean, Setophaga striata", file)){
    dat$individual.taxon.canonical.name <- "Setophaga striata"
  }
  
  if(grepl("Crax globulosa", file)){
    dat$individual.taxon.canonical.name <- "Crax globulosa"
  }
  
  if(grepl("Foraging ecology of masked boobies", file)){
    dat$individual.taxon.canonical.name <- "Sula dactylatra"
  }
  
  if(grepl("Lagostrophus fasciatus", file)){
    dat$individual.taxon.canonical.name <- "Lagostrophus fasciatus"
  }
  
  if(grepl("Lowland tapirs, Tapirus terrestris, in", file)){
    dat$individual.taxon.canonical.name <- "Tapirus terrestris"
  }  
  
  if(grepl("Milvus migrans", file)){
    dat$individual.taxon.canonical.name <- "Milvus migrans"
  }
  
  if(grepl("Wildebeest (Eastern white bearded) Mo", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Connochaetes taurinus"
  }
  
  
  # Need to check these below. Will determine if there are problems here
  
  # Find cases where the species name is not recorded
  if(dat$individual.taxon.canonical.name %>% unique() %>% is.na() %>% all()){
    problems <- c(problems, paste("no species name for", file))
    next()
  } 
  if(all(unique(dat$individual.taxon.canonical.name) == "")){
    problems <- c(problems, paste("no species name for", file))
    next()
  } 
  
  


  if(grepl("Blackpoll Warbler", file)){
    dat$individual.taxon.canonical.name <- "Setophaga striata"
  }

  if(grepl("Coyote Valley Bobcat Habitat Connectivity Study", file)){
    dat$individual.taxon.canonical.name <- "Lynx rufus"
  }
  
  if(grepl("Cassin's Vireo", file)){
    dat$individual.taxon.canonical.name <- "Vireo cassinii"
  }

  if(grepl("Monitoring of Capra ibex (Bovidae) populations", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Capra ibex"
  }

  # This one doesn't have species-level data
  if(grepl("Local flight paths of nocturnally migrating birds", file)){
    next()
  }
  
  
  # These are apparently deployed data where there isn't an individual local
  # identifier
  if(file %in% c("./data/Movebank raw/Bechstein's bat Bats The Netherlands and Belgium.csv",
                 "./data/Movebank raw/Griffon vulture [fdlmes.gr].csv")){
    dat$individual.local.identifier <- "a"
  }

  

  dat <- dat %>% filter(individual.taxon.canonical.name != "")
  dat <- dat %>% filter(individual.local.identifier != "")
  
  
  
  
  # Here are ones where there's some individual tags that may not be on
  # individuals of an animal species...
  
  
  if(grepl("Sperm whale CRC NW Atlantic", file, fixed = T)){
    dat$individual.taxon.canonical.name <- "Physeter macrocephalus"
  }
  
  # Change column formats ---------------------------------------
  #head(dat)
  #str(dat)
  
  # Time
  dat$timestamp <- as.POSIXct(dat$timestamp, tz = "UTC")
  
  dat <- dat %>% filter(location.lat > -180)
  
  local_tz <- tz_lookup_coords(lat = mean(dat$location.lat, na.rm = T),
                               lon = mean(dat$location.long, na.rm = T),
                               warn = F)
  
  dat$localtime <- format(dat$timestamp, tz = local_tz, usetz = T,
                          format = "%Y-%m-%d %H:%M:%S")
  #dat$date <- as.Date(dat$localtime)
  hour <- dat$localtime %>% 
    substr(12, 13) %>% 
    as.numeric()
  dat$day <- ifelse(hour > 6 & hour < 18,
                    T, F)
  
  
  # Sort by time for each individual
  dat <- dat %>% arrange(individual.local.identifier, timestamp)
  

  
  # Manipulate to get displacement values ---------------------------------------
  
  # Chose hypothetical frugivory events
  # We'll basically do stratified sampling to get day-time timestamps
  # for each individual
  
  focal_sp <- focal_sp_class <- nocturnal <- diurnal <- body_mass <- max_days <- time_res_goal <- NA
  
  # Want to change the sampling time based on the taxon identity
  focal_sp <- unique(dat$individual.taxon.canonical.name) %>% sort()
  focal_sp_class <- tax_name(sci = focal_sp, get = "class", db = "ncbi")$class
  
  # There is at least one issue where the species name isn't in the database
  focal_sp <- ifelse(focal_sp == "Tyto furcata", "Tyto alba", focal_sp)
  focal_sp <- ifelse(focal_sp == "Myotis bechsteini", "Myotis bechsteinii", focal_sp)
  focal_sp <- ifelse(focal_sp == "Myotis daubentoni", "Myotis daubentonii", focal_sp)
  focal_sp <- ifelse(focal_sp == "Neophron percnopterus", "Neophron perenopterus", focal_sp)
  focal_sp <- ifelse(focal_sp == "Onychoprion fuscatus", "Sterna fuscata", focal_sp)
  focal_sp <- ifelse(focal_sp == "Ichthyaetus melanocephalus", "Lixus melanocephalus", focal_sp)
  focal_sp <- ifelse(focal_sp == "Phoenicopterus minor", "Phoeniconaias minor", focal_sp)
  focal_sp <- ifelse(focal_sp == "Antrostomus vociferus", "Caprimulgus vociferus", focal_sp)
  focal_sp <- ifelse(focal_sp == "Physeter macrocephalus", "Physeter katadon", focal_sp)
  focal_sp <- ifelse(focal_sp == "Larus vegae", "Larus argentatus", focal_sp)
  focal_sp <- ifelse(focal_sp == "Gelochelidon nilotica", "Sterna nilotica", focal_sp)
  
  
  # Will skip the 
  if(!focal_sp_class %in% c("Aves", "Mammalia")) next()
  
  if(focal_sp_class == "Aves"){
    nocturnal <- elton_birds %>% filter(scientificNameStd %in% focal_sp) %>% pull(Nocturnal)
    diurnal <- nocturnal == 0
    
    body_mass <- elton_birds %>% filter(scientificNameStd %in% focal_sp) %>% pull(BodyMass.Value)
    max_days <- ceiling(exp(-5.64120 + 0.54627 * log(body_mass)) * 2) * 24.5/24
  }
  if(focal_sp_class == "Mammalia"){
    diurnal <- elton_mammals %>% filter(scientificNameStd %in% focal_sp) %>% pull(Activity.Diurnal)
    diurnal <- diurnal == 1

    body_mass <- elton_mammals %>% filter(scientificNameStd %in% focal_sp) %>% pull(BodyMass.Value)
    max_days <- ceiling(exp(-5.64120 + 0.54627 * log(body_mass)) * 2) * 24.5/24 # This allows us to capture events when there's exactly (or very close to) 1 day between events
    max_days <- ifelse(max_days > 10, 10, max_days)
  }
  
  # elton_mammals %>% filter(word(focal_sp, 1) == word(elton_mammals$scientificNameStd, 1))
  # "Myotis bechsteinii" %in% elton_mammals$scientificNameStd
  # focal_sp
  # 
  # elton_birds %>%
  #   filter(word(focal_sp, 1) == word(elton_birds$scientificNameStd, 1)) %>%
  #   arrange(English)
  # "Sterna fuscata" %in% elton_birds$scientificNameStd
  # "Pernis apivorus" %in% elton_birds$scientificNameStd
  # "Larus argentatus" %in% elton_birds$scientificNameStd
  # 
  # "Gull" %in% word(elton_birds$English, 2)
  # filter(elton_birds, English == "Gull-billed Tern")
  # focal_sp
  # 
  # 
  # "Ovis aries" %in% elton_mammals$scientificNameStd
  # "Physeter" %in% word(elton_mammals$scientificNameStd,1)
  # elton_mammals %>% filter(word(scientificNameStd, 1) == "Physeter")
  
  max_frug_events_per_individ <- 5
  
  
  # First, want to reduce temporal resolution if < X minutes by iteratively
  # removing timesteps that are less than X minutes from the next. There must be
  # a more clever way to do this, but here's a while-loop way.
  
  # But what should this X be? Let's reduce so that it's a max of about 300 points, which
  # works out to about 5 minutes between events for a 1 day max_days value
  
  time_res_goal <- (max_days * 1440 / 300)[1]
  
  if(!all(time_res_goal == time_res_goal[1])) stop()
  
  temp_res_check <- F
  while(temp_res_check == F){
    
    dat$keep_vec <- dat %>%
      group_by(individual.local.identifier) %>%
      mutate(diff = timestamp - lag(timestamp),
             diff_mins = as.numeric(diff, units = 'mins'),
             odd = row_number() %% 2) %>% 
      mutate(keep = ifelse(diff_mins < time_res_goal & odd == 1, 0, 1)) %>% 
      mutate(keep = ifelse(is.na(keep), 1, keep)) %>% 
      pull(keep)
    
    temp_res_check <- all(dat$keep_vec == 1)
    
    dat <- dat[dat$keep_vec == 1,]
    # print(dim(dat))
    # print(temp_res_check)
    
    
    #asdf <- dat %>% filter(individual.local.identifier == levels(as.factor(individual.local.identifier))[1])
    #hist(asdf$location.long)
    
    #dat %>% dplyr::select(timestamp, keep_vec)
    
    
  }
  
  
  set.seed(4)
  # # Here's one way to chose random frugivory events for each individual.
  # # The downside is that it's possible for frugivory event plus the subsequent
  # # monitoring period to overlap with another one. The for loop version below
  # # corrects for that, although it's really clunky...
  # days_to_sample <- 20
  # frug_events <- dat %>% as_tibble() %>%
  #   filter(day == diurnal) %>%
  #   group_by(individual.local.identifier) %>%
  #   sample_n(size = days_to_sample) %>%
  #   pull(event.id)
  
  frug_events <- c()
  for(sp in 1:length(focal_sp)){
    for(id in unique(dat$individual.local.identifier)){
      id_dat <- dat %>% filter(individual.local.identifier == id & 
                                 individual.taxon.canonical.name == focal_sp[sp] &
                                 day == diurnal[sp]) 
      # This last line ensures that the frugivory event is only during the
      # active period for the species.
      
      if(dim(id_dat)[1] == 0) {next()}
      
      
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
        
        possible_frugivory_events <- which(fe_dat$tae > (max_days[sp] * 60 * 24) &
                                             fe_dat$tbe > (max_days[sp] * 60 * 24))
        
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
      
    } # End of individual for loop
  }  # End of species for loop
  
  if(length(frug_events) == 0) { next()}
  
  dat$frug_event_id <- NA
  dat$frug_event_timestamp <- as.POSIXct(NA, tz = "UTC")
  dat$frug_event_lat <- NA
  dat$frug_event_long <- NA
  for(i in frug_events){
    event_ind <- which(dat$event.id == i)
    id <- dat$individual.local.identifier[event_ind]
    time_start <- dat$timestamp[event_ind]
    time_end <- time_start + minutes(round(max_days * 1440))
    
    track_inds <- which(dat$individual.local.identifier == id &
                          dat$timestamp >= time_start &
                          dat$timestamp <= time_end)
    
    # Check if the animal was recorded as dead during this track.
    if("mortality.status" %in% colnames(dat)){
      # Skip if it's recorded as dead
      if("dead" %in% dat$mortality.status[track_inds]){ next()}
    }
    
    
    dat$frug_event_id[track_inds] <- i
    dat$frug_event_timestamp[track_inds] <- as.POSIXct(dat$timestamp[event_ind], tz = "UTC")
    dat$frug_event_long[track_inds] <- dat$location.long[event_ind]
    dat$frug_event_lat[track_inds] <- dat$location.lat[event_ind]
    
  }
  
  # Will keep only data associated with the frugivory events and monitoring
  # period
  dat <- dat %>% filter(!is.na(frug_event_id))
  
  
  # Calculate displacement values and difference in time values
  dat$displacement <- apply(dat[,c("location.long", "location.lat",
                                   "frug_event_long", "frug_event_lat")], 
                            1, 
                            function(x){
                              distGeo(c(x[1], x[2]), 
                                      c(x[3], x[4]))
                            })
  dat$time_diff_min <- apply(dat[,c("timestamp", "frug_event_timestamp")],
                             1,
                             function(x){
                               difftime(x[1], x[2],
                                        units = "mins")
                             })
  


  # # Plot the individuals from this study
  # par(mfrow = c(2,1))
  # op <- par()
  # par(mar = c(2.1, 4.1, 2.1, 2.1))
  # plot(NA,
  #      xlab = "",
  #      ylab = "Displacement (m)",
  #      xlim = c(0, (max_days * 24 * 60)),
  #      ylim = c(1, max(dat$displacement, na.rm = T)),
  #      las = 1,
  #      frame = F,
  #      main = dat$individual.taxon.canonical.name[1]
  # )
  # for(i in frug_events){
  #   dd <- dat %>% filter(frug_event_id == i)
  # 
  #   lines(dd$time_diff_min,
  #         dd$displacement,
  #         col = rgb(0,0,0, 0.3))
  # }
  # 
  # 
  # 
  # par(mar = c(5.1, 4.1, 0.1, 2.1))
  # plot(NA,
  #      xlab = "Time since frugivory event (minutes)",
  #      ylab = "Displacement (m)",
  #      xlim = c(0, (max_days * 24 * 60)),
  #      ylim = c(0.1, max(dat$displacement, na.rm = T)),
  #      log = "y",
  #      las = 1,
  #      frame = F
  # )
  # for(i in frug_events){
  #   dd <- dat %>% filter(frug_event_id == i)
  # 
  #   # So these can be plotted nicely on the log scale
  #   dd$displacement[which(dd$displacement == 0)] <- 0.1
  # 
  #   lines(dd$time_diff_min,
  #         dd$displacement,
  #         col = rgb(0,0,0, 0.3))
  # }
  
  if(dim(dat)[1] == 0) next()

  
  # Save the displacement data to csv? Probably want to reduce the number
  # of columns
  
  if("study.name" %in% colnames(dat)){
  dat <- dat %>% 
    dplyr::select("event.id", "timestamp", "localtime", 
                  "location.long", "location.lat",
                  "individual.taxon.canonical.name", 
                  "individual.local.identifier",
                  "sensor.type",
                  "frug_event_id", 
                  "displacement", "time_diff_min", 
                  "study.name")
  }
  if("study.id" %in% colnames(dat)){
    dat <- dat %>% 
      dplyr::select("event.id", "timestamp", "localtime", 
                    "location.long", "location.lat",
                    "individual.taxon.canonical.name", 
                    "individual.local.identifier",
                    "sensor.type.id",
                    "frug_event_id", 
                    "displacement", "time_diff_min", 
                    "study.id")
  }
  
  
  output_file <- gsub("./data/Movebank raw/", "", file, fixed = T)
  write.csv(dat, file = paste0("./data/tidy/Movebank manipulated/", output_file))
  
  print(file)
}





# # Dealing with taxonomy and traits ---------------------------------------------
# 
# ipak("bdc")
# library("taxadb")
# #ipak("traitdata")
# remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)
# 
# move_spp <- unique(move_df$individual.taxon.canonical.name)
# 
# query_names <- bdc_query_names_taxadb(
#   sci_name            = move_spp,
#   replace_synonyms    = TRUE, # replace synonyms by accepted names?
#   suggest_names       = TRUE, # try to found a candidate name for misspelled names?
#   suggestion_distance = 0.9, # distance between the searched and suggested names
#   db                  = "gbif", # taxonomic database
#   rank_name           = "Animalia", # a taxonomic rank
#   rank                = "kingdom", # name of the taxonomic rank
#   parallel            = FALSE, # should parallel processing be used?
#   ncores              = 2, # number of cores to be used in the parallelization process
#   export_accepted     = FALSE # save names linked to multiple accepted names
# )
# 
# 
# 
# # Output the file -----------------------------------------------------------
# 
# write.csv(move_df, file = "./data/tidy/move_df.csv")



# # Plot some examples -----------------------------------------------------------
# 
# pdf("./outputs/Loxodonta africana example.pdf", 
#     width = 6.5,
#     height = 6.5)
# 
# layout(mat = matrix(c(1,3,
#                       1,3,
#                       1,4,
#                       2,4,
#                       2,5,
#                       2,5), nrow = 6, byrow = T))
# 
# # Plot this
# #par(mfrow = c(3,1))
# op <- par()
# par(mar = c(2.1, 4.1, 2.1, 2.1))
# plot(NA, 
#      xlab = "",
#      ylab = "Displacement (km)",
#      xlim = c(0, (max_days * 24 * 60) / (60 * 24)),
#      ylim = c(1, 100000/1000),
#      #log = "y",
#      las = 1,
#      frame = F,
#      xaxt = "n",
#      )
# axis(1, at = seq(0,5), labels = rep("", 6))
# for(i in frug_events){
#   dd <- dat %>% filter(frug_event_id == i)
#   
#   lines(dd$time_diff_min / (60 * 24),
#         dd$displacement/1000,
#         col = rgb(0,0,0, 0.3))
#   # print(i)
#   # Sys.sleep(2)#stop()#
# }
# 
# 
# 
# par(mar = c(5.1, 4.1, 0.1, 2.1))
# plot(NA, 
#      xlab = "Time since hypothetical frugivory event (days)",
#      ylab = "Displacement (km)",
#      xlim = c(0, (max_days * 24 * 60) / (60 * 24)),
#      ylim = c(0.01, 100000/1000),
#      log = "y",
#      las = 1,
#      frame = F,
#      yaxt = "n"
# )
# axis(2, at = c(0.01,0.1, 1, 10, 100), 
#      labels = c(0.01,0.1, 1, 10, 100), 
#      las = 1)
# for(i in frug_events){
#   dd <- dat %>% filter(frug_event_id == i)
#   
#   dd$displacement[which(dd$displacement == 0)] <- 0.1
#   
#   lines(dd$time_diff_min / (60 * 24),
#         dd$displacement/1000,
#         col = rgb(0,0,0, 0.3))
# }
# 
# # Here's what the gut passage time distribution looks like for
# # Loxodonta africana
# 
# par(mar = c(5.1, 4.1, 2.1, 2.1))
# gpts <- (rlnorm(10000, meanlog = log(40), sdlog = 0.2) / 24) 
# gpts %>% hist(main = "",
#               freq = F,
#               breaks = seq(0, 5, by = 0.25),
#               xlab = "Gut passage time (days)",
#               las = 1)
# 
# est_disp <- rep(NA, length(gpts))
# ids <- unique(dat$individual.local.identifier)
# for(i in 1:length(gpts)){
#   dd <- dat %>% filter(individual.local.identifier == sample(ids, 1)) %>%
#     mutate(timeX = abs(time_diff_min - (gpts[i] * 60 * 24)))
#   est_disp[i] <- dd$displacement[which.min(dd$timeX)]
# }
# 
# (est_disp/1000) %>% hist(main = "",
#                   freq = F,
#                   breaks = 21,
#                   xlab = "Dispersal distance (km)")
# 
# (est_disp/1000) %>% log10() %>% hist(main = "",
#                   freq = F,
#                   breaks = 21,
#                   xlab = "Dispersal distance (km) log10 scale")
# 
# 
# 
# 
# dev.off()
# 
# # shape <- mean^2 / sd^2
# # rate <- mean / sd^2
# # mean <- 4000
# # sd <- 2
# 
# mod <- glm(est_disp ~ 1, family = "Gamma")
# summary(mod)
# mod_shape <- MASS::gamma.shape(mod)$alpha
# mod_mean <- 1/coef(mod)[1]
# 
# 
# mod_sd <- mod_mean^2 / mod_shape
# mod_rate <- mod_mean / mod_sd^2
# 
# par(mfrow = c(2,2))
# hist(est_disp)
# hist(log(est_disp))
# 
# #hist(rgamma(n = 10000, shape = mod_shape, rate = mod_rate))
# 
# MASS::fitdistr(est_disp/10, "Gamma")#, start = list(shape = 1, rate = 0.001), lower = 0.0001)
# hist(rgamma(n = 10000, shape = 1.5, rate = 0.003)*10)
# hist(rgamma(n = 10000, shape = 1.5, rate = 0.003)*10)
# 
# 
# 
# #
