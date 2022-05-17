
# 
library("tidyverse")
library("RCurl")

# Get movebank metadata
move_metadata <- read.csv("./data/movebank_metadata.csv", header = T)

# Filter to get species that we can download
move_metadata %>% glimpse()

move_metadata <- move_metadata %>% filter(i_have_download_access)

study_ids <- move_metadata %>% pull(id)

# Get some info about sensors from the API
opts <- curlOptions(userpwd = "evanfricke:Hello_567vbv") 

sensor_ids <- getURL("https://www.movebank.org/movebank/service/direct-read?entity_type=tag_type", 
                     .opts=opts)
sensor_ids <- read.csv(text = sensor_ids)

sensors_to_use <- sensor_ids %>% 
  filter(is_location_sensor == "true")





# Loop through to download
downloaded_files <- list.files("./data/Movebank raw/")

# Get study IDs to do

study_ids_to_do <- study_ids[!((move_metadata %>% pull(name)) %in% gsub(".csv", "", downloaded_files, fixed = T))]

# set.seed(66)
#sampled_ids <- study_ids[sample(1:length(study_ids), 100)]
for(i in sample(study_ids_to_do)){
  
  print(paste("starting on", move_metadata %>% 
                filter(id == i) %>% 
                pull(name)))
  # First see if this is already downloaded
  move_filename <- move_metadata %>% 
    filter(id == i) %>% 
    pull(name) %>% 
    paste0(".csv") %>% 
    gsub("/", ".", ., fixed = T)
  
  
  if(grepl("homing pigeons", move_filename)) next()
  if(grepl("Homing Pigeons", move_filename)) next()
  if(grepl("Galapagos Tortoise", move_filename)) next()
  if(grepl("MPIAB White Stork GSM 2013", move_filename)) next()
  if(grepl("Fall migration of white storks in 2014", move_filename)) next()
  if(grepl("High-altitude flights of Himalayan vultures", move_filename)) next()
  if(grepl("LifeTrack White Stork SW Germany", move_filename)) next()
  if(grepl("LBBG_ZEEBRUGGE - Lesser black-backed gulls", move_filename)) next()
  if(grepl("Soaring flight in Eurasian griffon vultures", move_filename)) next()
  if(grepl("LifeTrack White Stork Vorarlberg", move_filename)) next()
  if(grepl("HUJ MPIAB White Stork E-Obs", move_filename)) next()
  if(grepl("Caspian Gull", move_filename)) next()
  if(grepl("LifeTrack White Stork Rheinland-Pfalz", move_filename)) next()
  if(grepl("LifeTrack White Stork Poland ECG", move_filename)) next()
  if(grepl("HG_OOSTENDE - Herring gulls", move_filename)) next()
  if(grepl("Northern Harrier (Breeding)", move_filename)) next()
  if(grepl("LifeTrack White Stork", move_filename)) next()
  
  
  if(move_filename %in% downloaded_files){
    print("already done")
    next()
  } 
  
  # See what sensors are available
  sensors <- move_metadata %>% 
    filter(id == i) %>% 
    pull(sensor_type_ids) %>% 
    strsplit(split = ",") %>% 
    unlist()
  
  all_csv_dat <- NA
  csv_dat <- NA
  
  for(j in sensors){
    
    # Get code for sensor id
    sensor_id <- sensors_to_use %>% filter(name == j) %>% pull(id)
    
    # Get license terms
    base_url <- paste0("https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=", 
                       i,
                       "&sensor_type_id=",
                       sensor_id,
                       "&attributes=individual_local_identifier,tag_local_identifier,timestamp,location_long,location_lat,visible,individual_taxon_canonical_name,study_id,sensor_type_id,event_id")
    
    url_dat <- getURL(base_url, .opts=opts)
    
    if(substr(url_dat, 1, 40) == "<html>\n<p>By accepting this document the"){
      hash <- digest::digest(url_dat, algo="md5", serialize = F)
      
      url_with_hash <- paste0(base_url, 
                              "&license-md5=",
                              hash,
                              "&api-token=610d0a02-c4bf-4544-be08-02c4ed726a87")
      
      url_dat <- getURL(url_with_hash, .opts=opts)
      csv_dat <- read.csv(text = url_dat)
      
    } else{
      csv_dat <- read.csv(text = url_dat)
    }
    
    if(dim(csv_dat)[1] == 0){
      print("no data in this one...")
      next()
    } 
    
    if(all(is.na(csv_dat))) stop()
    
    # Add this to the all_csv_dat
    if(is.na(all_csv_dat)){
      all_csv_dat <- csv_dat
    } else{
      all_csv_dat <- rbind(all_csv_dat, csv_dat)
    }
  }
  
  # Write this out
  write.csv(all_csv_dat, file = paste0("./data/Movebank raw/", move_filename))
  
  print(paste0("done with #",which(i == study_ids)))

}


foo <- "https://www.movebank.org/movebank/service/direct-read?entity_type=sensor" %>% 
  getURL(.opts = opts)

read.csv(text = foo)

# 
# 1000 * 60*60 * 24
# 
# 90000000
# 
# 
# 
# 
# curl -v -u evanfricke:Hello_567vbv -c cookies.txt -o license_terms.txt "https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=12112706"
# 
# library("digest")
# digest(as.character(aaa), algo="md5")
# 
# View(aaa)
# str(aaa)
# str(fdsa)
# 
# head(fdsa)
# 
# fdsa <- XML::readHTMLTable(asdf, header = TRUE)
# 
# 
# "api-token":"610d0a02-c4bf-4544-be08-02c4ed726a87"
# 
# ?getURL
