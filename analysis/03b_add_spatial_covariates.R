# Load packages ----------------------------------------------------------------
sapply(list.files("./R", full.names = T), source)

ipak(c("tidyverse",
       "sf",
       "raster",
       "ggplot2"))

# Load move dataframe and make sf object
move_df <- read.csv("./data/tidy/move_df_with_gee_covs.csv")

mollcrs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
move_sf <- move_df %>% st_as_sf(coords = c("location.long", "location.lat"),
                        crs = projcrs) %>% 
  st_transform(crs = mollcrs)

move_sf[1,]


# Load ocean and land footprint index
ocean_fp <- raster("./data/spatial data/cumulative_impact_2013.tif")
#land_ghm <- raster("./data/spatial data/lulc-human-modification-terrestrial-systems_mollweide.tif")
land_fp <- raster("./data/spatial data/HFP2009.tif") # https://datadryad.org/stash/dataset/doi:10.5061/dryad.052q5


# Now extract

time0 <- Sys.time()
land_fp_vals <- raster::extract(land_fp,
                                 move_sf)
time1 <- Sys.time()
time1-time0


time0 <- Sys.time()
ocean_fp_vals <- raster::extract(ocean_fp,
                                 move_sf)
time1 <- Sys.time()
time1-time0

par(mfrow=c(2,1))
hist(land_fp_vals)
hist(ocean_fp_vals/8.89)
#hist(.3^(ocean_fp_vals))

# # This caused the computer to restart... Why?
# land_fp4 <- raster::aggregate(land_fp, fact = 4, fun = mean, na.rm = T)
# ocean_fp4 <- raster::aggregate(ocean_fp, fact = 4, fun = mean, na.rm = T)

# Bind the footprint columns
land_fp_df <- tibble(land_fp = land_fp_vals)
ocean_fp_df <- tibble(ocean_fp = ocean_fp_vals)

# In order to save this again, we will actually load in the
# original, non-sf version of 
move_df <- move_df %>% 
  bind_cols(land_fp_df) %>% 
  bind_cols(ocean_fp_df)

move_df[1,]


# Figure out why there are NAs

asdf <- move_df %>% 
  filter(is.na(land_fp) & is.na(ocean_fp))
asdf <- move_df %>% 
  filter(!is.na(land_fp) & !is.na(ocean_fp))

dim(asdf)



library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = asdf[sample(1:nrow(asdf), 1000),], aes(x = location.long, y = location.lat))

library(leaflet)
leaflet(asdf[sample(1:nrow(asdf), 1000),]) %>% addTiles() %>% addMarkers(lng = ~ location.long, lat = ~location.lat)

# Write this to CSV
write.csv(move_df, file = "./data/tidy/move_df_spat.csv", row.names = F)


