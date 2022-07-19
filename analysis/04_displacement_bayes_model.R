# Load packages ----------------------------------------------------------------
sapply(list.files("./R", full.names = T), source)

ipak(c("tidyverse"))


# Load move dataframe
move_df <- read.csv("./data/tidy/move_df_spat.csv")



# Want to reconsider the way we handle the animal groupings
marine_foraging_birds <- c(
  "Alca torda",
  "Fratercula arctica",
  "Uria aalge",
  "Uria lomvia",
  "Hydroprogne caspia",
  "Ichthyaetus audouinii",
  "Larus marinus",
  "Onychoprion fuscatus",
  "Rissa tridactyla",
  "Sterna dougallii",
  "Pelecanus occidentalis",
  "Phaethon lepturus",
  "Phoebastria irrorata",
  "Thalassarche carteri",
  "Thalassarche chrysostoma",
  "Calonectris diomedea",
  "Puffinus puffinus",
  "Puffinus yelkouan",
  "Thalassoica antarctica",
  "Eudyptes robustus",
  "Pygoscelis adeliae",
  "Fregata aquila",
  "Fregata magnificens",
  "Morus bassanus",
  "Sula dactylatra")

marine_foraging_mammals <- c(
  "Halichoerus grypus",
  "Balaenoptera musculus",
  "Balaenoptera physalus")

raptor_orders <- c("Accipitriformes", 
                   "Falconiformes", 
                   "Strigiformes")

# Water and wading birds?

move_df %>%
  filter(class == "Aves",
         !individual.taxon.canonical.name %in% marine_foraging_birds,
         !order %in% raptor_orders) %>%
  dplyr::select(order, family, individual.taxon.canonical.name) %>%
  dplyr::arrange(order, family) %>%
  unique()

ww_bird_orders <- c("Anseriformes",
                    "Charadriiformes",
                    "Ciconiiformes",
                    "Gruiformes", # Be careful here. Currently just a crane and a coot, but there are land rails too...
                    "Pelecaniformes")
ww_bird_species <- c("Phalacrocorax carbo")

move_df$animal_group[which(move_df$individual.taxon.canonical.name %in% marine_foraging_birds)] <- "bird_marine"
# Raptors
move_df$animal_group[which(move_df$order %in% raptor_orders)] <- "bird_raptor"

# # Water and wading birds?
# move_df$animal_group[which(move_df$order %in% ww_bird_orders)] <- "bird_water_wading"
# move_df$animal_group[which(move_df$individual.taxon.canonical.name %in% ww_bird_species)] <- "bird_water_wading"

# Marine mammals
move_df$animal_group[which(move_df$individual.taxon.canonical.name %in% marine_foraging_mammals)] <- "mamm_marine"

move_df$animal_group %>% table()



# Manipulate human footprint data so that the ocean and land
# footprint indices are at the same scale

# Note that the range of the ocean values is: 0, 11.7999 for the entire raster
# and the land max is ~50.

center_fun <- function(x){ x - mean(x, na.rm - T)}
range01 <- function(x){ (x - min(x, na.rm - T)) / (max(x, na.rm - T) - min(x, na.rm - T))}

# # Uncomment if doing both marine and terrestrial species (and NDVI and chlor)
# move_df <- move_df %>% 
#   mutate(land_fp_n = land_fp / max(land_fp, na.rm = T),
#          ocean_fp_n = ocean_fp / max(ocean_fp, na.rm = T),
#          ndvi_terra_n = ndvi_terra / max(ndvi_terra, na.rm = T),
#          chlor_terra_n = (log(chlor_terra) + 2) / max((log(chlor_terra) + 2), na.rm = T)) %>% 
#   mutate(fp = ifelse(!is.na(land_fp_n), land_fp_n, ocean_fp_n),
#          product = ifelse(!is.na(ndvi_terra_n), ndvi_terra_n, chlor_terra_n))

move_df <- move_df %>%
  mutate(land_fp_n = land_fp / max(land_fp, na.rm = T),
         ndvi_terra_n = ndvi_terra / max(ndvi_terra, na.rm = T)) %>%
  mutate(fp = land_fp_n,
         product = ndvi_terra_n)


# Get a mean_fp value and production value as well and join this to move_df
mean_fp <- move_df %>% 
  group_by(frug_event_id) %>% 
  dplyr::summarise(mean_fp = mean(fp, na.rm = T)) %>% 
  dplyr::select(frug_event_id, mean_fp)

move_df <- move_df %>% 
  left_join(mean_fp)

# Get a mean_fp value and production value as well and join this to move_df
mean_product <- move_df %>% 
  group_by(frug_event_id) %>% 
  dplyr::summarise(mean_product = mean(product, na.rm = T)) %>% 
  dplyr::select(frug_event_id, mean_product)

move_df <- move_df %>% 
  left_join(mean_product)


# move_df$fp %>% is.na() %>% table()
# move_df$mean_fp %>% is.na() %>% table()
# 
# move_df$product %>% is.na() %>% table()
# move_df$mean_product %>% is.na() %>% table()
# move_df[!duplicated(move_df$frug_event_id),] %>% pull(mean_fp) %>% is.na() %>% table()

# # This might be helpful
# sp_summary <- move_df %>% 
#   group_by(individual.taxon.canonical.name) %>% 
#   summarise(ocean_non_na = sum(!is.na(ocean_fp)),
#             land_non_na = sum(!is.na(land_fp)),
#             animal_group = first(animal_group),
#             order = first(order),
#             family = first(family)) %>% 
#   mutate(ocean_land_ratio = round(ocean_non_na / (ocean_non_na + land_non_na) , digits = 2)) %>% 
#   arrange(animal_group, order, family, individual.taxon.canonical.name)





# # Let's try a frequentist approach first
# 
# move_df <- move_df %>% 
#   filter(!animal_group %in% c("mamm_cetacean", "bird_marine"),
#          !individual.taxon.canonical.name %in% marine_foraging_mammals)
# 
# ipak("lme4")
# move_mod <- lmer(log(displacement) ~ log(time_diff_min) + mean_fp * animal_group + mean_product * animal_group +
#               (log(time_diff_min)|individual.taxon.canonical.name) +
#               (1|frug_event_id)
#               ,
#           data = move_df) # log(mass) * * animal_group
# 
# summary(move_mod)
# 
# ran_df <- coef(move_mod)$individual.taxon.canonical.name
# 
# # sp_df <- move_df %>%
# #   filter(!duplicated(individual.taxon.canonical.name))
# #
# # ipak("sjPlot")
# # plot_model(mod, type = "pred")#, terms = c("animal_group", "mean_fp"))
# #
# #
# # #
# # asdf <- move_df %>%
# #   filter(animal_group == "bats")
# #
# # dim(asdf)
# #
# # plot(asdf$displacement ~ asdf$time_diff_min, log = "y", cex = 0.1)
# 
# 
# 
# # Plots
# 
# par(mfrow = c(5,5))
# for(i in sort(unique(move_df$individual.taxon.canonical.name))[1:25]){
#   asdf <- move_df %>%
#     filter(individual.taxon.canonical.name == i)
# 
#   nmax <- min(nrow(asdf), 1000)
# 
#   prop_uniq_disp <- length(unique(asdf$displacement)) / nrow(asdf)
# 
#   plot(displacement ~ time_diff_min,  log = "y",
#        data = asdf[sample(1:nrow(asdf), nmax),],
#        main = paste(i, round(prop_uniq_disp, digits = 2)),
#        cex = 0.1,
#        ylim = c(1, 2000000))
# 
#   abline(h = 1, col = "blue")
#   abline(h = 1000, col = "purple")
#   abline(h = 50000, col = "red")
# 
#   #i <- sort(unique(move_df$individual.taxon.canonical.name))[7]
# 
#   hi_fp <- 0.5
#   curve(exp(ran_df[i, 1] + ran_df[i, 2] * log(x) + hi_fp * ran_df[i, 3] + ran_df[i, 4] * hi_fp * log(x)),
#         add = T, col = "brown", lty = 2)
# 
#   low_fp <- 0
#   curve(exp(ran_df[i, 1] + ran_df[i, 2] * log(x) + low_fp * ran_df[i, 3] + ran_df[i, 4] * low_fp * log(x)),
#         add = T, col = "green", lty = 2)
# 
# }



#



# # Get indices associated with the frugivory events
# old_move_df <- move_df
# move_df <- old_move_df

move_df <- move_df[complete.cases(dplyr::select(move_df,
                                                time_diff_min,
                                                displacement,
                                                mean_product,
                                                mean_fp,
                                                mass)),]

move_df <- move_df %>% 
  dplyr::arrange(frug_event_id,
                 time_diff_min)



# move_df <- move_df[1:20000,]

event_inds <- which(!duplicated(move_df$frug_event_id))
dataList <- list(
  
  # Individual data points
  n = nrow(move_df),
  time = log(move_df$time_diff_min),
  displacement = log(move_df$displacement),
  
  # Predictor variables
  mass = log(move_df$mass)[event_inds],
  fp = move_df$mean_fp[event_inds],
  product = move_df$mean_product[event_inds],
  
  # Frugivory event monitoring periods
  n_event = length(unique(move_df$frug_event_id)),
  n_event_vec = as.numeric(as.factor(move_df$frug_event_id)),
  
  event_sp = move_df$individual.taxon.canonical.name[event_inds] %>% 
    as.factor() %>% as.numeric(),
  
  # Animal individuals
  n_id = length(unique(move_df$individual.local.identifier)),
  id = as.numeric(as.factor(move_df$individual.local.identifier)),
  
  # Species
  n_sp = length(unique(move_df$individual.taxon.canonical.name)),
  sp = as.numeric(as.factor(move_df$individual.taxon.canonical.name)),
  
  # Animal group
  n_animal_group = length(unique(move_df$animal_group)),
  animal_group = as.numeric(as.factor(move_df$animal_group)),
  
  animal_group_of_species = as.numeric(as.factor(move_df$animal_group))[!duplicated(move_df$individual.taxon.canonical.name)]
  
  
)

sink("./analysis/displacement_jags.txt")
cat(paste0("

model {

  # Likelihood

  for(i in 1:n){
    displacement[i] ~ dnorm(mean[i], tau[sp[i]])
    mean[i] <- beta_0[sp[i]] + exp(beta_t[sp[i]]) * time[i] + lin_pred[n_event_vec[i]] + rand_id[id[i]]
  }
  
  for(i in 1:n_event){
    # # This version has group-specfic mass covariates
    # lin_pred[i] <- beta_mass[animal_group[i]] * mass[i] + beta_fp[animal_group[i]] * fp[i] + beta_product[animal_group[i]] * product[i]
    
    # # This version has a single mass covariates across all taxa
    # lin_pred[i] <- overall_mean_beta_mass * mass[i] + beta_fp[animal_group[i]] * fp[i] + beta_product[animal_group[i]] * product[i]
    
    # This version has no mass covariate
    lin_pred[i] <- beta_fp[animal_group[i]] * fp[i] + beta_product[animal_group[i]] * product[i]
  }
  
  # Priors for event-level 'random effects'
  for(j in 1:n_id){
    rand_id[j] ~ dnorm(0, 0.1)
  }
  
  # Priors for species-level coefs
  for(j in 1:n_sp){
    beta_0[j] ~ dnorm(group_mean_beta_0[animal_group_of_species[j]], 0.1)
    beta_t[j] ~ dnorm(group_mean_beta_t[animal_group_of_species[j]], 1)
    
    tau[j] <- pow(sd[j], -2)
    sd[j] ~ dnorm(group_mean_sd[animal_group_of_species[j]], 0.1) T(0,) # half normal sd following Gelman et al. 2006
  }
  
  # Priors for group-level coefs
  for(k in 1:n_animal_group){
    group_mean_beta_0[k] ~ dnorm(0, 0.1)
    group_mean_beta_t[k] ~ dnorm(0, 1)
    
    group_mean_sd[k] ~ dunif(0, 10)
    
    # beta_mass[k] ~ dnorm(overall_mean_beta_mass, 0.1) # Commenting out to only use a single mass coefficient across taxa
    # beta_fp[k] ~ dnorm(overall_mean_beta_fp, 0.1) # Changing this out as we expect different taxa to specifically respond differently to fp
    beta_fp[k] ~ dnorm(0, 0.1)
    beta_product[k] ~ dnorm(overall_mean_beta_product, 0.1) # Commenting these out to remove this level of hierarchy
  }
  
  overall_mean_beta_mass ~ dnorm(0, 0.1)
  # overall_mean_beta_fp ~ dnorm(0, 0.1) # Commenting this out as we expect different taxa to specifically respond differently to fp
  overall_mean_beta_product ~ dnorm(0, 0.1) 
    
  # Derived quantities

} # End of model

    "),fill=TRUE)
sink()

ipak("rjags")

reps <- length(unique(move_df$individual.taxon.canonical.name))
inits <- list(list(a = rep(2, reps), b = rep(5, reps), beta_ghm = rep(0, reps)),
              list(a = rep(2, reps), b = rep(5, reps), beta_ghm = rep(0, reps)),
              list(a = rep(2, reps), b = rep(5, reps), beta_ghm = rep(0, reps)))

displacement_jags <- jags.model("./analysis/displacement_jags.txt", 
                                data = dataList,
                                n.adapt = 200,
                                n.chains = 3, 
                                #inits = inits
)
Sys.time() # Started at 1:52 pm on Saturday

# Looks like it takes about 9 hours to get everything going
# And it takes about 45 minutes to do 100 iterations. 0.45 minutes per iteration

update(displacement_jags, 1000)
Sys.time() # and it took this much longer to do 100 iterations

save(file = "./outputs/displacement_mod_0713.RData", displacement_jags)


variables_to_sample <- c("beta_0",
                         "beta_t",
                         "sd",
                         "group_mean_beta_0",
                         "group_mean_beta_t",
                         "group_mean_sd",
                         "beta_mass",
                         "beta_fp",
                         "beta_product",
                         "overall_mean_beta_product",
                         "overall_mean_beta_mass",
                         "overall_mean_beta_fp",
                         "rand_id")


time0 <- Sys.time()
displacement_samples <- coda.samples(displacement_jags,
                                     variable.names = variables_to_sample,
                                     thin = 5,
                                     n.iter = 2000)
#plot(displacement_samples)
time1 <- Sys.time()
time1-time0 # About 0.45 minutes per iteration?

ipak("MCMCvis")
MCMCtrace(displacement_samples, 
          params = "beta_t")

plot(dat$time_diff_min,
     dat$displacement/1000,
     cex = 0.1)
#curve(exp(1.78) * x / (exp(6.95) + x), add = T, col = "red")

curve(exp(5.1 + -4.4 * .1) * x / (exp(10.2 + -4.1 * .1) + x), add = T, col = "blue")
curve(exp(5.1 + -4.4 * .9) * x / (exp(10.2 + -4.1 * .9) + x), add = T, col = "red")

curve(exp(3) * x / (exp(7.6) + x), add = T, col = "blue")
curve(exp(3) * x / (exp(7.6) + x) * (1 + -0.86), add = T, col = "red")

x <- 2500
exp(3) * x / (exp(7.6) + x)
exp(3) * x / (exp(7.6) + x) * (1 + -0.86)


# Save displacement samples and a few other helper vectors
save(file = "./outputs/displacement_samples_0713.RData",
     displacement_samples,
     variables_to_sample)



ipak("lme4")
move_mod <- lmer(log(displacement) ~ log(time_diff_min) + mean_fp * animal_group + mean_product + # * animal_group + log(mass) +
              (log(time_diff_min)|individual.taxon.canonical.name) +
              (1|frug_event_id)
              ,
          data = move_df) # log(mass) * * animal_group

summary(move_mod)

sjPlot::plot_model(move_mod, type = "int")

ipak("effects")




terr_df <- move_df %>% 
  filter(!animal_group %in% c("mamm_cetacean", "bird_marine"),
         individual.taxon.canonical.name != "Halichoerus grypus")

terr_df$animal_group %>% table()

terr_df <- terr_df %>% 
  mutate(diet_group = ifelse(animal_group %in% c("bird_raptor", "mamm_carns"),
                             "carnivore", "non_carnivore")) %>% 
  mutate(diet_animal_group = paste(class, diet_group, sep = "_"))

terr_mod <- lmer(log(displacement) ~ log(time_diff_min) + mean_fp * diet_animal_group + mean_product + #log(mass) * diet_animal_group + # * animal_group +
                   (log(time_diff_min)|individual.taxon.canonical.name) +
                   (1|frug_event_id)
                 ,
                 data = terr_df)

summary(terr_mod)

AIC(terr_mod)

terr_df$individual.taxon.canonical.name %>% table() %>% length()


