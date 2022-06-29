# Load packages ----------------------------------------------------------------
sapply(list.files("./R", full.names = T), source)

ipak(c("tidyverse"))


# Load move dataframe
move_df <- read.csv("./data/tidy/move_df.csv", row.names = 1)

# Manipulate human footprint data so that the ocean and land
# footprint indices are at the same scale

# Note that the range of the ocean values is: 0, 11.7999 for the entire raster
move_df <- move_df %>% 
  mutate(ocean_fp = ocean_fp / max(ocean_fp, na.rm = T)) %>% 
  mutate(fp = ifelse(!is.na(land_fp), land_fp, ocean_fp))

# Get a mean_fp value as well and join this to move_df
mean_fp <- move_df %>% 
  group_by(frug_event_id) %>% 
  dplyr::summarise(mean_fp = mean(fp, na.rm = T)) %>% 
  dplyr::select(frug_event_id, mean_fp)

move_df <- move_df %>% 
  left_join(mean_fp)


move_df$fp %>% is.na() %>% table()
move_df$mean_fp %>% is.na() %>% table()

# Figure out why there are NA values


asdf <- move_df %>% filter(is.na(mean_fp)) 
dim(asdf)
plot(location.lat ~ location.long,
     data = asdf[1:10000,])

# Let's try a frequentist approach first

ipak("lme4")
mod <- lmer(log(displacement) ~ log(time_diff_min) * mean_fp + 
              (log(time_diff_min) * mean_fp|individual.taxon.canonical.name) + 
              (1|frug_event_id),
          data = move_df) # log(mass) * * animal_group 

summary(mod)

ran_df <- coef(mod)$individual.taxon.canonical.name

# sp_df <- move_df %>% 
#   filter(!duplicated(individual.taxon.canonical.name))
# 
# ipak("sjPlot")
# plot_model(mod, type = "pred")#, terms = c("animal_group", "mean_fp"))
# 
# 
# # 
# asdf <- move_df %>% 
#   filter(animal_group == "bats")
# 
# dim(asdf)
# 
# plot(asdf$displacement ~ asdf$time_diff_min, log = "y", cex = 0.1)



# Plots

par(mfrow = c(5,5))
for(i in sort(unique(move_df$individual.taxon.canonical.name))[1:25]){
  asdf <- move_df %>%
    filter(individual.taxon.canonical.name == i)

  nmax <- min(nrow(asdf), 1000)

  prop_uniq_disp <- length(unique(asdf$displacement)) / nrow(asdf)

  plot(displacement ~ time_diff_min,  log = "y",
       data = asdf[sample(1:nrow(asdf), nmax),],
       main = paste(i, round(prop_uniq_disp, digits = 2)),
       cex = 0.1,
       ylim = c(1, 2000000))

  abline(h = 1, col = "blue")
  abline(h = 1000, col = "purple")
  abline(h = 50000, col = "red")
  
  #i <- sort(unique(move_df$individual.taxon.canonical.name))[7]
  
  hi_fp <- 0.5
  curve(exp(ran_df[i, 1] + ran_df[i, 2] * log(x) + hi_fp * ran_df[i, 3] + ran_df[i, 4] * hi_fp * log(x)),
        add = T, col = "brown", lty = 2)
  
  low_fp <- 0
  curve(exp(ran_df[i, 1] + ran_df[i, 2] * log(x) + low_fp * ran_df[i, 3] + ran_df[i, 4] * low_fp * log(x)),
        add = T, col = "green", lty = 2)

}



#





















# dataList <- list(
#   
#   # Individual data points
#   n = nrow(move_df),
#   time = log(move_df$time_diff_min),
#   displacement = log(move_df$displacement),
#   
#   # Frugivory event monitoring periods
#   n_event = length(unique(move_df$frug_event_id)),
#   n_event_vec = as.numeric(as.factor(move_df$frug_event_id)),
#   gHM = move_df$gHM[!duplicated(move_df$frug_event_id)] + 0.01,
#   
#   event_sp = move_df$individual.taxon.canonical.name[!duplicated(move_df$frug_event_id)] %>% 
#     as.factor() %>% as.numeric(),
#   
#   # Animal individuals
#   n_id = length(unique(move_df$individual.local.identifier)),
#   id = as.numeric(as.factor(move_df$individual.local.identifier)),
#   
#   # Species
#   n_sp = length(unique(move_df$individual.taxon.canonical.name)),
#   sp = as.numeric(as.factor(move_df$individual.taxon.canonical.name))
#   
# )
#
#
#
# sink("./analysis/displacement_jags.txt")
# cat(paste0("
# 
# model {
# 
#   # Likelihood
# 
#   for(i in 1:n){
# 
#     shape[i] <- pow(mean[i], 2) / pow(sd[sp[i]] * sqrt(time[i]), 2)
# 
#     rate[i] <- mean[i]  / pow(sd[sp[i]] * sqrt(time[i]),2)
# 
#     displacement[i] ~ dgamma(shape[i], rate[i])# T(0.0001, )
#     mean[i] <- mean_peak[n_event_vec[i]] * time[i] / (exp(b[sp[i]]) + time[i])
#   }
#   
#   for(j in 1:n_event){
#     mean_peak[j] <- exp(a[event_sp[j]] + beta_ghm[event_sp[j]] * gHM[j])
#   }
# 
#   
#   # Priors
#   for(k in 1:n_sp){
#     a[k] ~ dnorm(0, 0.01)
#     b[k] ~ dnorm(0, 0.01)
#     
#     beta_ghm[k] ~ dnorm(beta_ghm_mean, beta_ghm_tau)# T(-1,1)
#   
#     sd[k] ~ dunif(0, 10)
#   }
#   
#   beta_ghm_mean ~ dnorm(0, 0.01)
#   beta_ghm_tau <- pow(beta_ghm_sd, -2)
#   beta_ghm_sd ~ dunif(0, 10)
# 
#   # Derived quantities
#   
#   
#   
# 
# 
# } # End of model
# 
#     "),fill=TRUE)
# sink()



dataList <- list(
  
  # Individual data points
  n = nrow(move_df),
  time = log(move_df$time_diff_min),
  displacement = log(move_df$displacement),
  mass = log(move_df$mass),
  
  # Frugivory event monitoring periods
  n_event = length(unique(move_df$frug_event_id)),
  n_event_vec = as.numeric(as.factor(move_df$frug_event_id)),
  #gHM = move_df$gHM + 0.01,#[!duplicated(move_df$frug_event_id)] + 0.01,
  
  event_sp = move_df$individual.taxon.canonical.name[!duplicated(move_df$frug_event_id)] %>% 
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
    mean[i] <- beta_0[sp[i]] + exp(beta_t[sp[i]]) * time[i] + beta_mass[animal_group[i]] * mass[i]# + beta_ghm[sp[i]] * gHM[i]
  }
  
  # Priors
  for(j in 1:n_sp){
    beta_0[j] ~ dnorm(group_mean_beta_0[animal_group_of_species[j]], 0.01)
    beta_t[j] ~ dnorm(group_mean_beta_t[animal_group_of_species[j]], 0.01)
    #beta_ghm[j] ~ dnormgroup_mean_ghm[animal_group_of_species[j]], 0.01)
    
    tau[j] <- pow(sd[j], -2)
    sd[j] ~ dnorm(group_mean_sd[animal_group_of_species[j]], 0.01) T(0,) # half normal sd following Gelman et al. 2006
  }
  
  for(k in 1:n_animal_group){
    group_mean_beta_0[k] ~ dnorm(0, 0.01)
    group_mean_beta_t[k] ~ dnorm(0, 0.1)
    
    group_mean_sd[k] ~ dunif(0, 10)
    
    beta_mass[k] ~ dnorm(overall_mean_beta_mass, 0.1)
  }
  
  overall_mean_beta_mass ~ dnorm(0, 0.01)
    
  

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
                                n.adapt = 250,
                                n.chains = 3, 
                                #inits = inits
)

update(displacement_jags, 1000)

#save(file = "./outputs/gamma_jags_model_temp_precip.RData", displacement_jags)


variables_to_sample <- c("beta_a_0",
                         "beta_a_ghm",
                         "beta_b_0",
                         "beta_b_ghm",
                         "sd")

variables_to_sample <- c("a",
                         "b",
                         "beta_ghm",
                         "beta_ghm_mean",
                         "sd")

variables_to_sample <- c("beta_0",
                         "beta_t",
                         "beta_mass",
                         #"beta_ghm",
                         "sd",
                         "group_mean_beta_0",
                         "group_mean_beta_t",
                         "group_mean_beta_t",
                         "group_mean_sd",
                         "overall_mean_beta_mass")

displacement_samples <- coda.samples(displacement_jags,
                                     variable.names = variables_to_sample,
                                     thin = 20,
                                     n.iter = 5000)
plot(displacement_samples)


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


# # Save displacement samples and a few other helper vectors
# save(file = "./outputs/gamma_samples_temp_precip.RData",
#      displacement_samples,
#      variables_to_sample,
#      cov_to_var,
#      cov_to_name,
#      cov_to_sample,
#      n_cov,
#      a_vars_to_sample,
#      b_vars_to_sample)
