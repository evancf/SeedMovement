# Testing out some stuff - note that you need to individually
# accept the license terms online before downloading

# Load packages ----------------------------------------------------------------
source(list.files("./R", full.names = T))

ipak(c("tidyverse",
       "move",
       "lutz",
       "lubridate"))


# Read in data

move_df <- read.csv("./data/tidy/move_df.csv", header = T)




# 
dd <- move_df %>% filter(time_diff_min > 0) %>% 
  filter(individual.taxon.canonical.name != "Milvus migrans") %>% 
  filter(displacement < (200 * 1000)) # There are a few outliers
dd$displacement[which(dd$displacement == 0)] <- min(dd$displacement[which(dd$displacement != 0)])

# To test things out, will simulate an effect of gHM
# Want to make this very strongly correlated with displacement
dd$gHM <- NA

for(j in unique(dd$individual.taxon.canonical.name)){
  sp_set <- filter(dd, individual.taxon.canonical.name == j)
  sp_max_val <- max(sp_set$displacement)
  
  for(i in unique(sp_set$frug_event_id)){
    ind <- which(dd$frug_event_id == i)
    
    max_val <- max(dd$displacement[ind])
    
    dd$gHM[ind] <- abs(1-(max_val/sp_max_val))
    
  }
}



dataList <- list(
  
  # Individual data points
  n = nrow(dd),
  time = dd$time_diff_min,
  displacement = dd$displacement/1000,
  
  # Frugivory event monitoring periods
  n_event = length(unique(dd$frug_event_id)),
  n_event_vec = as.numeric(as.factor(dd$frug_event_id)),
  gHM = dd$gHM[!duplicated(dd$frug_event_id)] + 0.01,
  
  event_sp = dd$individual.taxon.canonical.name[!duplicated(dd$frug_event_id)] %>% 
    as.factor() %>% as.numeric(),
  
  # Animal individuals
  n_id = length(unique(dd$individual.local.identifier)),
  id = as.numeric(as.factor(dd$individual.local.identifier)),
  
  # Species
  n_sp = length(unique(dd$individual.taxon.canonical.name)),
  sp = as.numeric(as.factor(dd$individual.taxon.canonical.name))
  
)



sink("./analysis/displacement_jags.txt")
cat(paste0("

model {

  # Likelihood

  for(i in 1:n){

    shape[i] <- pow(mean[i], 2) / pow(sd[sp[i]] * sqrt(time[i]), 2)

    rate[i] <- mean[i]  / pow(sd[sp[i]] * sqrt(time[i]),2)

    displacement[i] ~ dgamma(shape[i], rate[i])# T(0.0001, )
    mean[i] <- mean_peak[n_event_vec[i]] * time[i] / (exp(b[sp[i]]) + time[i])
  }
  
  for(j in 1:n_event){
    mean_peak[j] <- exp(a[event_sp[j]] + beta_ghm[event_sp[j]] * gHM[j])
  }

  
  # Priors
  for(k in 1:n_sp){
    a[k] ~ dnorm(0, 0.01)
    b[k] ~ dnorm(0, 0.01)
    
    beta_ghm[k] ~ dnorm(beta_ghm_mean, beta_ghm_tau)# T(-1,1)
  
    sd[k] ~ dunif(0, 10)
  }
  
  beta_ghm_mean ~ dnorm(0, 0.01)
  beta_ghm_tau <- pow(beta_ghm_sd, -2)
  beta_ghm_sd ~ dunif(0, 10)

  # Derived quantities
  
  
  


} # End of model

    "),fill=TRUE)
sink()


ipak("rjags")


reps <- length(unique(dd$individual.taxon.canonical.name))
inits <- list(list(a = rep(2, reps), b = rep(5, reps), beta_ghm = rep(0, reps)),
              list(a = rep(2, reps), b = rep(5, reps), beta_ghm = rep(0, reps)),
              list(a = rep(2, reps), b = rep(5, reps), beta_ghm = rep(0, reps)))

displacement_jags <- jags.model("./analysis/displacement_jags.txt", 
                                data = dataList,
                                n.adapt = 1000,
                                n.chains = 3, 
                                inits = inits
)

update(displacement_jags, 2000)

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

displacement_samples <- coda.samples(displacement_jags,
                                variable.names = variables_to_sample,
                                thin = 25,
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


