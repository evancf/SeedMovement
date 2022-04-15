# Testing out some stuff - note that you need to individually
# accept the license terms online before downloading

# Load packages ----------------------------------------------------------------
source(list.files("./R", full.names = T))

ipak(c("tidyverse",
       "move",
       "lutz",
       "lubridate"))


# To test things out, will simulate an effect of gHM
# Want to make this very strongly correlated with displacement
dat$gHM <- NA
for(i in unique(dat$frug_event_id)){
  ind <- which(dat$frug_event_id == i)
  
  max_val <- max(dat$displacement[ind])
  
  dat$gHM[ind] <- abs(1-(max_val/17000))
  
}




# 


dd <- dat %>% filter(time_diff_min > 0)
dataList <- list(
  
  # Individual dda points
  n = nrow(dd),
  time = dd$time_diff_min,
  displacement = dd$displacement/1000,
  
  # Frugivory event monitoring periods
  n_event = length(unique(dd$frug_event_id)),
  n_event_vec = as.numeric(as.factor(dd$frug_event_id)),
  gHM = dd$gHM[!duplicated(dd$frug_event_id)],
  
  # Individuals
  n_id = length(unique(dd$individual.local.identifier)),
  id = as.numeric(as.factor(dd$individual.local.identifier))
  
)

# Define model

sink("./analysis/displacement_jags.txt")
cat(paste0("

model {

  # Likelihood

  for(i in 1:n){

    shape[i] <- pow(mean[i], 2) / pow(sd*sqrt(time[i]), 2)

    rate[i] <- mean[i]  / pow(sd*sqrt(time[i]),2)

    displacement[i] ~ dgamma(shape[i], rate[i])
    mean[i] <- exp(a[n_event_vec[i]]) * time[i] / (exp(b[n_event_vec[i]]) + time[i]) # Monod
  }
  
  for(j in 1:(n_event)){
  
    a[j] <- beta_a_0 + beta_a_ghm * gHM[j]
    b[j] <- beta_b_0 + beta_b_ghm * gHM[j]
    #ind_mean_sdd[j] <- exp(a[j]) * 2400 / (exp(b[j]) + 2400) + beta_ghm * gHM[j]
  }

  
  # Priors

  beta_a_0 ~ dnorm(0, 0.001)
  beta_b_0 ~ dnorm(0, 0.001)
  beta_a_ghm ~ dnorm(0, 0.001)
  beta_b_ghm ~ dnorm(0, 0.001)
  
  sd ~ dunif(0, 100)

  # Derived quantities
  
  
  


} # End of model

    "),fill=TRUE)
sink()


sink("./analysis/displacement_jags.txt")
cat(paste0("

model {

  # Likelihood

  for(i in 1:n){

    shape[i] <- pow(mean[i], 2) / pow(sd*sqrt(time[i]), 2)

    rate[i] <- mean[i]  / pow(sd*sqrt(time[i]),2)

    displacement[i] ~ dgamma(shape[i], rate[i])
    mean[i] <- exp(a) * time[i] / (exp(b) + time[i]) * (1 + beta_a_ghm * gHM[n_event_vec[i]])
  }

  
  # Priors

  a ~ dnorm(0, 0.001)
  b ~ dnorm(0, 0.001)
  beta_a_ghm ~ dnorm(0, 0.001)
  
  sd ~ dunif(0, 100)

  # Derived quantities
  
  
  


} # End of model

    "),fill=TRUE)
sink()


ipak("rjags")

reps <- length(unique(dd$frug_event_id))
inits <- list(list(a = rep(4, reps), b = rep(5, reps)),
              list(a = rep(4, reps), b = rep(5, reps)),
              list(a = rep(4, reps), b = rep(5, reps)))

# inits <- list(list(beta_a_0 = rep(3, 1), beta_b_0 = rep(-1, 1)),
#               list(beta_a_0 = rep(4, 1), beta_b_0 = rep(-4, 1)),
#               list(beta_a_0 = rep(5, 1), beta_b_0 = rep(-2, 1)))


displacement_jags <- jags.model("./analysis/displacement_jags.txt", data = dataList,
                           n.adapt = 1000,
                           n.chains = 3#, inits = inits
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
                         "beta_a_ghm",
                         "sd")

displacement_samples <- coda.samples(displacement_jags,
                                variable.names = variables_to_sample,
                                thin = 10,
                                n.iter = 1000)
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


