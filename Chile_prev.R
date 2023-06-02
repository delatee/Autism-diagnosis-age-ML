# Adele Tyson, agmt3@cam.ac.uk
# 22/5/2023
# Bayesian prevalence analysis of autism prevalence in Chile and by school's region

# Load the data and environment setup
source("Autism-diagnosis-age-ML/Chile.R")

chile_stdpop_raw <- read_excel("04_Data/pop_chile_2021_single_age.xlsx") %>%
  clean_names() 

chile_stdpop <- chile_stdpop_raw %>%
  filter(sex != 9) %>%
  rename("std_pop" = "pop_2021") %>%
  mutate(pop_prop = std_pop / sum(std_pop))



################################################################################

# Try Bayesian analysis of autism prevalence and specificity and sensitivity of school assessment
# "Bayesian Estimation of Disease Prevalence and the Parameters of Diagnostic Tests in the Absence of a Gold Standard"
# Lawrence Joseph, Theresa W. Gyorkos, Louis Coupal
# https://www.cambridge.org/core/journals/epidemiology-and-psychiatric-sciences/article/bayesian-approach-to-estimating-the-population-prevalence-of-mood-and-anxiety-disorders-using-multiple-measures/DB1D2CA6C27C7E8C85C60B62B969BB72

# Use sensitivity and specificity of Social Attention and Communication Surveillance–Revised (SACS-R) tool
# "Diagnostic Accuracy of the Social Attention and Communication Surveillance–Revised With Preschool Tool for Early Autism Detection in Very Young Children"
# Josephine Barbaro, Nancy Sadka, Melissa Gilbert, et al
# https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2789926

chile_bayes_aut <- chile %>%
  filter(age_june30 >= 6 & age_june30 <= 18,
         #special_needs_status == 1,
         sex != 0) %>%
  mutate(autism = ifelse(special_needs_code == 105, 1, 0),
         age_cat = ifelse(age_june30 <= 8, 1, ifelse(age_june30 <= 11, 2, ifelse(age_june30 <= 14, 3, 4)))) %>% 
          # 1 = 6-8, 2 = 9-11, 3 = 12-14, 4 = 15-18
  select(#school_code,
    school_region_name_abr,
    #school_rurality_code,
    #teaching_code1,
    #grade_code1,
    #grade_letter, 
    #student_id,
    sex,             # Maybe add this back in, random effect on sex might be useful
    age_june30,
    #special_needs_status,
    #special_needs_code,
    #student_region_code,
    #economic_sector_code,
    #teaching_code_new,
    autism,
    age_cat
  ) 

# Prevalence of autism in Chile dataset
sum(chile_bayes_aut$autism) / nrow(chile_bayes_aut) # 0.00476 = 0.476%, very low

# Is prevalence the same across geographic regions, age, sex?
n_std_pop <- sum(chile_stdpop$std_pop)

aut_prev_all<- chile_bayes_aut %>%
  group_by(school_region_name_abr, age_june30, sex, autism) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = autism, values_from = count) %>%
  rename("n_noautism" = "0", "n_autism" = "1", "age" = "age_june30") %>%
  mutate(n_autism = ifelse(is.na(n_autism), 0, n_autism),
         sample_pop_size = n_noautism + n_autism,
         sample_prevalence = n_autism / sample_pop_size) %>%
  left_join(chile_stdpop, by = c("age", "sex")) %>%
  mutate(aut_prev_std = n_autism / sample_pop_size * pop_prop,
         w = std_pop / (sample_pop_size * n_std_pop),
         w2 = pop_prop / sample_pop_size,
         sum_std_pop = sum(std_pop)) %>%
  ungroup()

ggplot(data = aut_prev_all) +
  geom_col(aes(x = school_region_name_abr, y = sample_prevalence, group = age, fill = as.factor(age)), position = "dodge")
  #geom_col(aes(x = school_region_name_abr, y = prevalence, group = sex, fill = as.factor(sex)), position = "dodge")
    # 1 is male, 2 is female

################################################################################

# Standardise prevalence by Chile's age and sex based population sizes
# using https://seer.cancer.gov/seerstat/WebHelp/Rate_Algorithms.htm 
# and https://wonder.cdc.gov/wonder/help/cancer/fayfeuerconfidenceintervals.pdf 
aut_prev_adjrate <- aut_prev_all %>%
  group_by(school_region_name_abr) %>%
  summarise(sum_sample_pop_size = sum(sample_pop_size),
            crude_rate = sum(n_autism) / sum(sample_pop_size),
            crude_count = sum(n_autism),
            adjusted_rate = sum(n_autism / sample_pop_size * pop_prop),
            adjusted_count = round(adjusted_rate * sum_sample_pop_size, 0), # had to fudge this to get MCMC to run bc it needs integers
            #adjusted_count = adjusted_rate * sum_sample_pop_size,
            var = sum(pop_prop^2 * n_autism / sample_pop_size^2),
            #se2 = sqrt(sum((std_pop/sum(std_pop))^2 * n_autism/sample_pop_size^2)),
            w_M = max(w),
            ci_lower = var / (2*adjusted_rate) * qchisq(p = 0.05/2, df = 2*adjusted_rate^2 / var),
            ci_upper = (var + w_M^2) / (2*(adjusted_rate + w_M)) * qchisq(p = 1-0.05/2, df = 2*(adjusted_rate+w_M)^2 / (var+w_M^2))) %>%
  arrange(school_region_name_abr)

#qchisq(0.3, 2.6)
#aut_prev_adjrate$var / (2*aut_prev_adjrate$adjusted_rate)
#(aut_prev_adjrate$var + aut_prev_adjrate$w_M^2) / 2*(aut_prev_adjrate$adjusted_rate + aut_prev_adjrate$w_M)
#qchisq(p = 0.05/2, df = 2*aut_prev_adjrate$adjusted_rate^2 / aut_prev_adjrate$var)
#qchisq(p = 1-0.05/2, df = 2*(aut_prev_adjrate$adjusted_rate+aut_prev_adjrate$w_M)^2 / (aut_prev_adjrate$var+aut_prev_adjrate$w_M^2))


################################################################################

# Bayesian prevalence analysis - common effects model with sample prevalence

nObs <- nrow(chile_bayes_aut)
nIter <- 1000
nBurn <- 1000

# Start with uniform prior
theta_a <- 1
theta_b <- 1
# This corresponds to a mean of 0.5

common_model <- "model {
  theta ~ dbeta(theta_a, theta_b) # Prior
  aut_sample ~ dbin(theta, nObs) # Prevalence in sample data
  
  aut_pred ~ dbin(theta, nObs) # Predicted prevalence in new sample of same size
  
  
  #spec ~ dnorm(spec_mu, 1/spec_sd) # dnorm requires prevalence not sd or var
  #sens ~ dnorm(sens_mu, 1/sens_sd)
  #aut_post <- aut_sample/nObs * sens + (1 - aut_sample/nObs) * spec
}"

common_data <- list(theta_a = theta_a, 
                    theta_b = theta_b,
                    nObs = nObs,
                    aut_sample = sum(chile_bayes_aut$autism) #,
                    #spec_mu = 0.996,
                    #pec_sd = (1.00-0.99) / (2*1.96),
                    #sens_mu = 0.62,
                    #sens_sd = (0.66-0.57) / (2*1.96)
                    )

common_ini <- list(list(theta = 0.001), #, spec = 0.5, sens = 0.5),
                   list(theta = 0.01)) #, spec = 0.9, sens = 0.9)) 

common_pars <- c("theta_a", "theta_b", "theta", 
                 #"spec", "sens",
                 "aut_sample", "aut_pred")

# Run JAGS model and discard burn-in samples
common_jag <- jags.model(textConnection(common_model),
                         data = common_data,
                         inits = common_ini,
                         n.chains = 2,
                         quiet = TRUE)
update(common_jag, n.iter = nBurn)
common_sam <- coda.samples(model = common_jag,
                           variable.names = common_pars,
                           n.iter = nIter)

# Check for convergence in parameters of interest
mcmc_trace(common_sam, common_pars) # Convergence looks fine and rhats <= 1.1
summary(as_draws(common_sam)) # mean posterior theta is 0.00477
plot(density(extract_variable(common_sam, "theta")), xlim = c(0,0.01))
plot(density(extract_variable(common_sam, "theta")), xlim = c(0.004,0.0055))
# Very very narrow posterior distribution centered approx at sample prevalence of 0.00476.
# Not that surprising given uniform prior was used.



### Try with informative prior

# Say autism has mean prevalence of 3% and we are 95% confidence that the prevalence is between 2% and 4%.
# Then mu = 0.03, sigma = (0.04-0.02) / (2*1.96)
theta_mu <- 0.03
theta_sigma <- (0.04-0.02) / (2*1.96)
theta_a <- theta_mu * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)
theta_b <- (1 - theta_mu) * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)

common_data <- list(theta_a = theta_a, 
                    theta_b = theta_b,
                    nObs = nObs,
                    aut_sample = sum(chile_bayes_aut$autism) #,
                    #spec_mu = 0.996,
                    #pec_sd = (1.00-0.99) / (2*1.96),
                    #sens_mu = 0.62,
                    #sens_sd = (0.66-0.57) / (2*1.96)
)

common_ini <- list(list(theta = 0.001), #, spec = 0.5, sens = 0.5),
                   list(theta = 0.01)) #, spec = 0.9, sens = 0.9)) 

common_pars <- c("theta_a", "theta_b", "theta", 
                 #"spec", "sens",
                 "aut_sample", "aut_pred")

# Run JAGS model and discard burn-in samples
common_jag <- jags.model(textConnection(common_model),
                         data = common_data,
                         inits = common_ini,
                         n.chains = 2,
                         quiet = TRUE)
update(common_jag, n.iter = nBurn)
common_sam <- coda.samples(model = common_jag,
                           variable.names = common_pars,
                           n.iter = nIter)

# Check for convergence in parameters of interest
mcmc_trace(common_sam, common_pars) # Convergence looks fine and rhats <= 1.1
summary(as_draws(common_sam)) # mean posterior theta is still 0.00477
plot(density(extract_variable(common_sam, "theta")), xlim = c(0,0.01))
plot(density(extract_variable(common_sam, "theta")), xlim = c(0.004,0.0055))

# Informative prior made no difference to posterior distribution

################################################################################
# 
# ### Bayesian prevalence analysis - random effect on school region with sample prevalences
# 
# chile_rand_region <- aut_prev_all %>%
#   group_by(school_region_name_abr) %>%
#   summarise(nObs = sum(sample_pop_size),
#             aut_sample = sum(n_autism)) %>%
#   arrange(school_region_name_abr)
# 
# # Try informative prior
# theta_a <- theta_mu * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)
# theta_b <- (1 - theta_mu) * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)
# 
# nRegion <- length(unique(chile_bayes_aut$school_region_name_abr))
# 
# rand_region_model <- "model {
#   for(i in 1:nRegion) { # For each region
#     theta[i] ~ dbeta(theta_a, theta_b)
#     aut_sample[i] ~ dbinom(theta[i], nObs[i])
# 
#     aut_pred[i] ~ dbinom(theta[i], nObs[i])
#   }
# }"
# 
# rand_region_data <- list(theta_a = theta_a, 
#                     theta_b = theta_b,
#                     #nObs = rep(3056300, 16),
#                     #nObs = c(113208, 178136, 57116, 44648, 19858, 270637, 146522, 154827, 165436, 27275, 188430, 83353, 68635, 1165047, 67945, 305227),
#                     nObs = chile_rand_region$nObs,
#                     #nObs = matrix(c(chile_rand_region$nObs), nrow = nRegion),
#                     #aut_sample = rep(sum(chile_bayes_aut$autism), 16),
#                     aut_sample = chile_rand_region$aut_sample,
#                     #aut_sample = matrix(c(chile_rand_region$aut_sample), nrow = nRegion),
#                     nRegion = nRegion)
# 
# #rand_region_ini <- list(list(theta = 0.001), #, spec = 0.5, sens = 0.5),
# #                   list(theta = 0.01)) #, spec = 0.9, sens = 0.9)) 
# 
# rand_region_pars <- c("theta_a", "theta_b", "theta", "aut_sample", "aut_pred")
# 
# # Run JAGS model and discard burn-in samples
# rand_region_jag <- jags.model(textConnection(rand_region_model),
#                          data = rand_region_data,
#                          #inits = rand_region_ini,
#                          n.chains = 2,
#                          quiet = TRUE)
# update(rand_region_jag, n.iter = nBurn)
# rand_region_sam <- coda.samples(model = rand_region_jag,
#                            variable.names = rand_region_pars,
#                            n.iter = nIter)
# 
# # Check for convergence in parameters of interest
# mcmc_trace(rand_region_sam, rand_region_pars) # Convergence looks fine and rhats <= 1.1
# summary(as_draws(rand_region_sam)) %>% print(n = Inf)
# plot(density(extract_variable(rand_region_sam, "theta[1]")), xlim = c(0,0.01))
# 
# # Plot each predicted prevalence distribution (theta[x]) and sample prevalence.
# plot(density(extract_variable(rand_region_sam, "theta[1]")), xlim = c(0, 0.02))
# abline(v = 974/113208, col = "red")
# plot(density(extract_variable(rand_region_sam, "theta[2]")), xlim = c(0, 0.02))
# abline(v = 617/178136, col = "red")
# # etc
# 
# # See Bayesian stats assignment for tidyverse extraction of theta distributions
# rand_region_theta <- as_tibble(as_draws_matrix(rand_region_sam), rownames = "Iteration")
# # Will come back to this
# ### Also need to try random effect on sex and on age, and maybe sex and region together or with age

################################################################################

### Bayesian prevalence analysis - try with prevalence model instead of count model

# Try informative prior
theta_mu <- 0.03
theta_sigma <- (0.04-0.02) / (2*1.96)
theta_a <- theta_mu * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)
theta_b <- (1 - theta_mu) * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)

nRegion <- length(unique(chile_bayes_aut$school_region_name_abr))

rand_region_model <- "model {
  for(i in 1:nRegion) { # For each region
    theta[i] ~ dbeta(theta_a, theta_b)
    aut_sample[i] ~ dbin(theta[i], nObs[i])

    aut_pred[i] ~ dbin(theta[i], nObs[i])
  }
}"

rand_region_data <- list(theta_a = theta_a, 
                         theta_b = theta_b,
                         #nObs = rep(3056300, 16),
                         #nObs = c(113208, 178136, 57116, 44648, 19858, 270637, 146522, 154827, 165436, 27275, 188430, 83353, 68635, 1165047, 67945, 305227),
                         nObs = aut_prev_adjrate$sum_sample_pop_size,
                         #nObs = matrix(c(chile_rand_region$nObs), nrow = nRegion),
                         #aut_sample = rep(sum(chile_bayes_aut$autism), 16),
                         aut_sample = aut_prev_adjrate$adjusted_count,
                         #aut_sample = matrix(c(chile_rand_region$aut_sample), nrow = nRegion),
                         nRegion = nRegion)

#rand_region_ini <- list(list(theta = 0.001), #, spec = 0.5, sens = 0.5),
#                   list(theta = 0.01)) #, spec = 0.9, sens = 0.9)) 

rand_region_pars <- c("theta_a", "theta_b", "theta", "aut_sample", "aut_pred")

# Run JAGS model and discard burn-in samples
rand_region_jag <- jags.model(textConnection(rand_region_model),
                              data = rand_region_data,
                              #inits = rand_region_ini,
                              n.chains = 2,
                              quiet = TRUE)
update(rand_region_jag, n.iter = nBurn)
rand_region_sam <- coda.samples(model = rand_region_jag,
                                variable.names = rand_region_pars,
                                n.iter = nIter)

# Check for convergence in parameters of interest
#mcmc_trace(rand_region_sam, rand_region_pars) 
mcmc_trace(rand_region_sam, paste0("theta[", 1:nRegion, "]")) # Convergence looks fine and rhats <= 1.1
mcmc_trace(rand_region_sam, paste0("aut_pred[", 1:nRegion, "]"))# Convergence looks fine and rhats <= 1.1
summary(as_draws(rand_region_sam)) %>% print(n = Inf)
rand_region_summ <- summary(subset_draws(as_draws(rand_region_sam), common_pars),
                       ~quantile(.x, probs=c(0.025, 0.5, 0.975)),
                       ~mcse_quantile(.x, probs=c(0.025, 0.5, 0.975)),
                       "rhat") %>%
  arrange(desc(mcse_q50))
rand_region_summ

prev_density_plots <- list()
 
for(i in 1:nRegion) {
  prevs <- data.frame(prev = extract_variable(rand_region_sam, paste0("theta[", i, "]")))
  density_plot <- ggplot(prevs, aes(x = prev)) + 
    geom_density() +
    geom_vline(xintercept = aut_prev_adjrate$ci_lower[i], color = "red", linetype = "dashed") +
    geom_vline(xintercept = aut_prev_adjrate$ci_upper[i], color = "red", linetype = "dashed") +
    labs(title = aut_prev_adjrate$school_region_name_abr[i])
  prev_density_plots[[i]] <- density_plot
}
do.call(grid.arrange, prev_density_plots)
#autism_prev_plots <- do.call(grid.arrange, prev_density_plots)
ggsave("autism_prev_plots.png", autism_prev_plots)

################################################################################

# Dumping ground


b1 <- seq(0, 1, by = 0.001)    
by1 <- dbeta(b1, shape1 = 33.5, shape2 = 1083) 
by1 <- dbeta(b1, shape1 = 9.2, shape2 = 13.8) 
plot(by1)


# Set priors for prevalence, sensitivity and specificity of school-based autism assessment
# Assume sensitivity and specificity are normally distributed
# aut_prev <- list(y_sample = count(filter(chile_slim, special_needs_code == "105")),
#                  n_sample = nrow(chile_slim), 
#                  spec_mu = 0.996, # from Barbaro et al
#                  spec_sd = (1.00-0.99) / (2*1.96), # from Barbaro et al, CI for spec is 0.99-1.00 (Joseph et al used 0.067 for survey and 0.004 for admin)
#                  sens_mu = 0.620, # from Barbaro et al, for SACS-R (excluding SACS-PR)
#                  sens_sd = (0.66-0.57) / (2*1.96), # from Barbaro et al, CI for sens is 0.57-0.66 (Joseph et al used 0.020 for survey and 0.020 for admin)
#                  p_mu = 0.5, # parameters of beta-proportion distribution, from Joseph et al
#                  p_kappa = 2 # ditto, kappa must be strictly positive
#                  #theta_a = 0.5 * 2, # a = mu * kappa
#                  #theta_b = (1-0.5) * 2 # b = (1-mu) * kappa
# )

#model <- stan_model("Autism-diagnosis-age-ML/single_test.stan")



