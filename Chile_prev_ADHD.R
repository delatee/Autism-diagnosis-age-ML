# Adele Tyson, agmt3@cam.ac.uk
# 22/5/2023
# Bayesian prevalence analysis of adhd prevalence in Chile and by school's region

# Load the data and environment setup
source("autdism-diagnosis-age-ML/Chile.R")

chile_stdpop_raw <- read_excel("04_Data/pop_chile_2021_single_age.xlsx") %>%
  clean_names() 

chile_stdpop <- chile_stdpop_raw %>%
  filter(sex != 9) %>%
  rename("std_pop" = "pop_2021") %>%
  mutate(pop_prop = std_pop / sum(std_pop))



################################################################################

# Try Bayesian analysis of adhd prevalence and specificity and sensitivity of school assessment
# "Bayesian Estimation of Disease Prevalence and the Parameters of Diagnostic Tests in the Absence of a Gold Standard"
# Lawrence Joseph, Theresa W. Gyorkos, Louis Coupal
# https://www.cambridge.org/core/journals/epidemiology-and-psychiatric-sciences/article/bayesian-approach-to-estimating-the-population-prevalence-of-mood-and-anxiety-disorders-using-multiple-measures/DB1D2CA6C27C7E8C85C60B62B969BB72

# Use sensitivity and specificity of Social Attention and Communication Surveillance–Revised (SACS-R) tool
# "Diagnostic Accuracy of the Social Attention and Communication Surveillance–Revised With Preschool Tool for Early adhd Detection in Very Young Children"
# Josephine Barbaro, Nancy Sadka, Melissa Gilbert, et al
# https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2789926

chile_bayes_adhd <- chile %>%
  filter(age_june30 >= 6 & age_june30 <= 18,
         sex != 0) %>%
  mutate(adhd = ifelse(special_needs_code == 203, 1, 0),
         age_cat = ifelse(age_june30 <= 8, 1, ifelse(age_june30 <= 11, 2, ifelse(age_june30 <= 14, 3, 4)))) %>% 
          # 1 = 6-8, 2 = 9-11, 3 = 12-14, 4 = 15-18
  select(school_region_name_abr,
         sex,
         age_june30,
         #age_cat,
         adhd) 

# Prevalence of adhd in Chile dataset
sum(chile_bayes_adhd$adhd) / nrow(chile_bayes_adhd) # 0.00476 = 0.476%, very low

# Is prevalence the same across geographic regions, age, sex?
n_std_pop <- sum(chile_stdpop$std_pop)

adhd_prev_all<- chile_bayes_adhd %>%
  group_by(school_region_name_abr, age_june30, sex, adhd) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = adhd, values_from = count) %>%
  rename("n_noadhd" = "0", "n_adhd" = "1", "age" = "age_june30") %>%
  mutate(n_adhd = ifelse(is.na(n_adhd), 0, n_adhd),
         sample_pop_size = n_noadhd + n_adhd,
         sample_prevalence = n_adhd / sample_pop_size) %>%
  left_join(chile_stdpop, by = c("age", "sex")) %>%
  mutate(adhd_prev_std = n_adhd / sample_pop_size * pop_prop,
         w = std_pop / (sample_pop_size * n_std_pop),
         w2 = pop_prop / sample_pop_size,
         sum_std_pop = sum(std_pop)) %>%
  ungroup()

ggplot(data = adhd_prev_all) +
  geom_col(aes(x = school_region_name_abr, y = sample_prevalence, group = age, fill = as.factor(age)), position = "dodge")
  #geom_col(aes(x = school_region_name_abr, y = prevalence, group = sex, fill = as.factor(sex)), position = "dodge")
    # 1 is male, 2 is female

################################################################################

# Standardise prevalence by Chile's age and sex based population sizes
# using https://seer.cancer.gov/seerstat/WebHelp/Rate_Algorithms.htm 
# and https://wonder.cdc.gov/wonder/help/cancer/fayfeuerconfidenceintervals.pdf 
adhd_prev_adjrate <- adhd_prev_all %>%
  group_by(school_region_name_abr) %>%
  summarise(sum_sample_pop_size = sum(sample_pop_size),
            crude_rate = sum(n_adhd) / sum(sample_pop_size),
            crude_count = sum(n_adhd),
            adjusted_rate = sum(n_adhd / sample_pop_size * pop_prop),
            adjusted_count = round(adjusted_rate * sum_sample_pop_size, 0), # had to fudge this to get MCMC to run bc it needs integers
            #adjusted_count = adjusted_rate * sum_sample_pop_size,
            var = sum(pop_prop^2 * n_adhd / sample_pop_size^2),
            #se2 = sqrt(sum((std_pop/sum(std_pop))^2 * n_adhd/sample_pop_size^2)),
            w_M = max(w),
            ci_lower = var / (2*adjusted_rate) * qchisq(p = 0.05/2, df = 2*adjusted_rate^2 / var),
            ci_upper = (var + w_M^2) / (2*(adjusted_rate + w_M)) * qchisq(p = 1-0.05/2, df = 2*(adjusted_rate+w_M)^2 / (var+w_M^2))) %>%
  arrange(school_region_name_abr)

################################################################################

# Bayesian prevalence analysis - common effects model with sample prevalence

nObs <- nrow(chile_bayes_adhd)
nIter <- 1000
nBurn <- 1000

### Try with informative prior

# Say adhd has mean prevalence of 5% and we are 95% confidence that the prevalence is between 4% and 6%.
# Then mu = 0.05, sigma = (0.06-0.04) / (2*1.96)
theta_mu <- 0.05
theta_sigma <- (0.06-0.04) / (2*1.96)
theta_a <- theta_mu * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)
theta_b <- (1 - theta_mu) * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)

common_model <- "model {
  theta ~ dbeta(theta_a, theta_b) # Prior
  adhd_sample ~ dbin(theta, nObs) # Prevalence in sample data
  adhd_pred ~ dbin(theta, nObs) # Predicted prevalence in new sample of same size
}"

common_data <- list(theta_a = theta_a, 
                    theta_b = theta_b,
                    nObs = nObs,
                    adhd_sample = sum(chile_bayes_adhd$adhd) #,
                    #spec_mu = 0.996,
                    #pec_sd = (1.00-0.99) / (2*1.96),
                    #sens_mu = 0.62,
                    #sens_sd = (0.66-0.57) / (2*1.96)
)

common_ini <- list(list(theta = 0.001), #, spec = 0.5, sens = 0.5),
                   list(theta = 0.01)) #, spec = 0.9, sens = 0.9)) 

common_pars <- c("theta_a", "theta_b", "theta", 
                 #"spec", "sens",
                 "adhd_sample", "adhd_pred")

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
plot(density(extract_variable(common_sam, "theta")), xlim = c(0,0.02))
plot(density(extract_variable(common_sam, "theta")), xlim = c(0.014,0.016))

# Informative prior made no difference to posterior distribution

################################################################################

### Bayesian prevalence analysis - try with prevalence model instead of count model

# Try informative prior
theta_a <- theta_mu * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)
theta_b <- (1 - theta_mu) * (theta_mu * (1-theta_mu) / theta_sigma^2 - 1)

nRegion <- length(unique(chile_bayes_adhd$school_region_name_abr))

rand_region_model <- "model {
  for(i in 1:nRegion) { # For each region
    theta[i] ~ dbeta(theta_a, theta_b)
    adhd_sample[i] ~ dbin(theta[i], nObs[i])

    adhd_pred[i] ~ dbin(theta[i], nObs[i])
  }
}"

rand_region_data <- list(theta_a = theta_a, 
                         theta_b = theta_b,
                         #nObs = rep(3056300, 16),
                         #nObs = c(113208, 178136, 57116, 44648, 19858, 270637, 146522, 154827, 165436, 27275, 188430, 83353, 68635, 1165047, 67945, 305227),
                         nObs = adhd_prev_adjrate$sum_sample_pop_size,
                         #nObs = matrix(c(chile_rand_region$nObs), nrow = nRegion),
                         #adhd_sample = rep(sum(chile_bayes_adhd$adhd), 16),
                         adhd_sample = adhd_prev_adjrate$adjusted_count,
                         #adhd_sample = matrix(c(chile_rand_region$adhd_sample), nrow = nRegion),
                         nRegion = nRegion)

#rand_region_ini <- list(list(theta = 0.001), #, spec = 0.5, sens = 0.5),
#                   list(theta = 0.01)) #, spec = 0.9, sens = 0.9)) 

rand_region_pars <- c("theta_a", "theta_b", "theta", "adhd_sample", "adhd_pred")

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
mcmc_trace(rand_region_sam, paste0("adhd_pred[", 1:nRegion, "]"))# Convergence looks fine and rhats <= 1.1
summary(as_draws(rand_region_sam)) %>% print(n = Inf)
rand_region_summ <- summary(subset_draws(as_draws(rand_region_sam), common_pars),
                       ~quantile(.x, probs=c(0.025, 0.5, 0.975)),
                       ~mcse_quantile(.x, probs=c(0.025, 0.5, 0.975)),
                       "rhat") %>%
  #arrange(desc(mcse_q50))
  arrange(desc(rhat))
rand_region_summ

prev_density_plots <- list()
 
for(i in 1:nRegion) {
  prevs <- data.frame(prev = extract_variable(rand_region_sam, paste0("theta[", i, "]")))
  density_plot <- ggplot(prevs, aes(x = prev)) + 
    geom_density() +
    geom_vline(xintercept = adhd_prev_adjrate$ci_lower[i], color = "red", linetype = "dashed") +
    geom_vline(xintercept = adhd_prev_adjrate$ci_upper[i], color = "red", linetype = "dashed") +
    labs(title = adhd_prev_adjrate$school_region_name_abr[i])
  prev_density_plots[[i]] <- density_plot
}
adhd_prev_plots <- do.call(grid.arrange, prev_density_plots)
ggsave("adhd_prev_plots.png", adhd_prev_plots, height = 10, width = 15)

################################################################################
