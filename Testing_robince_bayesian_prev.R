# Copied method and bayesprev.R from https://github.com/robince/bayesian-prevalence/blob/master/R/example_csv.R

#setwd("C:/Users/delat/OneDrive/MPhil Population Health Sciences 2022-2023/12 Dissertation")

# Load the required functions
source("Autism-diagnosis-age-ML/robince_bayesian_prev/bayesprev.R")

# Load the data
# Define prevalence of autism as binary value for each student in right age range 
# where 1 indicates the within-unit null hypothesis was rejected and 0 indicates it was not rejected
chile_aut_prev <- chile %>%
  filter(age_june30 >= 6 & age_june30 <= 18,
         #special_needs_status == 1,
         sex != 0) %>%
  mutate(autism = ifelse(special_needs_code == 105, 1, 0)) %>%
  select(autism)

chile_aut_prev_NUBLE <- chile %>%
  filter(age_june30 >= 6 & age_june30 <= 18,
         school_region_name_abr == "NUBLE",
         #special_needs_status == 1,
         sex != 0) %>%
  mutate(autism = ifelse(special_needs_code == 105, 1, 0)) %>%
  select(autism)
  

# Set modelling parameters
alpha <- 0.05 # alpha value used for the within-unit tests, probably don't need this as I'm only doing between-unit tests
Ntests <- nrow(chile_aut_prev_NUBLE) # number of students
Ntests <- nrow(sigdat)
Nsigtests <- sum(chile_aut_prev_NUBLE) # number of significant tests, ie number of students with autism
Nsigtests <- sum(sigdat)
  
# Plot the posteriorpdf for the population prevalence proportion
xvals <- seq(0, 1, .01)
pdf <- bayesprev_posterior(x = 0.01, k = 15, n = 3056)
pdf <- bayesprev_posterior(x = xvals, k = Nsigtests, n = Ntests)
plot(xvals, pdf, type ="l", xlab = expression(gamma), ylab ="Posterior density", lwd=3)
  
  # 0.95 lower bound for the population prevalence proportion
  
  b = bayesprev_bound(0.05, Nsigtests, Ntests)
  print(b)
  
  # MAP estimate of the population prevalence proportion
  
  m = bayesprev_map(Nsigtests, Ntests)
  print(m)
  
  # 96% HPDI for the population prevalence proportion
  
  int = bayesprev_hpdi(0.96, Nsigtests, Ntests)
  print(int)
  
  
  
  # Example of possible Bayesian prevalence analyses on a common plot.
  

  xvals <- seq(0, 1, .01)
  pdf <- bayesprev_posterior(xvals, Nsigtests, Ntests)
  plot(xvals, pdf, type ="l", xlab = expression(gamma), ylab ="Posterior density", lwd=3)

  
  # Add the MAP estimate as a point
  
  xmap = bayesprev_map(Nsigtests, Ntests)
  pmap = bayesprev_posterior(xmap, Nsigtests, Ntests)
  points(xmap, pmap, cex =2, col= "red", pch=16)
  
  # Add the .95 lower bound as a vertical line
  
  xbound = bayesprev_bound(0.05, Nsigtests, Ntests)
  pbound = bayesprev_posterior(xbound, Nsigtests, Ntests)
  lines(c(xbound, xbound), c(0, pbound+0.5), col="blue", lwd=3)
  
  
  # Add the 0.96 HPDI
  
  int = bayesprev_hpdi(0.96, Nsigtests, Ntests)
  i1 = int[1]
  i2 = int[2]
  h1 = bayesprev_posterior(i1, Nsigtests, Ntests)
  h2 = bayesprev_posterior(i2, Nsigtests, Ntests)
  lines(c(i1, i2), c(h1, h2), col ="green", lwd=3)
  
  
###############################
  
# See if my way gets the same results on their data as their way
  
nObs <- nrow(sigdat)
nIter <- 1000
nBurn <- 1000
  
common_model <- "model {
  theta ~ dbeta(theta_a, theta_b)
  aut_sample ~ dbin(theta, nObs)
  
  aut_pred ~ dbin(theta, nObs)
}"
  
common_data <- list(theta_a = 0.05, 
                    theta_b = 1,
                    nObs = nObs,
                    aut_sample = sum(sigdat))
  
#common_ini <- list(list(theta = 0.001, spec = 0.5, sens = 0.5),
#                   list(theta = 0.01, spec = 0.9, sens = 0.9)) 
  
common_pars <- c("theta_a", "theta_b", "theta", "aut_pred")
  
# Run JAGS model and discard burn-in samples
common_jag <- jags.model(textConnection(common_model),
                         data = common_data,
                         #inits = common_ini,
                         n.chains = 2,
                         quiet = TRUE)
update(common_jag, n.iter = nBurn)
common_sam <- coda.samples(model = common_jag,
                           variable.names = common_pars,
                           n.iter = nIter)
  
# Check for convergence in parameters of interest
mcmc_trace(common_sam, common_pars)
  
summary(as_draws(common_sam))

plot(density(extract_variable(common_sam, "theta")), xlim = c(0,1)) # Huzzar!
 

  
  
  
  
