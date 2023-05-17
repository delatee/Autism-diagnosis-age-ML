# Adele Tyson
# Started 5/5/2023
# Import and explore data from Chile schools 2021

library(janitor)
library(psych)
library(Hmisc)
library(corrplot)
library(caret)
library(mltools)
library(ggrepel)
library(rjags)
library(rstan)
library(posterior)
library(tidybayes)
library(bayesplot)
library(tidyverse)

setwd("C:/Users/delat/OneDrive/MPhil Population Health Sciences 2022-2023/12 Dissertation")

chile_raw <- read.csv("04_Data/School Census Chile.csv") %>%
  clean_names() 

chile <- chile_raw %>%
  rename(year = agno,
         school_code = rbd,
         school_check_code = dgv_rbd, 
         school_name = nom_rbd,
         school_region_code = cod_reg_rbd,
         school_region_name_abr = nom_reg_rbd_a,
         school_province_code = cod_pro_rbd,
         school_commune_code = cod_com_rbd,
         school_commune_name = nom_com_rbd,
         school_dept_code = cod_deprov_rbd,
         school_dept_name = nom_deprov_rbd,
         school_dependency_code = cod_depe, # has categories 1-6, no1 and no2 here are no1 in grouped
         school_dependency_code_grouped = cod_depe2, # has categories 1-5
         school_rurality_code = rural_rbd,
         school_operation_status = estado_estab, 
         teaching_code1 = cod_ense, # min = 10, max = 910, eg preschool, special education hearing impaired
         teaching_code2 = cod_ense2, # subject matter coding, 1-8
         teaching_code3 = cod_ense3, # age based coding, 1-7
         grade_code1 = cod_grado, # grade of schooling, 1-10, 21-25, 31-34, nests in teaching_code1
         grade_code2 = cod_grado2, # equivalent grade of schooling for adult special education, 1-8, 99
         grade_letter = let_cur, # refers to the class within the grade, close to start of alphabet is higher aptitude
         course_timing = cod_jor, # time of day, morning, afternoon, both, night, no info
         course_type = cod_tip_cur, # 0 = simple course, 1-4 = combined course, 99 = no info
         course_descr = cod_des_cur, # Description of course (TP secondary education only). 0: Does not apply, 1: Only High School, 2: Dual, 3: Other
         student_id = mrun,
         sex = gen_alu, # 0 = no info, 1 = male, 2 = female
         dob = fec_nac_alu,
         age_june30 = edad_alu, # age at 30th June 2021
         special_needs_status = int_alu, # integrated student indicator, 0 = no, 1 = yes. Mostly no
         special_needs_code = cod_int_alu, # ADHD, blindness, etc. 0 = none
         student_region_code = cod_reg_alu,
         student_commune_code = cod_com_alu,
         student_commune_name = nom_com_alu,
         economic_sector_code = cod_sec,
         economic_specialty_code = cod_espe,
         economic_branch_code = cod_rama,
         economic_profspec_code = cod_men,
         teaching_code_new = ens) 

# Explore data
describe(chile)

table(chile$school_region_code, chile$school_region_name)
table(chile$school_commune_name, chile$school_dept_name)
sum(chile$school_province_code != chile$school_dept_code)

ggplot(chile, aes(school_check_code)) + geom_bar()
ggplot(chile, aes(school_region_name_abr)) + geom_bar()
sort(unique(chile$school_province_code)) ; length(unique(chile$school_province_code))
ggplot(chile, aes(school_province_code)) + geom_bar()
# Most students are in Region Metropolitan (RM)

ggplot(chile, aes(school_dependency_code)) + geom_bar()
ggplot(chile, aes(school_dependency_code_grouped)) + geom_bar()

ggplot(chile, aes(school_rurality_code)) + geom_bar() # Most schools are urban
ggplot(chile, aes(school_operation_status)) + geom_bar() # Nearly all schools are still open 
chile %>% filter(school_operation_status == 2) # 1 student at school on break, code = 11575
chile %>% filter(school_code == 11575) # they're the only student at that school
chile %>% filter(school_operation_status == 3) %>% print(print_max = Inf) # 28 students at a closed school, code = 2153
chile %>% filter(school_code == 2153) # they're the only students at that school

sort(unique(chile$teaching_code1))
sort(unique(chile$teaching_code2))
sort(unique(chile$teaching_code3))

sort(unique(chile$grade_code1))
sort(unique(chile$grade_cod2))
sort(unique(chile$course_letter))

ggplot(chile, aes(dob)) + geom_bar()
ggplot(chile, aes(age_june30)) + geom_bar()

table(chile$teaching_code1, chile$ens)
table(chile$teaching_code2, chile$ens)
table(chile$teaching_code3, chile$ens)

################################################################################


chile_slim <- chile %>%
  filter(age_june30 >= 6 & age_june30 <= 18,
        special_needs_status == 1,
        sex != 0) %>%
  select(school_code,
         school_region_code,
         school_rurality_code,
         #teaching_code1,
         grade_code1,
         #grade_letter, # Isn't numeric so leave it out for now so cor() works
         #student_id,
         sex,
         #age_june30,
         #special_needs_status,
         special_needs_code,
         student_region_code,
         #economic_sector_code,
         teaching_code_new) %>%
   mutate(#school_code = factor(school_code), # maybe don't need to make into factors because cor() needs numeric
  #        school_region_code = factor(school_region_code),
  #        school_rurality_code = factor(school_rurality_code),
  #        grade_code1 = factor(grade_code1),
  #        #grade_letter = factor(grade_letter),
  #        sex = factor(sex),
  #        special_needs_status = factor(special_needs_status),
          special_needs_code = factor(special_needs_code),
  #        student_region_code = factor(student_region_code),
  #        economic_sector_code = factor(economic_sector_code),
  #        teaching_code_new = factor(teaching_code_new)
     )
         
#chile_slim <- dummyVars("~ special_needs_code", chile_slim)
special_needs_code_dummy <- dummyVars(" ~ special_needs_code", data = chile_slim)
chile_slim_sn <- data.frame(cbind(chile_slim, predict(special_needs_code_dummy, newdata = chile_slim))) %>%
  rename(sn_autism = special_needs_code.105) %>%
  select(-special_needs_code)
#chile_slim <- pivot_wider(chile_slim, names_from = special_needs_code, values_from = special_needs_status)
# Need to get the special_needs_codes into separate columns

Hmisc::describe(chile_slim_sn)

chile_slim_autism <- chile_slim %>% 
  filter(special_needs_code == 105) %>%
  select(-special_needs_code)

################################################################################

# Using all sn categories together isn't really legit
slim_cor <- cor(chile_slim_sn, use = "pairwise")
slim_cor
corrplot(slim_cor)
slim_eigen <- eigen(slim_cor)
slim_eigen$values
scree(slim_cor) # , factors = FALSE) # PC is principle components, FA is factors
# Only keep the factors that have eigenvalue above 1. Will use PC results for now https://www.researchgate.net/post/Both-PC-and-FA-in-scree-plot-which-to-use-in-an-EFA
# There are 4 such factors.

slim_efa <- fa(chile_slim_sn, nfactors = 10)
slim_efa


# Just autism is more legit
slim_cor_autism <- cor(chile_slim_autism, use = "pairwise")
slim_cor_autism
corrplot(slim_cor_autism)
slim_eigen_autism <- eigen(slim_cor_autism)
slim_eigen_autism$values
scree(slim_cor_autism) # , factors = FALSE) # PC is principle components, FA is factors
# Only keep the factors that have eigenvalue above 1. Will use PC results for now https://www.researchgate.net/post/Both-PC-and-FA-in-scree-plot-which-to-use-in-an-EFA
# There are 4 such factors.

slim_efa_autism <- fa(chile_slim_autism, nfactors = 4)
slim_efa_autism

# Need to compare factors for autism to factors for no autism

# Could do same for ADHD



################################################################################

chile_slim_log <- chile %>%
  filter(age_june30 >= 6 & age_june30 <= 18) %>% #,
         #special_needs_status == 1,
         #sex != 0) %>%
  select(#school_code,
         school_region_code,
         school_rurality_code,
         teaching_code1,
         grade_code1,
         grade_letter, 
         #student_id,
         sex,
         age_june30,
         #special_needs_status,
         special_needs_code,
         student_region_code,
         #economic_sector_code,
         #teaching_code_new
         ) %>%
  mutate(#school_code = factor(school_code), # maybe don't need to make into factors because cor() needs numeric
            school_region_code = factor(school_region_code),
            school_rurality_code = factor(school_rurality_code),
            grade_code1 = factor(grade_code1),
            teaching_code1 = factor(teaching_code1),
            #grade_letter = factor(grade_letter),
            sex = factor(sex),
            #special_needs_status = factor(special_needs_status),
            #special_needs_code = factor(special_needs_code),
            student_region_code = factor(student_region_code),
    #        economic_sector_code = factor(economic_sector_code),
            #teaching_code_new = factor(teaching_code_new)
            autism = ifelse(special_needs_code == 105, 1, 0)
  ) %>%
  select(-special_needs_code)

log_fit <- glm(autism ~ ., data = chile_slim_log)
summary(log_fit)
# School region code matters more than student region code, both have significant values
# Sex is not significant
# Probably need to do something different with age, add a random effect on age?

################################################################################

# Try Bayesian analysis of autism prevalence and specificity and sensitivity of school assessment
# "Bayesian Estimation of Disease Prevalence and the Parameters of Diagnostic Tests in the Absence of a Gold Standard"
# Lawrence Joseph, Theresa W. Gyorkos, Louis Coupal
# https://www.cambridge.org/core/journals/epidemiology-and-psychiatric-sciences/article/bayesian-approach-to-estimating-the-population-prevalence-of-mood-and-anxiety-disorders-using-multiple-measures/DB1D2CA6C27C7E8C85C60B62B969BB72

# Use sensitivity and specificity of Social Attention and Communication Surveillance–Revised (SACS-R) tool
# "Diagnostic Accuracy of the Social Attention and Communication Surveillance–Revised With Preschool Tool for Early Autism Detection in Very Young Children"
# Josephine Barbaro, Nancy Sadka, Melissa Gilbert, et al
# https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2789926



# Set priors for prevalence, sensitivity and specificity of school-based autism assessment
# Assume sensitivity and specificity are normally distributed
aut_prev <- list(y_sample = count(filter(chile_slim, special_needs_code == "105")),
                 n_sample = nrow(chile_slim), 
                 spec_mu = 0.996, # from Barbaro et al
                 spec_sd = (1.00-0.99) / (2*1.96), # from Barbaro et al, CI for spec is 0.99-1.00 (Joseph et al used 0.067 for survey and 0.004 for admin)
                 sens_mu = 0.620, # from Barbaro et al, for SACS-R (excluding SACS-PR)
                 sens_sd = (0.66-0.57) / (2*1.96), # from Barbaro et al, CI for sens is 0.57-0.66 (Joseph et al used 0.020 for survey and 0.020 for admin)
                 p_mu = 0.5, # parameters of beta-proportion distribution, from Joseph et al
                 p_kappa = 2 # ditto, kappa must be strictly positive
                 #p_a = 0.5 * 2, # a = mu * kappa
                 #p_b = (1-0.5) * 2 # b = (1-mu) * kappa
)

model <- stan_model("Autism-diagnosis-age-ML/single_test.stan")


