# Adele Tyson, agmt3@cam.ac.uk
# 5/5/2023
# Explore features of chile data, check for correlation, look at features associated with autism diagnosis.

# Load the data and environment setup
source("Autism-diagnosis-age-ML/Chile.R")


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

