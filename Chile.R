# Adele Tyson
# Started 5/5/2023
# Set up R environment and import and clean data from Chile schools 2021

library(nleqslv) # Only needed for robince bayesian prevalence
library(janitor)
library(readxl)
library(psych)
library(Hmisc)
library(poolr)
library(epitools)
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


chile_merged_raw <- read.csv("04_Data/Data_Chile_Merge.csv")



