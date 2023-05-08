# Adele Tyson
# Started 5/5/2023
# Import and explore data from Chile schools 2021

setwd("C:/Users/delat/OneDrive/MPhil Population Health Sciences 2022-2023/12 Dissertation/04_Data")

library(janitor)
library(Hmisc)
library(tidyverse)

chile_raw <- read.csv("School Census Chile.csv") %>%
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
         course_letter = let_cur, # not sure, Lucy asking her chilean
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
  

# Need to make lots of the columns into factors because they are categorical not numeric variables

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
