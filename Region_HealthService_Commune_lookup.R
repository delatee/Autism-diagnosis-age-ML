
# Combine communes, regions and health services into one lookup table

library(writexl)
library(tidyverse)

chile.adm2 <- st_read("04_Data/CHL_adm_humdata/chl_admbnda_adm2_bcn_20211008.shp")
chile.adm3 <- st_read("04_Data/CHL_adm_humdata/chl_admbnda_adm3_bcn_20211008.shp") %>%
  mutate(commune_code = str_sub(ADM3_PCODE, start = 3, end = -1),
         commune_name = ADM3_ES)

chile_communes_raw <- read_excel("04_Data/commune_by_health_service.xlsx") %>%
  clean_names()

chile_communes <- chile_communes_raw %>%
  #mutate(comuna_upper = toupper(comuna)) %>%
  # mutate(commune_name = ifelse(comuna_upper == "AISÉN", "AYSÉN",
  #                                     ifelse(comuna_upper == "LA CALERA", "CALERA",
  #                                            ifelse(comuna_upper == "COIHAIQUE", "COYHAIQUE",
  #                                                   ifelse(comuna == "Isla de Pascua", "ISLA DE PASCUA",
  #                                                          ifelse(comuna_upper == "MAULLÍN" , "MAULLIN",
  #                                                                 ifelse(comuna == "Pedro Aguirre Cerda", "PEDRO AGUIRRE CERDA",
  #                                                                        ifelse(comuna_upper == "RÁNQUIL", "RANQUIL",
  #                                                                               ifelse(comuna_upper == "TREGUACO", "TREHUACO",
  #                                                                                      ifelse(comuna_upper == "VICHUQUÉN", "VICHUQUEN", comuna_upper)))))))
  #                                     ))) %>%
  mutate(commune_name = #ifelse(comuna == "Aisén", "Aysén", # Doesn't work
                        ifelse(comuna == "La Calera", "Calera",
                        ifelse(comuna == "Coihaique", "Coyhaique",
                        #ifelse(comuna == "Isla de Pascua", "Isla de Pascua",
                        #ifelse(comuna == "Los Álamos" , "Los Alamos", # Doesn't work
                        #ifelse(comuna == "Los Ángeles", "Los Angeles", # Doesn't work
                        #ifelse(comuna == "Marchihue", "Marchigüe",
                        ifelse(comuna == "Paiguano", "Paihuano",
                        ifelse(comuna == "Pedro Aguirre Cerda", "Pedro Aguirre Cerda",
                        #ifelse(comuna == "Ránquil", "Ranquil",
                        comuna))))) %>%
rename("health_service_name" = "servicio_de_salud") %>%
  select(commune_name, health_service_name)


# chile_merged_raw <- read.csv("04_Data/Data_Chile_Merge.csv") %>% clean_names()
# 
# chile_merged <- chile_merged_raw %>%
#   rename(commune_code = cod_com_rbd,
#          region_name = nom_reg_rbd_a,
#          commune_name = nom_com_rbd) %>%
#   group_by(region_name, commune_code, commune_name) %>%
#   summarise()

region_service_commune_lookup <- chile.adm3 %>%
  merge(chile_communes, by = "commune_name", all.x = TRUE) %>%
  mutate(health_service_name = ifelse(ADM3_PCODE == "CL11201", "Servicio de Salud Aisén",
                               ifelse(commune_name == "Isla de Pascua", "Servicio de Salud Metropolitano Oriente",
                               ifelse(commune_name == "Los Alamos", "Servicio de Salud Arauco",
                               ifelse(commune_name == "Los Angeles", "Servicio de Salud Biobío",
                               ifelse(ADM3_PCODE == "CL06204", "Servicio de Salud Del Libertador B.O'Higgins",
                               ifelse(commune_name == "Pedro Aguirre Cerda", "Servicio de Salud Metropolitano Sur",
                               ifelse(commune_name == "Ranquil", "Servicio de Salud Ñuble", 
                                      health_service_name))))))),
         region_code = str_sub(ADM1_PCODE, start = 3, end = -1),
         commune_name_upper = toupper(commune_name)) %>%
  rename(region_name = ADM1_ES) %>%
  select(region_name, region_code, 
         commune_name, commune_name_upper, commune_code, 
         health_service_name,
         geometry)
# We lost commune Antartica but we don't need it because there is no school data for it.
# Can't de-select geometry column here but that doesn't matter

# Create a number for each health service
health_service_lookup <- chile_communes %>%
  group_by(health_service_name) %>%
  summarise() %>% 
  arrange(health_service_name) %>%
  rowid_to_column("health_service_code") %>%
  rename(health_service_name_long = health_service_name) %>%
  mutate(health_service_name = str_sub(health_service_name_long, start = 19, end = -1))


region_service_commune_lookup <- merge(region_service_commune_lookup, health_service_lookup,
                                       by = "health_service_name", all = TRUE) %>%
  relocate(health_service_name, .before = health_service_code)


write_xlsx(region_service_commune_lookup, path = "04_Data/Outputs/region_service_commune.xlsx")
