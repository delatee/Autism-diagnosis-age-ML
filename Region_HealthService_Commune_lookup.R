
# Combine communes, regions and health services into one lookup table

library(writexl)
library(tidyverse)

# Get level 3 ie commune boundaries
chile.adm2 <- st_read("04_Data/CHL_adm_humdata/chl_admbnda_adm2_bcn_20211008.shp")
chile.adm3 <- st_read("04_Data/CHL_adm_humdata/chl_admbnda_adm3_bcn_20211008.shp") %>%
  mutate(commune_code = str_sub(ADM3_PCODE, start = 3, end = -1),
         commune_name = ADM3_ES)

chile_communes_raw <- read_excel("04_Data/commune_by_health_service.xlsx") %>%
  clean_names()

# Get communes with health services
chile_communes <- chile_communes_raw %>%
  mutate(commune_name = ifelse(comuna == "La Calera", "Calera",
                        ifelse(comuna == "Coihaique", "Coyhaique",
                        ifelse(comuna == "Paiguano", "Paihuano",
                        ifelse(comuna == "Pedro Aguirre Cerda", "Pedro Aguirre Cerda",
                        comuna))))) %>%
rename("health_service_name" = "servicio_de_salud") %>%
  select(commune_name, health_service_name)

# Create a number for each health service
health_service_lookup <- chile_communes %>%
  group_by(health_service_name) %>%
  summarise() %>% 
  arrange(health_service_name) %>%
  rowid_to_column("health_service_code")

# Get region codes with region name abreviations
region_abr_lookup <- data.frame(
  ADM1_PCODE = c(paste0("CL0", c(1:9)), paste0("CL", c(10:16))),
  region_name_abr = c("TPCA", "ANTOF", "ATCMA", "COQ", "VALPO", "LGBO", "MAULE", "BBIO",
                      "ARAUC", "LAGOS", "AYSEN", "MAG", "RM", "RIOS", "AYP", "NUBLE"))

# Put region, health service and communes together
region_service_commune_lookup <- chile.adm3 %>%
  merge(chile_communes, by = "commune_name", all.x = TRUE) %>%
  mutate(health_service_name = 
           ifelse(ADM3_PCODE == "CL11201", "Servicio de Salud Aisén",
           ifelse(commune_name == "Isla de Pascua", "Servicio de Salud Metropolitano Oriente",
           ifelse(commune_name == "Los Alamos", "Servicio de Salud Arauco",
           ifelse(commune_name == "Los Angeles", "Servicio de Salud Biobío",
           ifelse(ADM3_PCODE == "CL06204", "Servicio de Salud Del Libertador B.O'Higgins",
           ifelse(commune_name == "Pedro Aguirre Cerda", "Servicio de Salud Metropolitano Sur",
           ifelse(commune_name == "Ranquil", "Servicio de Salud Ñuble", health_service_name))))))),
         region_code = str_sub(ADM1_PCODE, start = 3, end = -1),
         commune_name_upper = toupper(commune_name)) %>%
  rename(region_name = ADM1_ES) %>%
  filter(commune_code != "01401") %>% # Just remove the Tocopilla duplicate because we probably won't need to map this bit. Could instead merge the polygons
  merge(region_abr_lookup, by = "ADM1_PCODE")
  
region_service_commune_lookup <-merge(region_service_commune_lookup, 
                                      health_service_lookup,
                                       by = "health_service_name", all = TRUE) %>%
  rename(region_name_long = region_name,
         health_service_name_long = health_service_name) %>%
  mutate(region_name = ifelse(grepl("Región de ", region_name_long), 
                              substr(region_name_long, start = 11, stop = nchar(region_name_long)), 
                       ifelse(grepl("Región del ", region_name_long), 
                              substr(region_name_long, start = 12, stop = nchar(region_name_long)),
                       ifelse(region_name_long == "Región Metropolitana", "Metropolitana", NA))),
         health_service_name = ifelse(grepl(" Del ", health_service_name_long), 
                                      str_sub(health_service_name_long, start = 23, end = -1), 
                                      str_sub(health_service_name_long, start = 19, end = -1))) %>%
  select(region_name_long, region_name, region_name_abr, region_code, 
         commune_name, commune_name_upper, commune_code, 
         health_service_name_long, health_service_name, health_service_code,
         geometry)
# We lost commune Antartica but we don't need it because there is no school data for it.
# Can't de-select geometry column here but that doesn't matter
write_xlsx(region_service_commune_lookup, path = "04_Data/Outputs/region_service_commune.xlsx")
