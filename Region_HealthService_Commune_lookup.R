
# Combine communes, regions and health services into one lookup table

library(writexl)
library(tidyverse)

chile_communes_raw <- read_excel("04_Data/commune_by_health_service.xlsx") %>%
  clean_names()

chile_communes <- chile_communes_raw %>%
  mutate(comuna_upper = toupper(comuna)) %>%
  mutate(commune_name = ifelse(comuna_upper == "AISÉN", "AYSÉN",
                                      ifelse(comuna_upper == "LA CALERA", "CALERA", 
                                             ifelse(comuna_upper == "COIHAIQUE", "COYHAIQUE",
                                                    ifelse(comuna == "Isla de Pascua", "ISLA DE PASCUA", 
                                                           ifelse(comuna_upper == "MAULLÍN" , "MAULLIN",
                                                                  ifelse(comuna == "Pedro Aguirre Cerda", "PEDRO AGUIRRE CERDA", 
                                                                         ifelse(comuna_upper == "RÁNQUIL", "RANQUIL",
                                                                                ifelse(comuna_upper == "TREGUACO", "TREHUACO",
                                                                                       ifelse(comuna_upper == "VICHUQUÉN", "VICHUQUEN", comuna_upper)))))))
                                      ))) %>%
  rename("health_service_name" = "servicio_de_salud") %>%
  select(commune_name, health_service_name)


chile_merged_raw <- read.csv("04_Data/Data_Chile_Merge.csv") %>% clean_names()

chile_merged <- chile_merged_raw %>%
  rename(region_name = nom_reg_rbd_a,
         commune_name = nom_com_rbd) %>%
  group_by(region_name, commune_name) %>%
  summarise()

region_service_commune_lookup <- chile_merged %>%
  merge(chile_communes, by = "commune_name", all = TRUE) %>%
  mutate(region_name = ifelse(commune_name == "ANTÁRTICA", "MAG", region_name))

write_xlsx(region_service_commune_lookup, path = "04_Data/Outputs/region_service_commune.xlsx")
