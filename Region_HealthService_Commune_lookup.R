
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
  mutate(commune_name = ifelse(comuna == "Aisén", "Aysén",
                        ifelse(comuna == "La Calera", "Calera",
                        ifelse(comuna == "Coihaique", "Coyhaique",
                        #ifelse(comuna == "Isla de Pascua", "Isla de Pascua",
                        ifelse(comuna == "Los Álamos" , "Los Alamos",
                        ifelse(comuna == "Los Ángeles", "Los Angeles",
                        ifelse(comuna == "Marchihue", "Marchigüe",
                        ifelse(comuna == "Paiguano", "Paihuano",
                        ifelse(comuna == "Pedro Aguirre Cerda", "Pedro Aguirre Cerda",
                        ifelse(comuna == "Ránquil", "Ranquil", comuna)))))))))) %>%
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
  merge(chile_communes, by = "commune_name", all = TRUE) #%>%
  #mutate(region_name = ifelse(commune_name == "ANTÁRTICA", "MAG", region_name))

write_xlsx(region_service_commune_lookup, path = "04_Data/Outputs/region_service_commune.xlsx")
