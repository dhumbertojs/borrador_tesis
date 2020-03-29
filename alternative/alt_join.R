rm(list = ls())
setwd("~")

library(dplyr)
library(stringr)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/databases"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/regression/alternative"

cri <- read.csv(paste(inp, "Crimes.csv", sep = "/"))
cri <- cri %>% 
  mutate(
    municipio = sapply(strsplit(as.character(muniYear), "_"), "[", 1),
    year = as.numeric(sapply(strsplit(as.character(muniYear), "_"), "[", 2))
  ) %>% 
  group_by(municipio) %>% 
  mutate(
    tot_del_lag = lag(tot_del, n = 1, order_by = year),
    hom_lag = lag(hom, n = 1, order_by = year)
  ) %>% 
  ungroup() %>% 
  select(-c(municipio, year))
ele <- read.csv(paste(inp, "Electoral.csv", sep = "/"))
mar <- read.csv(paste(inp, "margination.csv", sep = "/"))
ser <- read.csv(paste(inp, "Services.csv", sep = "/"))
ser <- ser %>% 
  mutate(
    municipio = sapply(strsplit(as.character(muniYear), "_"), "[", 1),
    year = as.numeric(sapply(strsplit(as.character(muniYear), "_"), "[", 2))
  ) %>% 
  group_by(municipio) %>% 
  mutate(
    agua_lag = lag(agua, n = 1, order_by = year),
    dren_lag = lag(dren, n = 1, order_by = year),
    elec_lag = lag(elec, n = 1, order_by = year)
  ) %>% 
  ungroup() %>% 
  select(-c(municipio, year))

summary(ele)##Estos son los datos electorales 1980-2015
summary(ser) #Datos de servicios 1994-2015
#hay 13,464 NA en agua
#17,094 NA en drenaje
#19,402 NA en electricidad
summary(cri)#Datos de carpetas de investigacion 1994-2010
#En total de delitos hay 7,994 NA
#En homicidios hay 10,100 NA
summary(mar)#Datos de marginación y población de 1990-2015
#En el Indice de Marginación hay 3,285 NA
#En poblacion hay 500 NA

ele <- ele %>% 
  filter(year >= 1994)

try <- ele %>% 
  left_join(ser, by = "muniYear") %>% 
  left_join(cri, by = "muniYear") %>% 
#13,373
  left_join(mar, by = "muniYear")
#11,142 observaciones

try <- try %>% 
  group_by(muni) %>% 
  mutate(
    lag_pob = lag(POB_TOT, n = 1, order_by = year)
  ) %>% 
ungroup() %>% 
  mutate(
    t.agua = (agua/POB_TOT) * 100000,
    t.dren = (dren/POB_TOT) * 100000,
    t.elec = (elec/POB_TOT) * 100000,
    t.del = (tot_del/POB_TOT) * 100000,
    t.hom = (hom/POB_TOT) * 100000
  ) %>% 
  mutate(
    lt.agua = (agua_lag/lag_pob) * 100000,
    lt.dren = (dren_lag/lag_pob) * 100000,
    lt.elec = (elec_lag/lag_pob) * 100000, 
    lt.del = (tot_del_lag/lag_pob) * 100000,
    lt.hom = (hom_lag/lag_pob) * 100000,
    #Valor presente - valor pasado / valor pasado
    
    ch.agua = t.agua - lt.agua,
    ch.dren = t.dren - lt.dren,
    ch.elec = t.elec - lt.elec,
    ch.del = t.del - lt.del,
    ch.hom = t.hom - lt.hom
    
  ) %>% 
  mutate(
    alt = ifelse(as.character(win_top) != as.character(inc_top), 1, 0),
    state = str_pad(state, width = 2, side = "left", pad = "0"),
    edo.year = paste(state, year, sep = "_")
  )
nrow(try)

write.csv(try, paste(out, "final.csv", sep = "/"), row.names = F)