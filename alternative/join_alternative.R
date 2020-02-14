#Alternative models

rm(list = ls())
setwd("~")

library(dplyr)
library(stringr)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/databases"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/alternative"

cri <- read.csv(paste(inp, "Crimes.csv", sep = "/"))
cri <- cri %>% 
  mutate(
    municipio = sapply(strsplit(as.character(muniYear), "_"), "[", 1)
  ) %>% 
  group_by(municipio) %>% 
  mutate(
    tot_del_lag = lag(tot_del, n = 1),
    hom_lag = lag(hom, n = 1)
  ) %>% 
  ungroup() %>% 
  select(-municipio)
ele <- read.csv(paste(inp, "Electoral.csv", sep = "/"))
mar <- read.csv(paste(inp, "margination.csv", sep = "/"))
ser <- read.csv(paste(inp, "Services.csv", sep = "/"))
ser <- ser %>% 
  mutate(
    municipio = sapply(strsplit(as.character(muniYear), "_"), "[", 1)
  ) %>% 
  group_by(municipio) %>% 
  mutate(
    agua_lag = lag(agua, n = 1),
    dren_lag = lag(dren, n = 1),
    elec_lag = lag(elec, n = 1)
         ) %>% 
  ungroup() %>% 
  select(-municipio)

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

try <- inner_join(ele, ser, by = "muniYear")
try <- left_join(try, cri, by = "muniYear")
#13,373
try <- inner_join(try, mar, by = "muniYear")
#11,142 observaciones

try <- try %>% 
  mutate(
    t.agua = (agua/POB_TOT) * 100000,
    t.dren = (dren/POB_TOT) * 100000,
    t.elec = (elec/POB_TOT) * 100000,
    t.del = (tot_del/POB_TOT) * 100000,
    t.hom = (hom/POB_TOT) * 100000
  ) %>% 
  mutate(
    lt.agua = (agua_lag/POB_TOT) * 100000,
    lt.dren = (dren_lag/POB_TOT) * 100000,
    lt.elec = (elec_lag/POB_TOT) * 100000, 
    lt.del = (tot_del_lag/POB_TOT) * 100000,
    lt.hom = (hom_lag/POB_TOT) * 100000,
    #Valor presente - valor pasado / valor pasado
    
    ch.agua = ifelse(!is.na(lt.agua) & !is.na(t.agua), ((t.agua - lt.agua) * 100)/lt.agua, NA),
    ch.dren = ifelse(!is.na(lt.dren) & !is.na(t.dren), ((t.dren - lt.dren) * 100)/lt.dren, NA),
    ch.elec = ifelse(!is.na(lt.elec) & !is.na(t.elec), ((t.elec - lt.elec) * 100)/lt.elec, NA),
    ch.del = ifelse(!is.na(lt.del) & !is.na(t.del), ((t.del - lt.del) * 100)/lt.del, NA),
    ch.hom = ifelse(!is.na(lt.hom) & !is.na(t.hom), ((t.hom - lt.hom) * 100)/lt.hom, NA)
  )  %>% 
  filter(!is.infinite(ch.agua) & !is.infinite(ch.dren) & !is.infinite(ch.elec) & 
           !is.infinite(ch.del) & !is.infinite(ch.hom)) %>% 
  select(-c(agua, dren, elec, tot_del, hom, 
            t.agua, t.dren, t.elec, t.del, t.hom, 
            lt.agua, lt.dren, lt.elec, lt.del, lt.hom)) %>% 
  filter(win_top != "Otros") %>% 
  mutate(
    alt = ifelse(as.character(win_top) != as.character(inc_top), 1, 0),
    state = str_pad(state, width = 2, side = "left", pad = "0"),
    edo.year = paste(state, year, sep = "_")
  )
nrow(try)

write.csv(try, paste(out, "final.csv", sep = "/"), row.names = F)