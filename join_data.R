rm(list = ls())
setwd("~")

library(dplyr)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/databases"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability"

cri <- read.csv(paste(inp, "Crimes.csv", sep = "/"))
ele <- read.csv(paste(inp, "Electoral.csv", sep = "/"))
mar <- read.csv(paste(inp, "margination.csv", sep = "/"))
ser <- read.csv(paste(inp, "Services.csv", sep = "/"))

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

# fin <- full_join(ele, ser, by = "muniYear")
# fin <- full_join(fin, cri, by = "muniYear")
# fin <- full_join(fin, mar, by = "muniYear")
# #64,710 observaciones

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
  group_by(muni) %>% 
  mutate(
    lt.agua = lag(t.agua, n = 1),
    lt.dren = lag(t.dren, n = 1),
    lt.elec = lag(t.elec, n = 1), 
    lt.del = lag(t.del, n = 1),
    lt.hom = lag(hom, n = 1),
    
    ch.agua = ifelse(!is.na(lt.agua) & !is.na(t.agua), (t.agua - lt.agua) * 100/t.agua, NA),
    ch.dren = ifelse(!is.na(lt.dren) & !is.na(t.dren), (t.dren - lt.dren) * 100/t.dren, NA),
    ch.elec = ifelse(!is.na(lt.elec) & !is.na(t.elec), (t.elec - lt.elec) * 100/t.elec, NA),
    ch.del = ifelse(!is.na(lt.del) & !is.na(t.del), (t.del - lt.del) * 100/t.del, NA),
    ch.hom = ifelse(!is.na(lt.hom) & !is.na(t.hom), (t.hom - lt.hom) * 100/t.hom, NA)
  )  %>% 
 filter(!is.infinite(ch.agua) & !is.infinite(ch.dren) & !is.infinite(ch.elec) & 
          !is.infinite(ch.del) & !is.infinite(ch.hom)) %>% 
 ungroup() %>% 
  select(-c(agua, dren, elec, tot_del, hom, 
            t.agua, t.dren, t.elec, t.del, t.hom, 
            lt.agua, lt.dren, lt.elec, lt.del, lt.hom)) %>% 
  filter(win_top != "Otros") %>% 
  mutate(alt = ifelse(as.character(win_top) != as.character(inc_top), 1, 0))
nrow(try)
##Con esta transformación me quedan 10,587 observaciones
#despues del filter son 10,230

write.csv(try, paste(out, "final.csv", sep = "/"), row.names = F)