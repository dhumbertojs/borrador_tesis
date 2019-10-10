rm(list = ls())
setwd("~")

library(dplyr)
library(readxl)
library(tidyr)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/datos/bienes"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/databases"

data <- read_excel(paste(inp, "SIMBAD_19046_20191010012052836.xlsx", sep = "/"), skip = 3)

clave <- select(data, 1:2)

d1994 <- data %>% select(3:4) %>% 
  mutate(year = 1994) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...3`, 
         hom = `Homicidio...4`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1995 <- data %>% select(5:6) %>% 
  mutate(year = 1995) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...5`, 
         hom = `Homicidio...6`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1996 <- data %>% select(7:8) %>% 
  mutate(year = 1996) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...7`, 
         hom = `Homicidio...8`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1997 <- data %>% select(9:10) %>% 
  mutate(year = 1997) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...9`, 
         hom = `Homicidio...10`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1998 <- data %>% select(11:12) %>% 
  mutate(year = 1998) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...11`, 
         hom = `Homicidio...12`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1999 <- data %>% select(13:14) %>% 
  mutate(year = 1999) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...13`, 
         hom = `Homicidio...14`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2000 <- data %>% select(15:16) %>% 
  mutate(year = 2000) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...15`, 
         hom = `Homicidio...16`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2001 <- data %>% select(17:18) %>% 
  mutate(year = 2001) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...17`, 
         hom = `Homicidio...18`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2002 <- data %>% select(19:20) %>% 
  mutate(year = 2002) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...19`, 
         hom = `Homicidio...20`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2003 <- data %>% select(21:22) %>% 
  mutate(year = 2003) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...21`, 
         hom = `Homicidio...22`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2004 <- data %>% select(23:24) %>% 
  mutate(year = 2004) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...23`, 
         hom = `Homicidio...24`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2005 <- data %>% select(25:26) %>% 
  mutate(year = 2005) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...25`, 
         hom = `Homicidio...26`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2006 <- data %>% select(27:28) %>% 
  mutate(year = 2006) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...27`, 
         hom = `Homicidio...28`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2007 <- data %>% select(29:30) %>% 
  mutate(year = 2007) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...29`, 
         hom = `Homicidio...30`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2008 <- data %>% select(31:32) %>% 
  mutate(year = 2008) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...31`, 
         hom = `Homicidio...32`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2009 <- data %>% select(33:34) %>% 
  mutate(year = 2009) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...33`, 
         hom = `Homicidio...34`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2010 <- data %>% select(35:36) %>% 
  mutate(year = 2010) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos...35`, 
         hom = `Homicidio...36`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )

fin <- bind_rows(list(d1994, d1995, d1996, d1997, d1998, d1999, d2000,
                      d2001, d2002, d2003, d2004, d2005, d2006, d2007, 
                      d2008, d2009, d2010))

fin <- fin %>% 
  select(Clave, year, tot_del, hom) %>% 
  mutate(
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0)))))
  ) %>% filter(edos != 1 & quit != 1) %>%
  select(-c("edos", "quit"))

fin <- fin %>% 
  mutate(
    tot_del = ifelse(tot_del == "ND", NA, tot_del),
    tot_del = as.numeric(tot_del),
    hom = ifelse(hom == "ND", NA, hom),
    hom = as.numeric(hom),
    
    muniYear = paste(Clave, year, sep = "_")
  ) %>% 
  arrange(muniYear) %>% 
  select(muniYear, tot_del, hom)

summary(fin)

##De los datos originales hay muchso NA
#En total de delitos hay 7,994
#En homicidios hay 10,100

write.csv(fin, paste(out, "Crimes.csv", sep = "/"), row.names = F)