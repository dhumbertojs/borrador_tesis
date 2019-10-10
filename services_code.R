rm(list = ls())
setwd("~")

library(dplyr)
library(readxl)
library(tidyr)
library(janitor)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/datos/servicios"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/databases"

data <- read_excel(paste(inp, "SIMBAD_45106_20191009115505598.xlsx", sep = "/"), skip = 3)

clave <- select(data, 1:2)

d1994 <- data %>% select(3: 5) %>% 
  mutate(year = 1994) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...3`, 
         dren = `Sistemas de drenaje y alcantarillado a/...4`, 
         elec = `Tomas instaladas de energía eléctrica a/...5`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1995 <- data %>% select(6: 8) %>% 
  mutate(year = 1995) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...6`, 
         dren = `Sistemas de drenaje y alcantarillado a/...7`, 
         elec = `Tomas instaladas de energía eléctrica a/...8`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1996 <- data %>% select(9:11) %>% 
  mutate(year = 1996) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...9`, 
         dren = `Sistemas de drenaje y alcantarillado a/...10`, 
         elec = `Tomas instaladas de energía eléctrica a/...11`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1997 <- data %>% select(12:14) %>% 
  mutate(year = 1997) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...12`, 
         dren = `Sistemas de drenaje y alcantarillado a/...13`, 
         elec = `Tomas instaladas de energía eléctrica a/...14`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1998 <- data %>% select(15:17) %>% 
  mutate(year = 1998) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...15`, 
         dren = `Sistemas de drenaje y alcantarillado a/...16`, 
         elec = `Tomas instaladas de energía eléctrica a/...17`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1999 <- data %>% select(18:20) %>% 
  mutate(year = 1999) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...18`, 
         dren = `Sistemas de drenaje y alcantarillado a/...19`, 
         elec = `Tomas instaladas de energía eléctrica a/...20`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2000 <- data %>% select(21:23) %>% 
  mutate(year = 2000) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...21`, 
         dren = `Sistemas de drenaje y alcantarillado a/...22`, 
         elec = `Tomas instaladas de energía eléctrica a/...23`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2001 <- data %>% select(24:26) %>% 
  mutate(year = 2001) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...24`, 
         dren = `Sistemas de drenaje y alcantarillado a/...25`, 
         elec = `Tomas instaladas de energía eléctrica a/...26`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2002 <- data %>% select(27:29) %>% 
  mutate(year = 2002) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...27`, 
         dren = `Sistemas de drenaje y alcantarillado a/...28`, 
         elec = `Tomas instaladas de energía eléctrica a/...29`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2003 <- data %>% select(30:32) %>% 
  mutate(year = 2003) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...30`, 
         dren = `Sistemas de drenaje y alcantarillado a/...31`, 
         elec = `Tomas instaladas de energía eléctrica a/...32`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2004 <- data %>% select(33:35) %>% 
  mutate(year = 2004) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...33`, 
         dren = `Sistemas de drenaje y alcantarillado a/...34`, 
         elec = `Tomas instaladas de energía eléctrica a/...35`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2005 <- data %>% select(36:38) %>% 
  mutate(year = 2005) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...36`, 
         dren = `Sistemas de drenaje y alcantarillado a/...37`, 
         elec = `Tomas instaladas de energía eléctrica a/...38`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2006 <- data %>% select(39:41) %>% 
  mutate(year = 2006) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...39`, 
         dren = `Sistemas de drenaje y alcantarillado a/...40`, 
         elec = `Tomas instaladas de energía eléctrica a/...41`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2007 <- data %>% select(42:44) %>% 
  mutate(year = 2007) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...42`, 
         dren = `Sistemas de drenaje y alcantarillado a/...43`, 
         elec = `Tomas instaladas de energía eléctrica a/...44`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2008 <- data %>% select(45:47) %>% 
  mutate(year = 2008) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...45`, 
         dren = `Sistemas de drenaje y alcantarillado a/...46`, 
         elec = `Tomas instaladas de energía eléctrica a/...47`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2009 <- data %>% select(48:50) %>% 
  mutate(year = 2009) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...48`, 
         dren = `Sistemas de drenaje y alcantarillado a/...49`, 
         elec = `Tomas instaladas de energía eléctrica a/...50`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2010 <- data %>% select(51:53) %>% 
  mutate(year = 2010) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...51`, 
         dren = `Sistemas de drenaje y alcantarillado a/...52`, 
         elec = `Tomas instaladas de energía eléctrica a/...53`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2011 <- data %>% select(54:56) %>% 
  mutate(year = 2011) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...54`, 
         dren = `Sistemas de drenaje y alcantarillado a/...55`, 
         elec = `Tomas instaladas de energía eléctrica a/...56`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2012 <- data %>% select(57:59) %>% 
  mutate(year = 2012) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...57`, 
         dren = `Sistemas de drenaje y alcantarillado a/...58`, 
         elec = `Tomas instaladas de energía eléctrica a/...59`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2013 <- data %>% select(60:62) %>% 
  mutate(year = 2013) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...60`, 
         dren = `Sistemas de drenaje y alcantarillado a/...61`, 
         elec = `Tomas instaladas de energía eléctrica a/...62`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2014 <- data %>% select(63:65) %>% 
  mutate(year = 2014) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...63`, 
         dren = `Sistemas de drenaje y alcantarillado a/...64`, 
         elec = `Tomas instaladas de energía eléctrica a/...65`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2015 <- data %>% select(66:68) %>% 
  mutate(year = 2015) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/...66`, 
         dren = `Sistemas de drenaje y alcantarillado a/...67`, 
         elec = `Tomas instaladas de energía eléctrica a/...68`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )

fin <- bind_rows(list(d1994, d1995, d1996, d1997, d1998, d1999, d2000,
                      d2001, d2002, d2003, d2004, d2005, d2006, d2007,
                      d2008, d2009, d2010, d2011, d2012, d2013, d2014, d2015))

fin <- fin %>% 
  select(Clave, year, agua, dren, elec) %>% 
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
    agua = ifelse(agua == "ND", NA, agua),
    agua = as.numeric(agua),
    dren = ifelse(dren == "ND", NA, dren),
    dren = as.numeric(dren),
    elec = ifelse(elec == "ND", NA, elec),
    elec = as.numeric(elec),
    
    muniYear = paste(Clave, year, sep = "_")
  ) %>% 
  arrange(muniYear) %>% 
  select(muniYear, agua, dren, elec)

summary(fin)

##De los datos originales
#Hay 13,464 NA de agua
#17,094 NA de drenaje
#19,402 NA de electricidad

#Revisando los datos, un mismo municipio no reporta todos los datos para todos los años
write.csv(fin, paste(out, "Services.csv", sep = "/"), row.names = F)