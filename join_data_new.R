library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

inp <- "/Users/dhjs/Documents/projects/electoral_accountability/databases"
list.files(inp)
out <- "/Users/dhjs/Documents/projects/electoral_accountability"

cri <- read.csv(paste(inp, "homicidios.csv", sep = "/")) %>% 
  rename(hom = homicidios) %>% 
  mutate(muni = sapply(strsplit(muniYear, "_"), "[", 1)) %>% 
  group_by(muni) %>% 
  fill(hom) %>% 
  ungroup() %>% 
  select(-3)
ele <- read.csv(paste(inp, "Electoral.csv", sep = "/")) #%>% 
# mutate(muni = str_pad(muni, width = 5, side = "left", pad = "0"))
mar <- read.csv(paste(inp, "margination.csv", sep = "/")) %>% 
  rename(year = AÑO, muni = CVE_MUN) %>% 
  select(-POB_TOT) %>% 
  mutate(muni = str_pad(muni, width = 5, side = "left", pad = "0"))
pob <- read.csv(paste(inp, "poblation.csv", sep = "/"))
ser <- read.csv(paste(inp, "Services_original.csv", sep = "/")) %>% 
  mutate(muni = sapply(strsplit(muniYear, "_"), "[", 1)) %>% 
  group_by(muni) %>% 
  fill(c(agua, dren, elec), .direction = "down") %>% 
  ungroup() %>% 
  select(-5)

muni <- levels(as.factor(mar$muni))
year <- 1990:2015
years <- data.frame(year)
years <- filter(years, !year %in% c(1990, 1995, 2000, 2005, 2010, 2015))
im <- map(years$year, 
          ~ bind_cols(.x, muni))
im <- bind_rows(im) %>% 
  rename(year = ...1, muni = ...2) %>% 
  mutate(muniYear = paste(muni, year, sep = "_"))
mar <- bind_rows(mar, im) %>% 
  arrange(muniYear) %>% 
  fill(IM) %>% 
  select(-c(muni, year))

# #64,710 observaciones
nrow(ele)#27,292
ele <- filter(ele, year >= 1994)
nrow(ele)#17,023

try <- ele %>% 
  left_join(ser, by = "muniYear") %>% 
  left_join(cri, by = "muniYear") %>% 
  #13,373
  left_join(mar, by = "muniYear") %>% 
  #11,142 observaciones
  left_join(pob, by = "muniYear")

try <- try %>% 
  mutate(
    inc.ch = inc.ch*100,
    t.agua = (agua/POB_TOT) * 100000,
    t.dren = (dren/POB_TOT) * 100000,
    t.elec = (elec/POB_TOT) * 100000,
    #t.del = (tot_del/POB_TOT) * 100000,
    t.hom = (hom/POB_TOT) * 100000
  ) %>% 
  group_by(muni) %>% 
  mutate(
    lt.agua = lag(t.agua, n = 1, order_by = year),
    lt.dren = lag(t.dren, n = 1, order_by = year),
    lt.elec = lag(t.elec, n = 1, order_by = year), 
    #lt.del = lag(t.del, n = 1, order_by = year),
    lt.hom = lag(t.hom, n = 1, order_by = year),
    
    #Valor presente - valor pasado / valor pasado
    
    ch.agua = ifelse(!is.na(lt.agua) & !is.na(t.agua), ((t.agua - lt.agua) * 100)/lt.agua, NA),
    ch.dren = ifelse(!is.na(lt.dren) & !is.na(t.dren), ((t.dren - lt.dren) * 100)/lt.dren, NA),
    ch.elec = ifelse(!is.na(lt.elec) & !is.na(t.elec), ((t.elec - lt.elec) * 100)/lt.elec, NA),
    # ch.del = ifelse(!is.na(lt.del) & !is.na(t.del), ((t.del - lt.del) * 100)/lt.del, NA),
    # ch.hom = ifelse(!is.na(lt.hom) & !is.na(t.hom), ((t.hom - lt.hom) * 100)/lt.hom, NA),
    
    # ch.agua1 = t.agua - lt.agua,
    # ch.dren1 = t.dren - lt.dren,
    # ch.elec1 = t.elec - lt.elec,
    #ch.del = t.del - lt.del,
    ch.hom = t.hom - lt.hom,
    
    ch.agua = ifelse(is.infinite(ch.agua), NA, ch.agua),
    ch.dren = ifelse(is.infinite(ch.dren), NA, ch.dren),
    ch.elec = ifelse(is.infinite(ch.elec), NA, ch.elec)#,
    # ch.del = ifelse(is.infinite(ch.del), NA, ch.del),
    # ch.hom = ifelse(is.infinite(ch.hom), NA, ch.hom)
    
  )  %>% 
  ungroup() %>% 
  mutate(
    state = str_pad(state, width = 2, side = "left", pad = "0"),
    edo.year = paste(state, year, sep = "_")
  )
nrow(try)
##Con esta transformación me quedan 10,587 observaciones
#despues del filter son 10,230

write.csv(try, paste(out, "final_original.csv", sep = "/"), row.names = F)
