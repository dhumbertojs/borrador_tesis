rm(list = ls())
setwd("~")

library(dplyr)
library(tidyr)
library(Hmisc)

inp <- "/Users/dhjs/Documents/projects/electoral_accountability"
list.files(inp)

data <- read.csv(paste(inp, "final.csv", sep = "/"))
##Hay 10,230 observaciones

#Estas son las variables originales
data2 <- data %>% 
  select(
    muniYear, state, muni, year, wintop_state, win_top, inc_top, conco,        
    inc.ch, IM, POB_TOT, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, alt, edo.year     
  )

tabla <- data2 %>% 
  select(
    IM, POB_TOT,
    inc.ch, ch.agua, ch.dren, ch.elec, ch.del, ch.hom     
  ) %>% 
  rename(POB = POB_TOT) %>% 
  summarise_all(funs(mean, sd, min, max), na.rm = T) %>% 
  pivot_longer(everything()) %>% 
  separate(name, into = c("variable", "stat"), sep = "_") %>% 
  mutate(value = round(value, 3)) %>% 
  pivot_wider(
    names_from = stat
  )


corr <- data2 %>% 
  select(
    alt, inc.ch, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, IM, POB_TOT
  ) %>% 
  rename(pob = POB_TOT, im = IM)

rcorr(as.matrix(corr), type = "pearson")

##el otro calculo

# data3 <- data %>% 
#   select(
#     muniYear, state, muni, year, wintop_state, win_top, inc_top, conco,        
#     inc.ch2, IM, POB_TOT, ch.agua2, ch.dren2, ch.elec2, ch.del2, ch.hom2, alt, edo.year     
#   )

tabla <- data %>% 
  select(
    IM, POB_TOT,
    inc.ch, ch.agua, ch.dren, ch.elec, ch.del, ch.hom     
  ) %>% 
  rename(POB = POB_TOT) %>% 
  summarise_all(funs(mean, sd, min, max), na.rm = T) %>% 
  pivot_longer(everything()) %>% 
  separate(name, into = c("variable", "stat"), sep = "_") %>% 
  mutate(value = round(value, 3))


corr <- data3 %>% 
  select(
    alt, inc.ch2, ch.agua2, ch.dren2, ch.elec2, ch.del2, ch.hom2, IM, POB_TOT
  ) %>% 
  rename(pob = POB_TOT, im = IM)

rcorr(as.matrix(corr), type = "pearson")