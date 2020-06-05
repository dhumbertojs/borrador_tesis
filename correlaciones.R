rm(list = ls())
setwd("~")

library(dplyr)
library(tidyr)
library(Hmisc)
library(stargazer)

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
  # summarise_all(funs(mean, sd, min, max), na.rm = T) %>% 
  # pivot_longer(everything()) %>% 
  # separate(name, into = c("variable", "stat"), sep = "_") %>% 
  # mutate(value = round(value, 3)) %>% 
  # pivot_wider(
  #   names_from = stat
  # )
  stargazer(
    type = "html",
    title = "Estadística descriptiva",
    column.labels = c("holi", "Observaciones", "Promedio",
                      "Desviación estándar", "Mínimo", "Máximo"),
    covariate.labels = c("Índice de marginación",
                         "Población", 
                         "Cambio de porcentaje de votos al Incumbent",
                         "Cambio porcentual de tomas de agua",
                         "Cambio porcentual de tomas de drenaje",
                         "Cambio porcentual de tomas de electricidad",
                         "Cambio porcentual de carpetas de delitos",
                         "Cambio porcentual de carpetas de homicidios"),
    omit.summary.stat = c("p25", "p75"),
    digits = 2,
    out = paste(inp, "Descriptivos_total.html", sep = "/")
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

# tabla <- data %>% 
#   select(
#     IM, POB_TOT,
#     inc.ch, ch.agua, ch.dren, ch.elec, ch.del, ch.hom     
#   ) %>% 
#   rename(POB = POB_TOT) %>% 
#   summarise_all(funs(mean, sd, min, max), na.rm = T) %>% 
#   pivot_longer(everything()) %>% 
#   separate(name, into = c("variable", "stat"), sep = "_") %>% 
#   mutate(value = round(value, 3))


# corr <- data3 %>% 
#   select(
#     alt, inc.ch2, ch.agua2, ch.dren2, ch.elec2, ch.del2, ch.hom2, IM, POB_TOT
#   ) %>% 
#   rename(pob = POB_TOT, im = IM)
# 
# rcorr(as.matrix(corr), type = "pearson")

data3 <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= quantile(ch.agua, 0.975, na.rm = T) & 
                       ch.agua >= quantile(ch.agua, 0.025, na.rm = T), ch.agua, NA),
    
    ch.dren = ifelse(ch.dren <= quantile(ch.dren, 0.975, na.rm = T) &
                       ch.dren >= quantile(ch.dren, 0.025, na.rm = T) , ch.dren, NA),
    
    ch.elec = ifelse(ch.elec <= quantile(ch.elec, 0.975, na.rm = T) &
                       ch.elec >= quantile(ch.elec, 0.025, na.rm = T), ch.elec, NA),
    
    ch.del = ifelse(ch.del <= quantile(ch.del, 0.975, na.rm = T) &
                      ch.del >= quantile(ch.del, 0.025, na.rm = T), ch.del, NA),
    
    ch.hom = ifelse(ch.hom <= quantile(ch.hom, 0.975, na.rm = T) & 
                      ch.hom >= quantile(ch.hom, 0.025, na.rm = T), ch.hom, NA)
  )%>% 
  select(
    muniYear, state, muni, year, wintop_state, win_top, inc_top, conco,        
    inc.ch, IM, POB_TOT, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, alt, edo.year     
  )

tabla2 <- data3 %>% 
  select(
    IM, POB_TOT,
    inc.ch, ch.agua, ch.dren, ch.elec, ch.del, ch.hom     
  ) %>% 
  rename(POB = POB_TOT) %>% 
  # summarise_all(funs(mean, sd, min, max), na.rm = T) %>% 
  # pivot_longer(everything()) %>% 
  # separate(name, into = c("variable", "stat"), sep = "_") %>% 
  # mutate(value = round(value, 3)) %>% 
  # pivot_wider(
  #   names_from = stat
  # )
  stargazer(
    type = "html",
    title = "Estadística descriptiva",
    column.labels = c("holi", "Observaciones", "Promedio",
                      "Desviación estándar", "Mínimo", "Máximo"),
    covariate.labels = c("Índice de marginación",
                         "Población", 
                         "Cambio de porcentaje de votos al Incumbent",
                         "Cambio porcentual de tomas de agua",
                         "Cambio porcentual de tomas de drenaje",
                         "Cambio porcentual de tomas de electricidad",
                         "Cambio porcentual de carpetas de delitos",
                         "Cambio porcentual de carpetas de homicidios"),
    omit.summary.stat = c("p25", "p75"),
    digits = 2,
    out = paste(inp, "Descriptivos_95%.html", sep = "/")
  )

corr2 <- data3 %>% 
  select(
    alt, inc.ch, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, IM, POB_TOT
  ) %>% 
  rename(pob = POB_TOT, im = IM)

rcorr(as.matrix(corr2), type = "pearson")