rm(list = ls())
setwd("~")

library(dplyr)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/datos"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/databases"

data <- read.csv(paste(inp, "data.csv", sep = "/"))

data <- data %>% 
  mutate(
    inc_top = ifelse(inc == "PRI", "PRI",
                     ifelse(inc == "PAN", "PAN",
                            ifelse(inc == "PRD", "PRD", 
                                   ifelse(is.na(inc), NA, "Otros")))),
    PAN.s = PAN/total,
    PRI.s = PRI/total,
    PRD.s = PRD/total,
    
    inc.share = ifelse(inc_top == "PAN", PAN.s,
                       ifelse(inc_top == "PRI", PRI.s,
                              ifelse(inc_top == "PRD", PRD.s, NA))),
    inc.share = (inc.share * 100)
  ) %>% 
  filter(inc_top != "Otros" & !is.na(Winner2))

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    inc.share.lag = lag(inc.share, n = 1),
    inc.ch = inc.share - inc.share.lag
  ) %>% 
  ungroup()

##20,725 observaciones: sin NA de Oaxaca, sin tomar en cuenta "Otros" partidos" 
write.csv(data, paste(out, "Electoral.csv", sep = "/"), row.names = F)