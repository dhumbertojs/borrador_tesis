rm(list = ls())
setwd("~")

library(dplyr)
library(stringr)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/datos"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/databases"

data <- read.csv(paste(inp, "data.csv", sep = "/"))

data <- data %>% 
  select(-c(PRI_ofi, PAN_ofi, PRD_ofi, win, win_top, conco))
  

data <- data %>% 
  mutate(
    win = as.character(Winner2),
    win = sapply(strsplit(win, "_"), "[", 1),
    win_top = ifelse(win == "PRI", "PRI",
                     ifelse(win == "PAN", "PAN",
                            ifelse(win == "PRD", "PRD", 
                                   ifelse(is.na(win), NA, "Otros"))))
  ) %>% 
  group_by(muni) %>% 
  mutate(
    inc = lag(win, n = 1, order_by = year)
  ) %>% 
  ungroup()

data <- data %>% 
  mutate(
    inc_top = ifelse(inc == "PRI", "PRI",
                     ifelse(inc == "PAN", "PAN",
                            ifelse(inc == "PRD", "PRD", 
                                   ifelse(is.na(inc), NA, "Otros")))),
    PAN.s = PAN/total,
    PRI.s = PRI/total,
    PRD.s = PRD/total
  ) #%>% filter(inc_top != "Otros" & !is.na(Winner2))

data <- data %>% 
  group_by(muni) %>% 
  mutate(
    lag_PAN.s = lag(PAN.s, n = 1, order_by = year),
    lag_PRI.s = lag(PRI.s, n = 1, order_by = year),
    lag_PRD.s = lag(PRD.s, n = 1, order_by = year)
  ) %>% 
  ungroup()

data <- data %>% 
  mutate(
    PAN.ch = ifelse(!is.na(PAN.s) & !is.na(lag_PAN.s), 
                                           (PAN.s - lag_PAN.s)/lag_PAN.s, NA),
    PRI.ch = ifelse(!is.na(PRI.s) & !is.na(lag_PRI.s), 
                                           (PRI.s - lag_PRI.s)/lag_PRI.s, NA),
    PRD.ch = ifelse(!is.na(PRD.s) & !is.na(lag_PRD.s), 
                                           (PRD.s - lag_PRD.s)/lag_PRD.s, NA),
    
    PAN.ch2 = PAN.s - lag_PAN.s,
    PRI.ch2 = PRI.s - lag_PRI.s,
    PRD.ch2 = PRD.s - lag_PRD.s,
    
    inc.ch = ifelse(inc_top == "PAN", PAN.ch,
                    ifelse(inc_top == "PRI", PRI.ch,
                           ifelse(inc_top == "PRD", PRD.ch, NA))),
    
    inc.ch2 = ifelse(inc_top == "PAN", PAN.ch2,
                    ifelse(inc_top == "PRI", PRI.ch2,
                           ifelse(inc_top == "PRD", PRD.ch2, NA))),
    
    conco = ifelse(win_top == wintop_state, 1, 0)
  ) %>% 
  filter(!is.infinite(inc.ch))

View(data %>% select(muni, year, win_top, inc_top, 
                     PAN, PRI, PRD, total, 
                     PAN.s, PRI.s, PRD.s, 
                     lag_PAN.s, lag_PRI.s, lag_PRD.s,
                     PAN.ch, PRI.ch, PRD.ch,
                     inc.ch, inc.ch2) %>% filter(muni == "1001"), "holi")

write.csv(data, paste(out, "Electoral.csv", sep = "/"), row.names = F)
