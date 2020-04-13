rm(list = ls())
setwd("~")

library(dplyr)
library(stargazer)
library(lfe)
library(lme4)
library(glmmML)
library(tidyr)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/alternative"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/alternative/tables"

options(scipen=999)

data <- read.csv(paste(inp, "final.csv", sep = "/"))
nrow(data) #17,023
data <- data %>% 
  filter(!is.na(inc_top)
  )

data <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= quantile(ch.agua, 0.95, na.rm = T) & 
                       ch.agua >= quantile(ch.agua, 0.05, na.rm = T), ch.agua, NA),
    
    ch.dren = ifelse(ch.dren <= quantile(ch.dren, 0.95, na.rm = T) &
                       ch.dren >= quantile(ch.dren, 0.05, na.rm = T) , ch.dren, NA),
    
    ch.elec = ifelse(ch.elec <= quantile(ch.elec, 0.95, na.rm = T) &
                       ch.elec >= quantile(ch.elec, 0.05, na.rm = T), ch.elec, NA),
    
    ch.del = ifelse(ch.del <= quantile(ch.del, 0.95, na.rm = T) &
                      ch.del >= quantile(ch.del, 0.05, na.rm = T), ch.del, NA),
    
    ch.hom = ifelse(ch.hom <= quantile(ch.hom, 0.95, na.rm = T) & 
                      ch.hom >= quantile(ch.hom, 0.05, na.rm = T), ch.hom, NA)
  )
#Convertí todos los outliers en NA


# Modelos -----------------------------------------------------------------

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data)
u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data)

u6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (muni | edo.year), data, family = binomial(link = "logit"))
u7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (muni | edo.year), data, family = binomial(link = "logit"))
u8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (muni | edo.year), data, family = binomial(link = "logit"))
u9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (muni | edo.year), data, family = binomial(link = "logit"))
u10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (muni | edo.year), data, family = binomial(link = "logit"))

stargazer(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10,
          title = "Tabla 4. Todas las observaciones",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", 
          out = paste(out, "todos_FE.html", sep = "/"), 
          flip = T)


##PAN####
#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PAN")
panfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PAN")
panfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PAN")
panfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PAN")
panfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PAN")

panfe6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PAN", 
                family = binomial(link = "logit"))
panfe7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PAN", 
                family = binomial(link = "logit"))
panfe8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PAN", 
                family = binomial(link = "logit"))
panfe9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PAN", 
                family = binomial(link = "logit"))
panfe10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PAN", 
                 family = binomial(link = "logit"))

stargazer(panfe1, panfe2, panfe3, panfe4, panfe5, panfe6, panfe7, panfe8, panfe9, panfe10, 
          title = "Tabla 5. Municipios gobernados por el PAN",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PAN_FE.html", sep = "/"), flip = T)

#PRI####

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRI")
prife2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRI")
prife3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRI")
prife4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRI")
prife5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRI")

prife6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRI", 
                family = binomial(link = "logit"))
prife7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRI", 
                family = binomial(link = "logit"))
prife8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRI", 
                family = binomial(link = "logit"))
prife9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRI", 
                family = binomial(link = "logit"))
prife10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRI", 
                 family = binomial(link = "logit"))

stargazer(prife1, prife2, prife3, prife4, prife5, prife6, prife7, prife8, prife9, prife10, 
          title = "Tabla 6. Municipios gobernados por el PRI",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRI_FE.html", sep = "/"), flip = T)


#PRD####

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRD")
prdfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRD")
prdfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRD")
prdfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRD")
prdfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | muni + edo.year | 0 | muni, data, subset = inc_top == "PRD")

prdfe6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRD", 
                family = binomial(link = "logit"))
prdfe7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRD", 
                family = binomial(link = "logit"))
prdfe8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRD", 
                family = binomial(link = "logit"))
prdfe9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRD", 
                family = binomial(link = "logit"))
prdfe10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (muni | edo.year), data, subset = inc_top == "PRD", 
                 family = binomial(link = "logit"))

stargazer(prdfe1, prdfe2, prdfe3, prdfe4, prdfe5, prdfe6, prdfe7, prdfe8, prdfe9, prdfe10, 
          title = "Tabla 7. Municipios gobernados por el PRD",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRD_FE.html", sep = "/"), flip = T)