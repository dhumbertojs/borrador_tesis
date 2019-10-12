rm(list = ls())
setwd("~")

library(dplyr)
library(stargazer)
library(lfe)
library(lme4)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/tables"

data <- read.csv(paste(inp, "final.csv", sep = "/"))
##Hay 10,230 observaciones

# Modelos -----------------------------------------------------------------

#Modelo lineal
#summary(lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data)) 
#Se eliminaron 4,561 por missingness
t1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data)
t2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data)
t3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data)
t4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data)
t5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data)

t6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, family = binomial(link = "probit"))
t7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, family = binomial(link = "probit"))
t8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, family = binomial(link = "probit"))
t9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, family = binomial(link = "probit"))
t10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, family = binomial(link = "probit"))

stargazer(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, type = "html", out = paste(out, "todos.html", sep = "/"), flip = T)

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data)
u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data)

u6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo_year), data, model = binomial("probit"))
u10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo_year), data, model = binomial("probit"))

stargazer(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, type = "html", out = paste(out, "todos_FE.html", sep = "/"), flip = T)

#PAN####
PAN1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
PAN2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
PAN3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
PAN4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
PAN5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")

PAN6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))
PAN10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "probit"))

stargazer(PAN1, PAN2, PAN3, PAN4, PAN5, PAN6, PAN7, PAN8, PAN9, PAN10, type = "html", out = paste(out, "PAN.html", sep = "/"), flip = T)

#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")
panfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PAN")

panfe6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))
panfe10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PAN", model = binomial("probit"))

stargazer(panfe1, panfe2, panfe3, panfe4, panfe5, panfe6, panfe7, panfe8, panfe9, panfe10, type = "html", out = paste(out, "PAN_FE.html", sep = "/"), flip = T)

#PRI####
pri1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
pri2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
pri3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
pri4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
pri5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")

pri6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))
pri10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "probit"))

stargazer(pri1, pri2, pri3, pri4, pri5, pri6, pri7, pri8, pri9, pri10, type = "html", out = paste(out, "PRI.html", sep = "/"), flip = T)

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")
prife5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRI")

prife6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))
prife10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRI", model = binomial("probit"))

stargazer(prife1, prife2, prife3, prife4, prife5, prife6, prife7, prife8, prife9, prife10, type = "html", out = paste(out, "PRI_FE.html", sep = "/"), flip = T)

#PRD####
prd1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
prd2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
prd3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
prd4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
prd5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")

prd6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))
prd10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "probit"))

stargazer(prd1, prd2, prd3, prd4, prd5, prd6, prd7, prd8, prd9, prd10, type = "html", out = paste(out, "PRD.html", sep = "/"), flip = T)

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")
prdfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | 0 | 0 | edo_year, data, subset = inc_top == "PRD")

prdfe6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))
prdfe10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo_year), data, subset = inc_top == "PRD", model = binomial("probit"))

stargazer(prdfe1, prdfe2, prdfe3, prdfe4, prdfe5, prdfe6, prdfe7, prdfe8, prdfe9, prdfe10, type = "html", out = paste(out, "PRD_FE.html", sep = "/"), flip = T)