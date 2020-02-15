rm(list = ls())
setwd("~")

library(dplyr)
library(stargazer)
library(lfe)
library(lme4)
library(glmmML)
library(tidyr)
library(Hmisc)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/tables"

data <- read.csv(paste(inp, "final.csv", sep = "/"))
##Hay 10,230 observaciones

data <- data %>% 
  select(
    muniYear, state, muni, year, wintop_state, win_top, inc_top, conco,        
    inc.ch, IM, POB_TOT, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, alt, edo.year     
  )

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


corr <- data %>% 
  select(
  alt, inc.ch, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, IM, POB_TOT
) %>% 
  rename(pob = POB_TOT, im = IM)

rcorr(as.matrix(corr), type = "pearson")

# Modelos -----------------------------------------------------------------

#Modelo lineal
#summary(lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data)) 

# t1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data)
# t2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data)
# t3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data)
# t4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data)
# t5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data)
# 
# t6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, family = binomial(link = "logit")(link = "logit"))
# t7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, family = binomial(link = "logit")(link = "logit"))
# t8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, family = binomial(link = "logit")(link = "logit"))
# t9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, family = binomial(link = "logit")(link = "logit"))
# t10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, family = binomial(link = "logit")(link = "logit"))
# 
# stargazer(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, type = "html", out = paste(out, "todos.html", sep = "/"), flip = T)

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = ch.agua <= 100)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = ch.elec <= 100)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = ch.dren <= 100)
u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = ch.del <= 100)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = ch.hom < 100)

u6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo.year), data, family = binomial(link = "logit"), subset = ch.agua <= 100)
u7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo.year), data, family = binomial(link = "logit"), subset = ch.elec <= 100)
u8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo.year), data, family = binomial(link = "logit"), subset = ch.dren <= 100)
u9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo.year), data, family = binomial(link = "logit"), subset = ch.del <= 100)
u10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo.year), data, family = binomial(link = "logit"), subset = ch.hom <= 100)

stargazer(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10,
          title = "Tabla 4. Todas las observaciones",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", 
          out = paste(out, "todos_FE.html", sep = "/"), 
          flip = T)

# summary(glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco, family = binomial(link = "logit")(link = "logit"), cluster = "edo.year", data = data)) ##Esto son errores estandar agrupados 

# u11 <- glmmboot(alt ~ ch.agua + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year)
# u12 <- glmmboot(alt ~ ch.dren + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year)
# u13 <- glmmboot(alt ~ ch.elec + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year)
# u14 <- glmmboot(alt ~ ch.del + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year)
# u15 <- glmmboot(alt ~ ch.hom + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year)

#PAN####
# PAN1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
# PAN2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
# PAN3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
# PAN4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
# PAN5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN")
# 
# PAN6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "logit")(link = "logit"))
# PAN7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "logit")(link = "logit"))
# PAN8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "logit")(link = "logit"))
# PAN9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "logit")(link = "logit"))
# PAN10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PAN", family = binomial(link = "logit")(link = "logit"))
# 
# stargazer(PAN1, PAN2, PAN3, PAN4, PAN5, PAN6, PAN7, PAN8, PAN9, PAN10, type = "html", out = paste(out, "PAN.html", sep = "/"), flip = T)

#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PAN" & ch.agua <= 100)
panfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PAN" & ch.elec <= 100)
panfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PAN" & ch.dren <= 100)
panfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PAN" & ch.del <= 100)
panfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PAN" & ch.hom <= 100)

panfe6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PAN" & ch.agua <= 100, family = binomial(link = "logit"))
panfe7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PAN" & ch.elec <= 100, family = binomial(link = "logit"))
panfe8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PAN" & ch.dren <= 100, family = binomial(link = "logit"))
panfe9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PAN" & ch.del <= 100, family = binomial(link = "logit"))
panfe10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PAN" & ch.hom <= 100, family = binomial(link = "logit"))

stargazer(panfe1, panfe2, panfe3, panfe4, panfe5, panfe6, panfe7, panfe8, panfe9, panfe10, 
          title = "Tabla 5. Municipios gobernados por el PAN",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PAN_FE.html", sep = "/"), flip = T)


# panfe11 <- glmmboot(alt ~ ch.agua + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PAN")
# panfe12 <- glmmboot(alt ~ ch.dren + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PAN")
# panfe13 <- glmmboot(alt ~ ch.elec + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PAN")
# panfe14 <- glmmboot(alt ~ ch.del + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PAN")
# panfe15 <- glmmboot(alt ~ ch.hom + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PAN")

#PRI####
# pri1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
# pri2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
# pri3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
# pri4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
# pri5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI")
# 
# pri6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "logit")(link = "logit"))
# pri7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "logit")(link = "logit"))
# pri8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "logit")(link = "logit"))
# pri9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "logit")(link = "logit"))
# pri10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRI", family = binomial(link = "logit")(link = "logit"))
# 
# stargazer(pri1, pri2, pri3, pri4, pri5, pri6, pri7, pri8, pri9, pri10, type = "html", out = paste(out, "PRI.html", sep = "/"), flip = T)

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRI" & ch.agua <= 100)
prife2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRI" & ch.elec <= 100)
prife3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRI" & ch.dren <= 100)
prife4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRI" & ch.del <= 100)
prife5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRI" & ch.hom <= 100)

prife6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRI" & ch.agua <= 100, family = binomial(link = "logit"))
prife7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRI" & ch.elec <= 100, family = binomial(link = "logit"))
prife8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRI" & ch.dren <= 100, family = binomial(link = "logit"))
prife9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRI" & ch.del <= 100, family = binomial(link = "logit"))
prife10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRI" & ch.hom <= 100, family = binomial(link = "logit"))

stargazer(prife1, prife2, prife3, prife4, prife5, prife6, prife7, prife8, prife9, prife10, 
          title = "Tabla 6. Municipios gobernados por el PRI",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRI_FE.html", sep = "/"), flip = T)

# prife11 <- glmmboot(alt ~ ch.agua + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PRI")
# prife12 <- glmmboot(alt ~ ch.dren + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PRI")
# prife13 <- glmmboot(alt ~ ch.elec + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PRI")
# prife14 <- glmmboot(alt ~ ch.del + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PRI")
# prife15 <- glmmboot(alt ~ ch.hom + log(POB_TOT) + IM + conco, family = binomial(link = "logit"), data, edo.year, subset = inc_top == "PRI")

#PRD####
# prd1 <- lm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
# prd2 <- lm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
# prd3 <- lm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
# prd4 <- lm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
# prd5 <- lm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD")
# 
# prd6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "logit")(link = "logit"))
# prd7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "logit")(link = "logit"))
# prd8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "logit")(link = "logit"))
# prd9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "logit")(link = "logit"))
# prd10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco, data, subset = inc_top == "PRD", family = binomial(link = "logit")(link = "logit"))
# 
# stargazer(prd1, prd2, prd3, prd4, prd5, prd6, prd7, prd8, prd9, prd10, type = "html", out = paste(out, "PRD.html", sep = "/"), flip = T)

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRD" & ch.agua <= 100)
prdfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRD" & ch.elec <= 100)
prdfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRD" & ch.dren <= 100)
prdfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRD" & ch.del <= 100)
prdfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco | edo.year | 0 | muni, data, subset = inc_top == "PRD" & ch.hom <= 100)

prdfe6 <- glmer(alt ~ ch.agua + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRD" & ch.agua <= 100, family = binomial(link = "logit"))
prdfe7 <- glmer(alt ~ ch.elec + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRD" & ch.elec <= 100, family = binomial(link = "logit"))
prdfe8 <- glmer(alt ~ ch.dren + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRD" & ch.dren <= 100, family = binomial(link = "logit"))
prdfe9 <- glmer(alt ~ ch.del + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRD" & ch.del <= 100, family = binomial(link = "logit"))
prdfe10 <- glmer(alt ~ ch.hom + log(POB_TOT) + IM + conco + (1 | edo.year), data, subset = inc_top == "PRD" & ch.hom <= 100, family = binomial(link = "logit"))

stargazer(prdfe1, prdfe2, prdfe3, prdfe4, prdfe5, prdfe6, prdfe7, prdfe8, prdfe9, prdfe10, 
          title = "Tabla 7. Municipios gobernados por el PRD",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", "Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Concordancia", "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRD_FE.html", sep = "/"), flip = T)

# prdfe11 <- glmmboot(alt ~ ch.agua + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year, subset = inc_top == "PRD")
# prdfe12 <- glmmboot(alt ~ ch.dren + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year, subset = inc_top == "PRD")
# prdfe13 <- glmmboot(alt ~ ch.elec + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year, subset = inc_top == "PRD")
# prdfe14 <- glmmboot(alt ~ ch.del + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year, subset = inc_top == "PRD")
# prdfe15 <- glmmboot(alt ~ ch.hom + log(POB_TOT) + IM + conco, family = binomial(link = "logit")("logit"), data, edo.year, subset = inc_top == "PRD")