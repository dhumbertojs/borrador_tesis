pacman::p_load(dplyr, stargazer, lfe, lme4, glmmML, tidyr, rms, miceadds)

inp <- "./"
list.files(inp)
out <- "./version_21/original/tables"

options(scipen=999)

data <- read.csv(paste(inp, "final_original.csv", sep = "/"))
nrow(data) #17,023
data <- data %>% 
  filter(!is.na(inc_top)) %>% 
  mutate(
    edo.year = as.character(edo.year)
  )

#data cropped, 2.5% en cada extremo
datac <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= quantile(ch.agua, 0.975, na.rm = T) & 
                       ch.agua >= quantile(ch.agua, 0.025, na.rm = T), ch.agua, NA),
    
    ch.dren = ifelse(ch.dren <= quantile(ch.dren, 0.975, na.rm = T) &
                       ch.dren >= quantile(ch.dren, 0.025, na.rm = T) , ch.dren, NA),
    
    ch.elec = ifelse(ch.elec <= quantile(ch.elec, 0.975, na.rm = T) &
                       ch.elec >= quantile(ch.elec, 0.025, na.rm = T), ch.elec, NA),
    
    # ch.del = ifelse(ch.del <= quantile(ch.del, 0.975, na.rm = T) &
    #                   ch.del >= quantile(ch.del, 0.025, na.rm = T), ch.del, NA),
    
    ch.hom = ifelse(ch.hom <= quantile(ch.hom, 0.975, na.rm = T) & 
                      ch.hom >= quantile(ch.hom, 0.025, na.rm = T), ch.hom, NA)
  )
#Convertí todos los outliers en NA


# Modelos cropped-----------------------------------------------------------------

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac)
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac)

u6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
                   family = binomial(link = "logit"), cluster = "muni", data=datac)
u7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datac)
u8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datac)
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, family = binomial(link = "logit"))
u10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
           family = binomial(link = "logit"), cluster = "muni", data=datac)

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 6. Todas las observaciones",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", 
          out = paste(out, "todos_FE_crop.html", sep = "/"), 
          flip = T)


##* PAN####
#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PAN")
panfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
               | 0 | 0 | muni, datac, subset = inc_top == "PAN")
panfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PAN")
#panfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac, subset = inc_top == "PAN")
panfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PAN")

panfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PAN"))
panfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PAN"))
panfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PAN"))
# panfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, subset = inc_top == "PAN", 
#                 family = binomial(link = "logit"))
panfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datac)

stargazer(panfe1, panfe2, panfe3, #panfe4, 
          panfe5, panfe6, panfe7, panfe8, #panfe9, 
          panfe10, 
          title = "Tabla 7. Municipios gobernados por el PAN",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PAN_FE_crop.html", sep = "/"), flip = T)

#* PRI####

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRI")
prife2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRI")
prife3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRI")
#prife4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac, subset = inc_top == "PRI")
prife5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRI")

prife6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRI"))
prife7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRI"))
prife8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRI"))
# prife9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, subset = inc_top == "PRI", 
#                 family = binomial(link = "logit"))
prife10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRI"))

stargazer(prife1, prife2, prife3, #prife4, 
          prife5, prife6, prife7, prife8, #prife9, 
          prife10, 
          title = "Tabla 8. Municipios gobernados por el PRI",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRI_FE_crop.html", sep = "/"), flip = T)


#* PRD####

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRD")
prdfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRD")
prdfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRD")
#prdfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac, subset = inc_top == "PRD")
prdfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRD")

prdfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRD"))

prdfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRD"))

prdfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRD"))
# prdfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, subset = inc_top == "PRD", 
#                 family = binomial(link = "logit"))
prdfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(inc_top == "PRD"))

stargazer(prdfe1, prdfe2, prdfe3, #prdfe4, 
          prdfe5, #prdfe6, 
          prdfe7, prdfe8, #prdfe9, 
          prdfe10, 
          title = "Tabla 9. Municipios gobernados por el PRD",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRD_FE_crop.html", sep = "/"), flip = T)


# Cropped info incompleta -------------------------------------------------

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac %>% filter(n_agua==0))
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac %>% filter(n_elec==0))
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac %>% filter(n_dren==0))
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, datac %>% filter(n_hom==0))

u6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_agua==0))
u7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_elec==0))
u8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_dren==0))
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, family = binomial(link = "logit"))
u10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
           family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_hom==0))

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 6. Todas las observaciones",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", 
          out = paste(out, "todos_FE_crop_trienios.html", sep = "/"), 
          flip = T)


##* PAN####
#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_agua==0), subset = inc_top == "PAN")
panfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
               | 0 | 0 | muni, datac %>% filter(n_elec==0), subset = inc_top == "PAN")
panfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_dren==0), subset = inc_top == "PAN")
#panfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac, subset = inc_top == "PAN")
panfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_hom==0), subset = inc_top == "PAN")

panfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_agua==0 & inc_top == "PAN"))
panfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_elec==0 & inc_top == "PAN"))
panfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_dren==0 & inc_top == "PAN"))
# panfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, subset = inc_top == "PAN", 
#                 family = binomial(link = "logit"))
panfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_hom==0 & inc_top == "PAN"))

stargazer(panfe1, panfe2, panfe3, #panfe4, 
          panfe5, panfe6, panfe7, panfe8, #panfe9, 
          panfe10, 
          title = "Tabla 7. Municipios gobernados por el PAN",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PAN_FE_crop_trienios.html", sep = "/"), flip = T)

#* PRI####

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_agua==0), subset = inc_top == "PRI")
prife2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRI")
prife3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_dren==0), subset = inc_top == "PRI")
#prife4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac, subset = inc_top == "PRI")
prife5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_hom==0), subset = inc_top == "PRI")

prife6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_agua==0 & inc_top == "PRI"))
prife7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_elec==0 & inc_top == "PRI"))
prife8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_dren==0 & inc_top == "PRI"))
# prife9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, subset = inc_top == "PRI", 
#                 family = binomial(link = "logit"))
prife10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_hom==0 & inc_top == "PRI"))

stargazer(prife1, prife2, prife3, #prife4, 
          prife5, prife6, prife7, prife8, #prife9, 
          prife10, 
          title = "Tabla 8. Municipios gobernados por el PRI",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRI_FE_crop_trienios.html", sep = "/"), flip = T)


#* PRD####

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_agua==0), subset = inc_top == "PRD")
prdfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_dren==0), subset = inc_top == "PRD")
prdfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac, subset = inc_top == "PRD")
#prdfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datac, subset = inc_top == "PRD")
prdfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datac %>% filter(n_hom==0), subset = inc_top == "PRD")

prdfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_agua==0 & inc_top == "PRD"))

prdfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_elec==0 & inc_top == "PRD"))

prdfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_dren==0 & inc_top == "PRD"))
# prdfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, subset = inc_top == "PRD", 
#                 family = binomial(link = "logit"))
prdfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datac %>% filter(n_hom==0 & inc_top == "PRD"))

stargazer(prdfe1, prdfe2, prdfe3, #prdfe4, 
          prdfe5, #prdfe6, 
          prdfe7, prdfe8, #prdfe9, 
          prdfe10, 
          title = "Tabla 9. Municipios gobernados por el PRD",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRD_FE_crop_trienios.html", sep = "/"), flip = T)

# Capped ------------------------------------------------------------------

datacap <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= 100, ch.agua, NA),
    
    ch.dren = ifelse(ch.dren < -100 |
                       ch.dren > 100 , NA, ch.dren),
    
    ch.elec = ifelse(ch.elec <= 100, ch.elec, NA),
    
    # ch.del = ifelse(ch.del < -100 |
    #                   ch.del > 100, NA, ch.del),
    
    ch.hom = ifelse(ch.hom < -100 | 
                      ch.hom > 100, NA, ch.hom)
  )

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap)
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap)
u6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
                  family = binomial(link = "logit"), cluster = "muni", data=datacap)
u7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datacap)
u8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datacap)
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, family = binomial(link = "logit"))
u10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
           family = binomial(link = "logit"), cluster = "muni", data=datacap)

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 6. Todas las observaciones",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", 
          out = paste(out, "todos_FE_cap.html", sep = "/"), 
          flip = T)


##* PAN####
#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PAN")
panfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PAN")
panfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PAN")
#panfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap, subset = inc_top == "PAN")
panfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PAN")

panfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PAN"))
panfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PAN"))
panfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PAN"))
# panfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, subset = inc_top == "PAN", 
#                 family = binomial(link = "logit"))
panfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PAN"))

stargazer(panfe1, panfe2, panfe3, #panfe4, 
          panfe5, panfe6, panfe7, panfe8, #panfe9, 
          panfe10, 
          title = "Tabla 7. Municipios gobernados por el PAN",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PAN_FE_cap.html", sep = "/"), flip = T)

#* PRI####

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRI")
prife2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRI")
prife3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRI")
#prife4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap, subset = inc_top == "PRI")
prife5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRI")

prife6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRI"))
prife7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRI"))
prife8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRI"))
# prife9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, subset = inc_top == "PRI", 
#                 family = binomial(link = "logit"))
prife10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRI"))

stargazer(prife1, prife2, prife3, #prife4, 
          prife5, prife6, prife7, prife8, #prife9, 
          prife10, 
          title = "Tabla 8. Municipios gobernados por el PRI",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRI_FE_cap.html", sep = "/"), flip = T)


#* PRD####

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRD")
prdfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRD")
prdfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRD")
#prdfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap, subset = inc_top == "PRD")
prdfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap, subset = inc_top == "PRD")

prdfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRD"))
prdfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRD"))
prdfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRD"))
# prdfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, subset = inc_top == "PRD",
#                 family = binomial(link = "logit"))
prdfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(inc_top == "PRD"))

stargazer(prdfe1, prdfe2, prdfe3, #prdfe4, 
          prdfe5, prdfe6, 
          prdfe7, prdfe8, #prdfe9, 
          prdfe10, 
          title = "Tabla 9. Municipios gobernados por el PRD",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRD_FE_cap.html", sep = "/"), flip = T)

# Capped info incompleta------------------------------------------------------------------

datacap <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= 100, ch.agua, NA),
    
    ch.dren = ifelse(ch.dren < -100 |
                       ch.dren > 100 , NA, ch.dren),
    
    ch.elec = ifelse(ch.elec <= 100, ch.elec, NA),
    
    # ch.del = ifelse(ch.del < -100 |
    #                   ch.del > 100, NA, ch.del),
    
    ch.hom = ifelse(ch.hom < -100 | 
                      ch.hom > 100, NA, ch.hom)
  )

#Efectos fijos

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap %>% filter(n_agua==0))
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap %>% filter(n_elec==0))
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap %>% filter(n_dren==0))
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datacap %>% filter(n_hom==0))
u6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_agua==0))
u7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_elec==0))
u8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
          family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_dren==0))
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, family = binomial(link = "logit"))
u10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
           family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_hom==0))

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 6. Todas las observaciones",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", 
          out = paste(out, "todos_FE_cap_trienios.html", sep = "/"), 
          flip = T)


##* PAN####
#Efectos fijos

panfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_agua==0), subset = inc_top == "PAN")
panfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_elec==0), subset = inc_top == "PAN")
panfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_dren==0), subset = inc_top == "PAN")
#panfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap, subset = inc_top == "PAN")
panfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_hom==0), subset = inc_top == "PAN")

panfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_agua==0 & inc_top == "PAN"))
panfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_elec==0 & inc_top == "PAN"))
panfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_dren==0 & inc_top == "PAN"))
# panfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, subset = inc_top == "PAN", 
#                 family = binomial(link = "logit"))
panfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_hom==0 & inc_top == "PAN"))

stargazer(panfe1, panfe2, panfe3, #panfe4, 
          panfe5, panfe6, panfe7, panfe8, #panfe9, 
          panfe10, 
          title = "Tabla 7. Municipios gobernados por el PAN",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PAN_FE_cap_trienios.html", sep = "/"), flip = T)

#* PRI####

#Efectos fijos

prife1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_agua==0), subset = inc_top == "PRI")
prife2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_elec==0), subset = inc_top == "PRI")
prife3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_dren==0), subset = inc_top == "PRI")
#prife4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap, subset = inc_top == "PRI")
prife5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_hom==0), subset = inc_top == "PRI")

prife6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_agua==0 & inc_top == "PRI"))
prife7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_elec==0 & inc_top == "PRI"))
prife8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_dren==0 & inc_top == "PRI"))
# prife9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, subset = inc_top == "PRI", 
#                 family = binomial(link = "logit"))
prife10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_hom==0  & inc_top == "PRI"))

stargazer(prife1, prife2, prife3, #prife4, 
          prife5, prife6, prife7, prife8, #prife9, 
          prife10, 
          title = "Tabla 8. Municipios gobernados por el PRI",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRI_FE_cap_trienios.html", sep = "/"), flip = T)


#* PRD####

#Efectos fijos

prdfe1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_agua==0), subset = inc_top == "PRD")
prdfe2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_elec==0), subset = inc_top == "PRD")
prdfe3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_dren==0), subset = inc_top == "PRD")
#prdfe4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | 0, datacap, subset = inc_top == "PRD")
prdfe5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
               | 0 | 0 | muni, datacap %>% filter(n_hom==0), subset = inc_top == "PRD")

prdfe6 <- glm.cluster(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_agua==0 & inc_top == "PRD"))
prdfe7 <- glm.cluster(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres, 
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_elec==0 & inc_top == "PRD"))
prdfe8 <- glm.cluster(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
              family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_dren==0 & inc_top == "PRD"))
# prdfe9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datacap, subset = inc_top == "PRD",
#                 family = binomial(link = "logit"))
prdfe10 <- glm.cluster(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
               family = binomial(link = "logit"), cluster = "muni", data=datacap %>% filter(n_hom==0 & inc_top == "PAN"))

stargazer(prdfe1, prdfe2, prdfe3, #prdfe4, 
          prdfe5, prdfe6, 
          prdfe7, prdfe8, #prdfe9, 
          prdfe10, 
          title = "Tabla 9. Municipios gobernados por el PRD",
          covariate.labels = c("Agua", "Electricidad", "Drenaje", #"Total de delitos", 
                               "Homicidios", "Población", "Índice de marginación", 
                               "Yuxtaposición", 
                               "Elección concurrente: diputación local",
                               "Elección concurrente: diputación federal",
                               "Elección concurrente: senaduría",
                               "Elección concurrente: gubernatura",
                               "Elección concurrente: presidencia",
                               "Constante"),
          dep.var.labels = c("Cambio % del incumbent", "Alternancia"),
          type = "html", out = paste(out, "PRD_FE_cap_trienios.html", sep = "/"), flip = T)
