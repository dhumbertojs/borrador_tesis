pacman::p_load(dplyr, stargazer, lfe, lme4, glmmML, tidyr, ggplot2)

inp <- "./"
list.files(inp)
out <- "./version_21/lead/tables"

options(scipen=999)

data <- read.csv(paste(inp, "final_original_lead.csv", sep = "/"))
nrow(data) #17,023
data <- data %>% 
  filter(!is.na(inc_top)) %>% 
  # mutate(
  #   edo.year = as.character(edo.year),
  #   dummy_PAN = ifelse(inc_top=="PAN", 1, 0),
  #   dummy_PRI = ifelse(inc_top=="PRI", 1, 0),
  #   dummy_PRD = ifelse(inc_top=="PRD", 1, 0)
  # ) %>% 
  filter(state=="18" | state=="27")




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
#Convertó todos los outliers en NA

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


# Analisis ----------------------------------------------------------------


u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datac)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datac)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datac)
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | muni, datac)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, datac)

u6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          datac, family = binomial(link = "logit"))
u7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          datac, family = binomial(link = "logit"))
u8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          datac, family = binomial(link = "logit"))
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , datac, family = binomial(link = "logit"))
u10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
           datac, family = binomial(link = "logit"))

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 10. Nayarit y Tabasco",
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
          out = paste(out, "11_todos.html", sep = "/"), 
          style = "aer",
          flip = T)



# PAN ---------------------------------------------------------------------

pan <- datac %>% 
  filter(inc_top == "PAN")

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pan)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pan)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pan)
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | muni, pan)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, pan)

u6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pan, family = binomial(link = "logit"))
u7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pan, family = binomial(link = "logit"))
u8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pan, family = binomial(link = "logit"))
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , pan, family = binomial(link = "logit"))
u10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
           pan, family = binomial(link = "logit"))

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 12. Nayarit y Tabasco (PAN)",
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
          out = paste(out, "11_pan.html", sep = "/"), 
          style = "aer",
          flip = T)


# PRI ---------------------------------------------------------------------

pri <- datac %>% 
  filter(inc_top == "PRI")

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pri)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pri)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pri)
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | muni, pri)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, pri)

u6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pri, family = binomial(link = "logit"))
u7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pri, family = binomial(link = "logit"))
u8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pri, family = binomial(link = "logit"))
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , pri, family = binomial(link = "logit"))
u10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
           pri, family = binomial(link = "logit"))

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 13. Nayarit y Tabasco (PRI)",
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
          out = paste(out, "11_pri.html", sep = "/"), 
          style = "aer",
          flip = T)


# PRD ---------------------------------------------------------------------

pan <- datac %>% 
  filter(inc_top == "PRD")

u1 <- felm(inc.ch ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pan)
u2 <- felm(inc.ch ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pan)
u3 <- felm(inc.ch ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres
           | 0 | 0 | muni, pan)
#u4 <- felm(inc.ch ~ ch.del + log(POB_TOT) + IM + conco | 0 | 0 | muni, pan)
u5 <- felm(inc.ch ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres 
           | 0 | 0 | muni, pan)

u6 <- glm(alt ~ ch.agua + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pan, family = binomial(link = "logit"))
u7 <- glm(alt ~ ch.elec + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pan, family = binomial(link = "logit"))
u8 <- glm(alt ~ ch.dren + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
          pan, family = binomial(link = "logit"))
#u9 <- glm(alt ~ ch.del + log(POB_TOT) + IM + conco , pan, family = binomial(link = "logit"))
u10 <- glm(alt ~ ch.hom + log(POB_TOT) + IM + conco + d_dipl + d_dipf + d_sen + d_gob + d_pres,
           pan, family = binomial(link = "logit"))

stargazer(u1, u2, u3, #u4, 
          u5, u6, u7, u8, #u9, 
          u10,
          title = "Tabla 14. Nayarit y Tabasco (PRD)",
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
          out = paste(out, "11_prd.html", sep = "/"), 
          style = "aer",
          flip = T)



party <- c("PAN" = "#153588", "PRI" = "#E13A27", 
           "PRD" = "#F6D626", "otros" = "#000000")

party_series <- data %>% 
  filter(!is.na(inc_top)) %>% 
  group_by(year) %>% 
  count(inc_top) %>% 
  mutate(
    total = sum(n),
    porcentaje = n/total
  )

ggplot(party_series, 
       aes(x = year, y = porcentaje, group = inc_top)) +
  geom_line(aes(color = inc_top), size = 2) +
  geom_point(aes(color = inc_top), shape = 2, size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = (1994:2015)) +
  scale_colour_manual(values = party, name = "Partido") +
  labs(title = "Municipios gobernados por partido",
       subtitle = "Como porcentaje por año",
       x = "", y = "") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 23)
    #axis.text.x = element_text(size = 14)
  )
ggsave("Nay y Tab.png", 
       path = out,
       dpi = 300,
       height = 6,
       width = 14
)