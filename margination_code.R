rm(list = ls())
setwd("~")

library(dplyr)
library(stringr)

inp <- "/Users/dhjs/Documents/projects/electoral_accountability/datos"
list.files(inp)
out <- "/Users/dhjs/Documents/projects/electoral_accountability/databases"

mar <- read.csv(paste(inp, "Base_Indice_de_marginacion_municipal_90-15.csv", sep = "/"), fileEncoding = "latin1", stringsAsFactors = F)

mar <- mar %>% 
  filter(ENT != "Nacional") %>% 
  select(CVE_MUN, IM, POB_TOT, AÑO) %>% 
  mutate(
    IM = as.numeric(IM),
    POB_TOT = as.numeric(POB_TOT),
    CVE_MUN = str_pad(CVE_MUN, width = 5, side = "left", pad = "0"),
    muniYear = paste(CVE_MUN, AÑO, sep = "_")
  )
write.csv (mar, paste(out, "margination.csv", sep = "/"), row.names = F)

t1 <- mar %>%
  filter( AÑO == 1990) %>%
  rename(IM.90 = IM, POB.90 = POB_TOT) %>%
  select(-c(AÑO, muniYear))

t2 <- mar %>%
  filter( AÑO == 1995) %>%
  rename(IM.95 = IM, POB.95 = POB_TOT) %>%
  select(-c(AÑO, muniYear))

t3 <- mar %>%
  filter( AÑO == 2000) %>%
  rename(IM.00 = IM, POB.00 = POB_TOT) %>%
  select(-c(AÑO, muniYear))

t4 <- mar %>%
  filter( AÑO == 2005) %>%
  rename(IM.05 = IM, POB.05 = POB_TOT) %>%
  select(-c(AÑO, muniYear))

t5 <- mar %>%
  filter( AÑO == 2010) %>%
  rename(IM.10 = IM, POB.10 = POB_TOT) %>%
  select(-c(AÑO, muniYear))

t6 <- mar %>%
  filter( AÑO == 2015) %>%
  rename(IM.15 = IM, POB.15 = POB_TOT) %>%
  select(-c(AÑO, muniYear))

fin <- as.data.frame(unique(mar$CVE_MUN))
colnames(fin) <- "CVE_MUN"

fin <- full_join(fin, t1, by = "CVE_MUN")
fin <- full_join(fin, t2, by = "CVE_MUN")
fin <- full_join(fin, t3, by = "CVE_MUN")
fin <- full_join(fin, t4, by = "CVE_MUN")
fin <- full_join(fin, t5, by = "CVE_MUN")
fin <- full_join(fin, t6, by = "CVE_MUN")

fin <- fin %>%
  mutate(
    t90.95 = (log(IM.95/IM.90))/5,####
    p90.95 = (log(POB.95/POB.90))/5,

    muniYear_91 =paste(CVE_MUN, "1991", sep = "_"),
    IM.91 = IM.90 * exp(t90.95 * 1),
    POB.91 = POB.90 * exp(p90.95 * 1),

    muniYear_92 =paste(CVE_MUN, "1992", sep = "_"),
    IM.92 = IM.90 * exp(t90.95 * 2),
    POB.92 = POB.90 * exp(p90.95 * 2),

    muniYear_93 =paste(CVE_MUN, "1993", sep = "_"),
    IM.93 = IM.90 * exp(t90.95 * 3),
    POB.93 = POB.90 * exp(p90.95 * 3),

    muniYear_94 =paste(CVE_MUN, "1994", sep = "_"),
    IM.94 = IM.90 * exp(t90.95 * 4),
    POB.94 = POB.90 * exp(p90.95 * 4),

    t95.00 = (log(IM.00/IM.95))/5,####
    p95.00 = (log(POB.00/POB.95))/5,

    muniYear_96 =paste(CVE_MUN, "1996", sep = "_"),
    IM.96 = IM.95 * exp(t95.00 * 1),
    POB.96 = POB.95 * exp(p95.00 * 1),

    muniYear_97 =paste(CVE_MUN, "1997", sep = "_"),
    IM.97 = IM.95 * exp(t95.00 * 2),
    POB.97 = POB.95 * exp(p95.00 * 2),

    muniYear_98 =paste(CVE_MUN, "1998", sep = "_"),
    IM.98 = IM.95 * exp(t95.00 * 3),
    POB.98 = POB.95 * exp(p95.00 * 3),

    muniYear_99 =paste(CVE_MUN, "1999", sep = "_"),
    IM.99 = IM.95 * exp(t95.00 * 4),
    POB.99 = POB.95 * exp(p95.00 * 4),

    t00.05 = (log(IM.05/IM.00))/5,####
    p00.05 = (log(POB.05/POB.00))/5,

    muniYear_01 = paste(CVE_MUN, "2001", sep = "_"),
    IM.01 = IM.00 * exp(t00.05 * 1),
    POB.01 = POB.00 * exp(p00.05 * 1),

    muniYear_02 = paste(CVE_MUN, "2002", sep = "_"),
    IM.02 = IM.00 * exp(t00.05 * 2),
    POB.02 = POB.00 * exp(p00.05 * 2),

    muniYear_03 = paste(CVE_MUN, "2003", sep = "_"),
    IM.03 = IM.00 * exp(t00.05 * 3),
    POB.03 = POB.00 * exp(p00.05 * 3),

    muniYear_04 = paste(CVE_MUN, "2004", sep = "_"),
    IM.04 = IM.00 * exp(t00.05 * 4),
    POB.04 = POB.00 * exp(p00.05 * 4),

    t05.10 = (log(IM.10/IM.05))/5,####
    p05.10 = (log(POB.10/POB.05))/5,

    muniYear_06 = paste(CVE_MUN, "2006", sep = "_"),
    IM.06 = IM.00 * exp(t05.10 * 1),
    POB.06 = POB.00 * exp(p05.10 * 1),

    muniYear_07 = paste(CVE_MUN, "2007", sep = "_"),
    IM.07 = IM.00 * exp(t05.10 * 2),
    POB.07 = POB.00 * exp(p05.10 * 2),

    muniYear_08 = paste(CVE_MUN, "2008", sep = "_"),
    IM.08 = IM.00 * exp(t05.10 * 3),
    POB.08 = POB.00 * exp(p05.10 * 3),

    muniYear_09 = paste(CVE_MUN, "2009", sep = "_"),
    IM.09 = IM.00 * exp(t05.10 * 4),
    POB.09 = POB.00 * exp(p05.10 * 4),

    t10.15 = (log(IM.15/IM.10))/5,####
    p10.15 = (log(POB.15/POB.10))/5,

    muniYear_11 = paste(CVE_MUN, "2011", sep = "_"),
    IM.11 = IM.00 * exp(t10.15 * 1),
    POB.11 = POB.00 * exp(p10.15 * 1),

    muniYear_12 = paste(CVE_MUN, "2012", sep = "_"),
    IM.12 = IM.00 * exp(t10.15 * 2),
    POB.12 = POB.00 * exp(p10.15 * 2),

    muniYear_13 = paste(CVE_MUN, "2013", sep = "_"),
    IM.13 = IM.00 * exp(t10.15 * 3),
    POB.13 = POB.00 * exp(p10.15 * 3),

    muniYear_14 = paste(CVE_MUN, "2014", sep = "_"),
    IM.14 = IM.00 * exp(t10.15 * 4),
    POB.14 = POB.00 * exp(p10.15 * 4)
  )

t.91 <- fin %>%
  select(ends_with("91")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.92 <- fin %>%
  select(ends_with("92")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.93 <- fin %>%
  select(ends_with("93")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.94 <- fin %>%
  select(ends_with("94")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.96 <- fin %>%
  select(ends_with("96")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.97 <- fin %>%
  select(ends_with("97")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.98 <- fin %>%
  select(ends_with("98")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.99 <- fin %>%
  select(ends_with("99")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.01 <- fin %>%
  select(ends_with("01")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.02 <- fin %>%
  select(ends_with("02")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.03 <- fin %>%
  select(ends_with("03")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.04 <- fin %>%
  select(ends_with("04")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.06 <- fin %>%
  select(ends_with("06")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.07 <- fin %>%
  select(ends_with("07")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.08 <- fin %>%
  select(ends_with("08")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.09 <- fin %>%
  select(ends_with("09")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.11 <- fin %>%
  select(ends_with("11")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.12 <- fin %>%
  select(ends_with("12")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.13 <- fin %>%
  select(ends_with("13")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

t.14 <- fin %>%
  select(ends_with("14")) %>%
  setNames(c("muniYear", "IM", "POB_TOT"))

pob <- bind_rows(list(mar, t.91, t.92, t.93, t.94, t.96,
                      t.97, t.98, t.99, t.01, t.02, t.03,
                      t.04, t.06, t.07, t.08, t.09, t.11,
                      t.12, t.13, t.14))
pob <- pob %>%
  select(muniYear, POB_TOT) %>%
  mutate(POB_TOT = round(POB_TOT))

as.data.frame(pob, row.names = NULL)

write.csv (pob, paste(out, "poblation.csv", sep = "/"), row.names = F)