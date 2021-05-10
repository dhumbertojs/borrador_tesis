library(dplyr)
library(readxl)
library(tidyr)
library(janitor)
library(imputeTS)
library(stringr)

inp_s <- "/Users/dhjs/Documents/projects/electoral_accountability/datos/servicios"
inp_c <- "/Users/dhjs/Documents/projects/electoral_accountability/datos/bienes"
inp_m <- "/Users/dhjs/Documents/projects/electoral_accountability/datos"

out <- "/Users/dhjs/Documents/projects/electoral_accountability"

# Services ----------------------------------------------------------------
services <- read_excel(paste(inp_s, "SIMBAD_45106_20191009115505598.xlsx", sep = "/"), skip = 3)

clave <- select(services, 1:2)

d1994 <- services %>% select(3: 5) %>% 
  mutate(year = 1994) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1995 <- services %>% select(6: 8) %>% 
  mutate(year = 1995) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1996 <- services %>% select(9:11) %>% 
  mutate(year = 1996) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1997 <- services %>% select(12:14) %>% 
  mutate(year = 1997) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1998 <- services %>% select(15:17) %>% 
  mutate(year = 1998) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d1999 <- services %>% select(18:20) %>% 
  mutate(year = 1999) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2000 <- services %>% select(21:23) %>% 
  mutate(year = 2000) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2001 <- services %>% select(24:26) %>% 
  mutate(year = 2001) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2002 <- services %>% select(27:29) %>% 
  mutate(year = 2002) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2003 <- services %>% select(30:32) %>% 
  mutate(year = 2003) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2004 <- services %>% select(33:35) %>% 
  mutate(year = 2004) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2005 <- services %>% select(36:38) %>% 
  mutate(year = 2005) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2006 <- services %>% select(39:41) %>% 
  mutate(year = 2006) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2007 <- services %>% select(42:44) %>% 
  mutate(year = 2007) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2008 <- services %>% select(45:47) %>% 
  mutate(year = 2008) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2009 <- services %>% select(48:50) %>% 
  mutate(year = 2009) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2010 <- services %>% select(51:53) %>% 
  mutate(year = 2010) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2011 <- services %>% select(54:56) %>% 
  mutate(year = 2011) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2012 <- services %>% select(57:59) %>% 
  mutate(year = 2012) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2013 <- services %>% select(60:62) %>% 
  mutate(year = 2013) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2014 <- services %>% select(63:65) %>% 
  mutate(year = 2014) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )
d2015 <- services %>% select(66:68) %>% 
  mutate(year = 2015) %>% bind_cols(clave) %>% 
  rename(agua = `Tomas domiciliarias de agua entubada a/`, 
         dren = `Sistemas de drenaje y alcantarillado a/`, 
         elec = `Tomas instaladas de energía eléctrica a/`) %>% 
  mutate(
    agua = as.character(agua),
    dren = as.character(dren),
    elec = as.character(elec)
  )

fin_s <- bind_rows(list(d1994, d1995, d1996, d1997, d1998, d1999, d2000,
                      d2001, d2002, d2003, d2004, d2005, d2006, d2007,
                      d2008, d2009, d2010, d2011, d2012, d2013, d2014, d2015))

fin_s <- fin_s %>% 
  select(Clave, year, agua, dren, elec) %>% 
  mutate(
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0)))))
  ) %>% filter(edos != 1 & quit != 1) %>%
  select(-c("edos", "quit"))

fin_s <- fin_s %>% 
  mutate(
    agua = ifelse(agua == "ND", NA, agua),
    agua = as.numeric(agua),
    dren = ifelse(dren == "ND", NA, dren),
    dren = as.numeric(dren),
    elec = ifelse(elec == "ND", NA, elec),
    elec = as.numeric(elec),
    
    muniYear = paste(Clave, year, sep = "_")
  ) %>% 
  arrange(muniYear) %>% 
  select(muniYear, agua, dren, elec)

fin_s_impute <- fin_s %>% 
  mutate(
    muni = str_sub(muniYear, 1,5)
  ) %>% 
  group_by(muni) %>% 
  na_ma(weighting = "exponential", k = 1) %>% 
  ungroup() %>% 
  select(-muni)

# Crime -------------------------------------------------------------------

data <- read_excel(paste(inp_c, "SIMBAD_19046_20191010012052836.xlsx", sep = "/"), skip = 3)

clave <- select(data, 1:2)

d1994 <- data %>% select(3:4) %>% 
  mutate(year = 1994) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1995 <- data %>% select(5:6) %>% 
  mutate(year = 1995) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1996 <- data %>% select(7:8) %>% 
  mutate(year = 1996) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1997 <- data %>% select(9:10) %>% 
  mutate(year = 1997) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1998 <- data %>% select(11:12) %>% 
  mutate(year = 1998) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d1999 <- data %>% select(13:14) %>% 
  mutate(year = 1999) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2000 <- data %>% select(15:16) %>% 
  mutate(year = 2000) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2001 <- data %>% select(17:18) %>% 
  mutate(year = 2001) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2002 <- data %>% select(19:20) %>% 
  mutate(year = 2002) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2003 <- data %>% select(21:22) %>% 
  mutate(year = 2003) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2004 <- data %>% select(23:24) %>% 
  mutate(year = 2004) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2005 <- data %>% select(25:26) %>% 
  mutate(year = 2005) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2006 <- data %>% select(27:28) %>% 
  mutate(year = 2006) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2007 <- data %>% select(29:30) %>% 
  mutate(year = 2007) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2008 <- data %>% select(31:32) %>% 
  mutate(year = 2008) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2009 <- data %>% select(33:34) %>% 
  mutate(year = 2009) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )
d2010 <- data %>% select(35:36) %>% 
  mutate(year = 2010) %>% bind_cols(clave) %>% 
  rename(tot_del = `Total delitos`, 
         hom = `Homicidio`) %>% 
  mutate(
    tot_del = as.character(tot_del),
    hom = as.character(hom)
  )

fin <- bind_rows(list(d1994, d1995, d1996, d1997, d1998, d1999, d2000,
                      d2001, d2002, d2003, d2004, d2005, d2006, d2007, 
                      d2008, d2009, d2010))

fin <- fin %>% 
  select(Clave, year, tot_del, hom) %>% 
  mutate(
    edos = ifelse(nchar(Clave) == 2, 1, 0),
    quit = ifelse(substr(Clave, 3, 5) == "995" , 1, 
                  ifelse(substr(Clave, 3, 5) == "996" , 1 , 
                         ifelse(substr(Clave, 3, 5) == "997" , 1, 
                                ifelse(substr(Clave, 3, 5) == "998" , 1, 
                                       ifelse(substr(Clave, 3, 5) == "999", 1, 0)))))
  ) %>% filter(edos != 1 & quit != 1) %>%
  select(-c("edos", "quit"))

fin <- fin %>% 
  mutate(
    tot_del = ifelse(tot_del == "ND", NA, tot_del),
    tot_del = as.numeric(tot_del),
    hom = ifelse(hom == "ND", NA, hom),
    hom = as.numeric(hom),
    
    muniYear = paste(Clave, year, sep = "_")
  ) %>% 
  arrange(muniYear) %>% 
  select(muniYear, tot_del, hom)

fin_impute <- fin %>% 
  mutate(
    muni = str_sub(muniYear, 1,5)
  ) %>% 
  group_by(muni) %>% 
  na_ma(weighting = "exponential", k = 1) %>% 
  ungroup() %>% 
  select(-muni)

# Margination -------------------------------------------------------------

mar <- read.csv(paste(inp_m, "Base_Indice_de_marginacion_municipal_90-15.csv", sep = "/"), fileEncoding = "latin1", stringsAsFactors = F)

mar <- mar %>% 
  filter(ENT != "Nacional") %>% 
  select(CVE_MUN, IM, POB_TOT, AÑO) %>% 
  mutate(
    IM = as.numeric(IM),
    POB_TOT = as.numeric(POB_TOT),
    CVE_MUN = str_pad(CVE_MUN, width = 5, side = "left", pad = "0"),
    muniYear = paste(CVE_MUN, AÑO, sep = "_")
  )

# # state -------------------------------------------------------------------
# edo_s <- fin_s %>%
#   mutate(
#     muni = str_sub(muniYear, 1, 5),
#     edo = str_sub(muniYear, 1, 2)
#   ) %>%
#   group_by(muni) %>%
#   mutate(
#     v_a = ifelse(is.na(agua), 1, 0),
#     v_d = ifelse(is.na(dren), 1, 0),
#     v_e = ifelse(is.na(elec), 1, 0),
#     tot_s = n()
#   ) %>%
#   ungroup() %>% 
#   filter(edo != "33" & edo != "C_")
# 
# check_es <- edo_s %>% 
#   group_by(edo, muni, tot_s) %>% 
#   summarise(
#     v_a = sum(v_a),
#     v_d = sum(v_d),
#     v_e = sum(v_e)
#   ) %>% 
#   ungroup() %>% 
#   # mutate(
#   #   v_a = v_a*100/tot,
#   #   v_d = v_d*100/tot,
#   #   v_e = v_e*100/tot
#   # ) %>% 
#   # select(-tot) %>% #Sobre esta línea tendría que separar para revisar los municipios
#   group_by(edo) %>% 
#   summarise(
#     tot_s = mean(tot_s),
#     
#     min_a = round(min(v_a),2),
#     max_a = round(max(v_a),2),
#     mean_a = round(mean(v_a),2),
#     std_a = round(sd(v_a),2),
#     
#     min_d = round(min(v_d),2),
#     max_d = round(max(v_d),2),
#     mean_d = round(mean(v_d),2),
#     std_d = round(sd(v_d),2),
#     
#     min_e = round(min(v_e),2),
#     max_e = round(max(v_e),2),
#     mean_e = round(mean(v_e),2),
#     std_e = round(sd(v_e),2)
#   ) 
# 
# edo_c <- fin %>%
#   mutate(
#     muni = str_sub(muniYear, 1, 5),
#     edo = str_sub(muniYear, 1, 2)
#   ) %>%
#   group_by(muni) %>%
#   mutate(
#     v_td = ifelse(is.na(tot_del), 1, 0),
#     v_h = ifelse(is.na(hom), 1, 0),
#     tot_c = n()
#   ) %>%
#   ungroup() %>% 
#   filter(edo != "33" & edo != "C_")
# 
# check_ec <- edo_c %>% 
#   group_by(edo, muni, tot_c) %>% 
#   summarise(
#     v_td = sum(v_td),
#     v_h = sum(v_h)
#   ) %>% 
#   ungroup() %>% 
#   # mutate(
#   #   v_a = v_a*100/tot,
#   #   v_d = v_d*100/tot,
#   #   v_e = v_e*100/tot
#   # ) %>% 
#   # select(-tot) %>% #Sobre esta línea tendría que separar para revisar los municipios
#   group_by(edo) %>% 
#   summarise(
#     tot_c = mean(tot_c),
#     
#     min_td = round(min(v_td),2),
#     max_td = round(max(v_td),2),
#     mean_td = round(mean(v_td),2),
#     std_td = round(sd(v_td),2),
#     
#     min_h = round(min(v_h),2),
#     max_h = round(max(v_h),2),
#     mean_h = round(mean(v_h),2),
#     sth_h = round(sd(v_h),2)
#   ) 
# 
# estado <- left_join(check_es, check_ec, by = "edo")
# 
# # Decade ------------------------------------------------------------------
# 
# dec_s <- fin_s %>% 
#   mutate(
#     edo = str_sub(muniYear, 1, 2),
#     year = str_sub(muniYear, 7, 10),
#     
#     decada = case_when(
#       year >= 1994 & year < 2000 ~ 1,
#       year >= 2000 & year < 2010 ~ 2,
#       year >= 2010 ~ 3
#     )
#   ) %>% 
#   group_by(decada) %>% 
#   mutate(
#     v_a = ifelse(is.na(agua), 1, 0),
#     v_d = ifelse(is.na(dren), 1, 0),
#     v_e = ifelse(is.na(elec), 1, 0)
#   ) %>%
#   ungroup() %>% 
#   group_by(decada, edo) %>% 
#   mutate(
#     tot_s = n()
#   ) %>% 
#   ungroup() %>% 
#   filter(edo != "33" & edo != "C_")
# 
# check_ds <- dec_s %>% 
#   group_by(edo, decada, tot_s) %>% 
#   summarise(
#     v_a = sum(v_a),
#     v_d = sum(v_d),
#     v_e = sum(v_e)
#   ) %>% 
#   ungroup() %>% 
#   # mutate(
#   #   v_a = v_a*100/tot,
#   #   v_d = v_d*100/tot,
#   #   v_e = v_e*100/tot
#   # ) %>% 
#   # select(-tot) %>% #Sobre esta línea tendría que separar para revisar los municipios
#   group_by(edo, decada) %>% 
#   summarise(
#     tot_s = tot_s,
#     min_a = round(min(v_a),2),
#     max_a = round(max(v_a),2),
#     mean_a = round(mean(v_a),2),
#     #std_a = round(sd(v_a),2),
#     
#     min_d = round(min(v_d),2),
#     max_d = round(max(v_d),2),
#     mean_d = round(mean(v_d),2),
#     #std_d = round(sd(v_d),2),
#     
#     min_e = round(min(v_e),2),
#     max_e = round(max(v_e),2),
#     mean_e = round(mean(v_e),2),
#     #std_e = round(sd(v_e),2)
#   ) 
# 
# dec_c <- fin %>% 
#   mutate(
#     edo = str_sub(muniYear, 1, 2),
#     year = str_sub(muniYear, 7, 10),
#     
#     decada = case_when(
#       year >= 1994 & year < 2000 ~ 1,
#       year >= 2000 & year < 2010 ~ 2,
#       year >= 2010 ~ 3
#     )
#   ) %>% 
#   group_by(decada) %>% 
#   mutate(
#     v_td = ifelse(is.na(tot_del), 1, 0),
#     v_h = ifelse(is.na(hom), 1, 0)
#   ) %>%
#   ungroup() %>% 
#   group_by(decada, edo) %>% 
#   mutate(
#     tot_c = n()
#   ) %>% 
#   ungroup() %>% 
#   filter(edo != "33" & edo != "C_")
# 
# check_dc <- dec_c %>% 
#   group_by(edo, decada, tot_c) %>% 
#   summarise(
#     v_td = sum(v_td),
#     v_h = sum(v_h)
#   ) %>% 
#   ungroup() %>% 
#   # mutate(
#   #   v_a = v_a*100/tot,
#   #   v_d = v_d*100/tot,
#   #   v_e = v_e*100/tot
#   # ) %>% 
#   # select(-tot) %>% #Sobre esta línea tendría que separar para revisar los municipios
#   group_by(edo, decada) %>% 
#   summarise(
#     tot_c = tot_c,
#     
#     min_td = round(min(v_td),2),
#     max_td = round(max(v_td),2),
#     mean_td = round(mean(v_td),2),
#     #std_td = round(sd(v_td),2),
#     
#     min_h = round(min(v_h),2),
#     max_h = round(max(v_h),2),
#     mean_h = round(mean(v_h),2),
#     #sth_h = round(sd(v_h),2)
#   ) 
# 
# decada <- left_join(check_ds, check_dc)
# 
# 
# # save --------------------------------------------------------------------
# 
# names <- tidyr::tibble(
#   edo = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
#           "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
#           "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
#           "31", "32"),
#   entidad = c("Aguascalientes",
#               "Baja California",
#               "Baja California Sur",
#               "Campeche",
#               "Coahuila", 
#               "Colima",
#               "Chiapas",
#               "Chihuahua",
#               "CDMX",
#               "Durango",
#               "Guanajuato",
#               "Guerrero",
#               "Hidalgo",
#               "Jalisco",
#               "Estado de México",
#               "Michoacán", 
#               "Morelos", 
#               "Nayarit", 
#               "Nuevo León",
#               "Oaxaca",
#               "Puebla", 
#               "Querétaro",
#               "Quintana Roo",
#               "San Luis Potosí",
#               "Sinaloa",
#               "Sonora",
#               "Tabasco", 
#               "Tamaulipas",
#               "Tlaxcala",
#               "Veracruz",
#               "Yucatán",
#               "Zacatecas")
# )
#   
# 
# estado <- left_join(names, estado)
# decada <- left_join(names, decada)
# 
# write.csv(estado, paste(out, "edo.csv", sep = "/"), row.names = F)
# write.csv(decada, paste(out, "dec.csv", sep = "/"), row.names = F)

# Electoral ---------------------------------------------------------------

ie <- "/Users/dhjs/Documents/projects/electoral_accountability/databases"
ele <- read.csv(paste(ie, "Electoral.csv", sep = "/"))


ele_original <- ele %>% 
  filter(year>= 1994) %>% 
  left_join(fin) %>% 
  left_join(fin_s) %>% 
  left_join(mar) %>% 
  mutate(
    decada = case_when(
      year < 2000 ~ "1",
      year >= 2000 & year < 2010 ~ "2",
      year >= 2010 ~ "3",
      T ~ "-"
    ),
    st_d = paste(state, decada, sep = "_")
  ) %>% 
  filter(!is.na(inc.ch)) %>% 
  group_by(st_d) %>% 
  summarise(
    agua_original = sum(!is.na(agua)),
    elec_original = sum(!is.na(elec)),
    dren_original = sum(!is.na(dren)),
    del_original = sum(!is.na(tot_del)),
    hom_original = sum(!is.na(dren))
  )


ele_imp <- ele %>% 
  filter(year>= 1994) %>% 
  left_join(fin_impute) %>% 
  left_join(fin_s_impute) %>% 
  left_join(mar) %>% 
  mutate(
    decada = case_when(
      year < 2000 ~ "1",
      year >= 2000 & year < 2010 ~ "2",
      year >= 2010 ~ "3",
      T ~ "-"
    ),
    st_d = paste(state, decada, sep = "_")
  ) %>% 
  filter(!is.na(inc.ch)) %>% 
  ungroup() %>% 
  group_by(st_d) %>% 
  summarise(
    agua_impute = sum(!is.na(agua)),
    elec_impute = sum(!is.na(elec)),
    dren_impute = sum(!is.na(dren)),
    del_impute = sum(!is.na(tot_del)),
    hom_impute = sum(!is.na(dren))
  )

electoral <- left_join(ele_original, ele_imp) %>% 
  select(st_d, agua_original, agua_impute, 
         elec_original, elec_impute,
         dren_original, dren_impute,
         del_original, del_impute, 
         hom_original, hom_impute)

openxlsx::write.xlsx(electoral, file = paste(out, "observations.xlsx", sep = "/"))
