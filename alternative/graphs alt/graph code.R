rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)
library(referenceIntervals)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability/alternative"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/alternative/graphs alt"

data <- read.csv(paste(inp, "final.csv", sep = "/"))
#12171 observaciones

#Quitar outliers
t.agua <- cook.outliers(data$ch.agua)
t.dren <- cook.outliers(data$ch.dren)
t.elec <- cook.outliers(data$ch.elec)
t.hom <- cook.outliers(data$ch.hom)
t.del <- cook.outliers(data$ch.del)

summary(t.agua$outliers)
#min - 2491.5 , max -122.4
#Quitaría 72 outliers
quantile(data$ch.agua, probs = c(0.01, 0.05, 0.1), na.rm = T)
#1% -125.90696

summary(t.dren$outliers)
#min -25517 , max -1000
#quitar 7 outliers
quantile(data$ch.dren, probs = c(0.01, 0.05, 0.1), na.rm = T)
#1% -270.72414

summary(t.elec$outliers)
#min -9680 , max -305.8
#quitar 21 out
quantile(data$ch.elec, probs = c(0.01, 0.05, 0.1), na.rm = T)
#1% -84.4955

summary(t.hom$outliers)
#min -7872.8 , max -593.2
#quitar 101 out
quantile(data$ch.hom, probs = c(0.01, 0.05, 0.1), na.rm = T)
#1% 5% -336.2261

summary(t.del$outliers)
#min -19611 , max -861.6
#quitar 31
quantile(data$ch.del, probs = c(0.01, 0.05, 0.1), na.rm = T)
#1% -546.40809

party <- c("PAN" = "#153588", "PRI" = "#E13A27", "PRD" = "#F6D626")

# data <- data %>% 
#   filter(
#     ch.agua >= -100 &
#       ch.dren >= -100 & 
#       ch.elec >= -100 &
#       ch.hom >= -100 &
#       ch.del >= -100
#   ) #quedan 697 observaciones

# data <- data %>% 
#   filter(
#     ch.hom != t.hom$outliers
#   ) #Solo quitando autliers de homicidios, quedan 3581 observaciones


# Diagramas de dispersión -------------------------------------------------

ggplot(data, aes(x = ch.agua, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F)
ggsave("point_ch.agua.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.dren, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F)
ggsave("point_ch.dren.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.elec, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F)
ggsave("point_ch.elec.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.del, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F)
ggsave("point_ch.del.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.hom, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F)
ggsave("point_ch.hom.png", path = out, dpi = 300)

#Por alternancia ####
ggplot(data, aes(x = ch.agua, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
ggsave("point_alt_ch.agua.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.dren, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
ggsave("point_alt_ch.dren.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.elec, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
ggsave("point_alt_ch.elec.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.del, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
ggsave("point_alt_ch.del.png", path = out, dpi = 300)

ggplot(data, aes(x = ch.hom, y = inc.share, col = inc_top)) +
  geom_jitter(alpha = 0.3) +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
ggsave("point_alt_ch.hom.png", path = out, dpi = 300)


# Boxplot por alternancia -----------------------------------------------------------------

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.agua)) +
  facet_grid(. ~ alt)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.dren)) +
  facet_grid(. ~ alt)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.elec)) +
  facet_grid(. ~ alt)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.del)) +
  facet_grid(. ~ alt)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.hom)) +
  facet_grid(. ~ alt)


# Boxplot por partido -----------------------------------------------------

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.agua)) +
  facet_grid(. ~ inc_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.dren)) +
  facet_grid(. ~ inc_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.elec)) +
  facet_grid(. ~ inc_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.del)) +
  facet_grid(. ~ inc_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.hom)) +
  facet_grid(. ~ inc_top)


# random ------------------------------------------------------------------

dada <- data %>% 
  select(inc_top, inc.share, year, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, alt) %>% 
  group_by(inc_top, year, alt) %>% 
  summarise_all(mean, na.r = T)

ggplot(dada) +
  geom_line(aes(x = year, y = inc.share, group = inc_top, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ alt)
ggsave("lines_incumbent_share.png", path = out, dpi = 300)

# try <- data %>% 
#   select(inc_top, inc.share, year, ch.agua, ch.dren, ch.elec, ch.del, ch.hom, alt, state) %>% 
#   group_by(inc_top, year, alt, state) %>% 
#   summarise_all(mean, na.r = T)
# ggplot(try, aes(x = year, y = inc.share, group = inc_top, col = inc_top)) +
#   geom_line() +
#   scale_colour_manual(values = party, name = "Partido") +
#   facet_grid(alt ~ state)
# ggsave("lines_incumbent_share_state.png", path = out, width = 12, height = 8, units = "in", dpi = 320)

####
ggplot(dada, aes(x = ch.agua, y = inc.share, col = inc_top)) +
  geom_jitter() +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)

ggplot(dada, aes(x = ch.dren, y = inc.share, col = inc_top)) +
  geom_jitter() +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)

ggplot(dada, aes(x = ch.elec, y = inc.share, col = inc_top)) +
  geom_jitter() +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)

ggplot(dada, aes(x = ch.del, y = inc.share, col = inc_top)) +
  geom_jitter() +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)

ggplot(dada, aes(x = ch.hom, y = inc.share, col = inc_top)) +
  geom_jitter() +
  scale_colour_manual(values = party, name = "Partido") +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ alt)
beepr::beep(2)