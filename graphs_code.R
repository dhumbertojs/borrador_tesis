rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/graphs"

data <- read.csv(paste(inp, "final.csv", sep = "/"))

party <- c("PAN" = "#153588", "PRI" = "#E13A27", "PRD" = "#F6D626")

# Diagramas de dispersión -------------------------------------------------

ggplot(data) +
  geom_jitter(aes(x = ch.agua, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")
ggsave("point_ch.agua.png", path = out, dpi = 300)

ggplot(data) +
  geom_jitter(aes(x = ch.dren, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")
ggsave("point_ch.dren.png", path = out, dpi = 300)

ggplot(data) +
  geom_jitter(aes(x = ch.elec, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")
ggsave("point_ch.elec.png", path = out, dpi = 300)

ggplot(data) +
  geom_jitter(aes(x = ch.del, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")
ggsave("point_ch.del.png", path = out, dpi = 300)

ggplot(data) +
  geom_jitter(aes(x = ch.hom, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")
ggsave("point_ch.hom.png", path = out, dpi = 300)


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
  facet_grid(. ~ win_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.dren)) +
  facet_grid(. ~ win_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.elec)) +
  facet_grid(. ~ win_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.del)) +
  facet_grid(. ~ win_top)

ggplot(data) +
  geom_boxplot(aes(x = as.factor(year), y = ch.hom)) +
  facet_grid(. ~ win_top)

