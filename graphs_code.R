rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)

inp <- "/home/dhjs/Documentos/R_projects/electoral_accountability"
list.files(inp)
out <- "/home/dhjs/Documentos/R_projects/electoral_accountability/graphs"

data <- read.csv(paste(inp, "final.csv", sep = "/"))

party <- c("PAN" = "#153588", "PRI" = "#E13A27", "PRD" = "#F6D626")

ggplot(data) +
  geom_jitter(aes(x = ch.agua, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")

ggplot(data) +
  geom_jitter(aes(x = ch.dren, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")

ggplot(data) +
  geom_jitter(aes(x = ch.elec, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")

ggplot(data) +
  geom_jitter(aes(x = ch.del, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")

ggplot(data) +
  geom_jitter(aes(x = ch.hom, y = inc.share, col = inc_top)) +
  scale_colour_manual(values = party, name = "Partido")
