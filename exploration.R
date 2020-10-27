rm(list = ls())
setwd("~")

library(dplyr)
library(ggplot2)

inp <- "/Users/dhjs/Documents/projects/electoral_accountability/alternative"
list.files(inp)

data <- read.csv(paste(inp, "final.csv", sep = "/"))
nrow(data) #17,023
data <- data %>% 
  filter(!is.na(inc_top) & # & !is.na(alt)
           inc_top != "Otros"
  ) %>% 
  mutate(
    inc.ch = inc.ch*100
  )
nrow(data)
#13890
summary(data)

party <- c("PAN" = "#153588", "PRI" = "#E13A27", 
           "PRD" = "#F6D626", "Otros" = "black")
#variables de interes

datac <- data %>% 
  mutate(
    ch.agua = ifelse(ch.agua <= quantile(ch.agua, 0.975, na.rm = T) & 
                       ch.agua >= quantile(ch.agua, 0.025, na.rm = T), ch.agua, NA),
    
    ch.dren = ifelse(ch.dren <= quantile(ch.dren, 0.975, na.rm = T) &
                       ch.dren >= quantile(ch.dren, 0.025, na.rm = T) , ch.dren, NA),
    
    ch.elec = ifelse(ch.elec <= quantile(ch.elec, 0.975, na.rm = T) &
                       ch.elec >= quantile(ch.elec, 0.025, na.rm = T), ch.elec, NA),
    
    ch.del = ifelse(ch.del <= quantile(ch.del, 0.975, na.rm = T) &
                      ch.del >= quantile(ch.del, 0.025, na.rm = T), ch.del, NA),
    
    ch.hom = ifelse(ch.hom <= quantile(ch.hom, 0.975, na.rm = T) & 
                      ch.hom >= quantile(ch.hom, 0.025, na.rm = T), ch.hom, NA)
  )

ggplot(data, aes(x = ch.agua, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)
   
ggplot(datac, aes(x = ch.agua, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)


ggplot(data, aes(x = ch.dren, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)

ggplot(datac, aes(x = ch.dren, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)


ggplot(data, aes(x = ch.elec, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)

ggplot(datac, aes(x = ch.elec, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)

ggplot(data, aes(x = ch.del, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)

ggplot(datac, aes(x = ch.del, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)


ggplot(data, aes(x = ch.hom, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)

ggplot(datac, aes(x = ch.hom, y = inc.ch, col = inc_top)) +
  geom_jitter() +
  ylim(-100, 100) +
  xlim(-100,100) +
  geom_smooth(method = "lm", se = T, color = "black") +
  scale_colour_manual(values = party, name = "Partido") +
  facet_grid(. ~ inc_top)
#inc.ch
#inc_top

ggplot(data, aes(x = inc_top, y = inc.ch)) +
  geom_violin() +
  stat_summary(fun.y = "mean", geom = "point") +
  ylim(-100,100)+
  geom_hline(aes(yintercept = mean(data$inc.ch, na.rm = T))) +
  facet_wrap(. ~ alt)

ggplot(data, aes(x = inc_top, y = ch.agua)) +
  geom_violin() +
  stat_summary(fun.y = "mean", geom = "point") +
  ylim(-100,100)+
  geom_hline(aes(yintercept = mean(data$ch.agua, na.rm = T))) +
  facet_wrap(. ~ alt)

ggplot(data, aes(x = inc_top, y = ch.elec)) +
  geom_violin() +
  stat_summary(fun.y = "mean", geom = "point") +
  ylim(-100,100)+
  geom_hline(aes(yintercept = mean(data$ch.elec, na.rm = T))) +
  facet_wrap(. ~ alt)

ggplot(data, aes(x = inc_top, y = ch.dren)) +
  geom_violin() +
  stat_summary(fun.y = "mean", geom = "point") +
  ylim(-100,100)+
  geom_hline(aes(yintercept = mean(data$ch.dren, na.rm = T))) +
  facet_wrap(. ~ alt)

ggplot(data, aes(x = inc_top, y = ch.del)) +
  geom_violin() +
  stat_summary(fun.y = "mean", geom = "point") +
  ylim(-100,100)+
  geom_hline(aes(yintercept = mean(data$ch.del, na.rm = T))) +
  facet_wrap(. ~ alt)

ggplot(data, aes(x = inc_top, y = ch.hom)) +
  geom_violin() +
  stat_summary(fun.y = "mean", geom = "point") +
  ylim(-100,100)+
  geom_hline(aes(yintercept = mean(data$ch.hom, na.rm = T))) +
  facet_wrap(. ~ alt)

da <- data %>% 
  mutate(cont = 1) %>% 
  group_by(year, alt) %>% 
  summarise(
    tot = sum(cont)
  ) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(
    tyear = sum(tot), 
    porc = tot*100/tyear
  ) %>% 
  ungroup() %>% 
  select(year, alt, porc)

data %>% 
  mutate(cont = 1) %>% 
  group_by(win_top) %>% 
  summarise(tot = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!is.na(win_top)) %>% 
  mutate(
    total = sum(tot),
    porc = tot*100/total
  )
