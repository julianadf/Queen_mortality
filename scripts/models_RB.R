rm(list=ls())
library(here)
library(tidyverse)
library(lattice)
library(permute)
library(vegan)
library(ggpubr)
library(effects)
library(visreg)
library(DHARMa)
library(MASS)
library(lme4)
library(car)
library(emmeans)

# Load and check data ----
queen.data <- read.csv2(here("Database.csv" ))

# Take away non-queen observations
queens <- queen.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Roadverge = as.factor(Roadverge)) %>% 
  filter(Caste == "queen")
str(queens)

# Riccardos model
queen.behaviour.0 <- queens %>% 
  group_by(Site, Roadverge, Traffic, Behaviour) %>% 
  summarise(behaviour.count = n()) 
queen.behaviour.0

# queen.behaviour <- queen.behaviour.0 %>% 
#   filter(Behaviour %in% c("dead", "foraging", "nesting", "passing")) %>% 
#   mutate(Behaviour = as.factor(Behaviour))
# queen.behaviour
# 
# behaviour.mod <- glmer(behaviour.count ~ Behaviour * Roadverge * scale(Traffic) + (1|Site), family = "poisson", data=queen.behaviour)
# summary(behaviour.mod)
# mod_dharma1 <- behaviour.mod %>% simulateResiduals(n=1000)
# plot(mod_dharma1)
# plot(allEffects(behaviour.mod))
# mod_dharma1 %>% testDispersion()
# mod_dharma1 %>% testZeroInflation()

queen.behaviour <- queen.behaviour.0 %>% 
  filter(Behaviour %in% c("dead", "foraging", "nesting")) %>% 
  mutate(Behaviour = as.factor(Behaviour)) 
queen.behaviour

queen.behaviour$obs <- c(1:54)

behaviour.mod <- glmer(behaviour.count ~ Behaviour * Roadverge * scale(Traffic) + (1|Site) + (1|obs), family = "poisson", data=queen.behaviour)
summary(behaviour.mod)
mod_dharma1 <- behaviour.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(behaviour.mod))
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()

queenag <- aggregate(cbind(Visit) ~ Species, data = queens, sum, na.rm=T)

behaviour.mod <- glmer(behaviour.count ~ Behaviour * Roadverge * scale(Traffic) + (1|Site), family = "poisson", data=queen.behaviour)
summary(behaviour.mod)
mod_dharma1 <- behaviour.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(behaviour.mod))
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()

# Does traffic and road verge quality affect queen mortality?
queen.mortality <- queens %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
queen.mortality

mortality.mod <- glm(dead ~ Roadverge + scale(Traffic) + Species, family = "binomial", data=queen.mortality)
summary(mortality.mod)
Anova(mortality.mod, type=3)
mod_dharma1 <- mortality.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(mortality.mod))
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()


# Look only at live bumblebees
live.queens <- queens %>% 
  filter(Behaviour != "dead")
live.queens

# foraging
foraging.bin <- live.queens %>% 
  mutate(forager = ifelse(Behaviour =="foraging", 1, 0))
foraging.bin

forage.bin.mod <- glm(forager ~ Roadverge * scale(Traffic) , family = "binomial", data=foraging.bin)
summary(forage.bin.mod)
mod_dharma1 <- forage.bin.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
visreg(forage.bin.mod, "Traffic", by="Roadverge", scale="response")

# nesting
nesting.bin <- live.queens %>% 
  mutate(nester = ifelse(Behaviour =="nesting", 1, 0))
nesting.bin

nesting.bin.mod <- glm(nester ~ Roadverge * scale(Traffic) , family = "binomial", data=nesting.bin)
summary(forage.bin.mod)
mod_dharma1 <- nesting.bin.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
visreg(nesting.bin.mod, "Traffic", by="Roadverge", scale="response")

# Does traffic and road verge quality affect foraging behaviour?
foragers <- queens %>% 
  filter(Behaviour == "foraging") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(foraging = n()) 
foragers

forage.mod <- glm(foraging ~ Roadverge * scale(Traffic) , family = "quasipoisson", data=foragers)
summary(forage.mod)
plot(allEffects(forage.mod))

# Does traffic and road verge quality affect nesting behaviour?
nesters <- queens %>% 
  filter(Behaviour == "nesting") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(nesting = n()) %>% 
  rows_insert(., tibble(Site ="Gottr?ra", Roadverge="SP", Traffic =3718, nesting=0)) %>%
  rows_insert(., tibble(Site ="Rimbo", Roadverge="SP", Traffic =939, nesting=0)) %>%
  rows_insert(., tibble(Site ="Rovs?ttra", Roadverge="SR", Traffic =267, nesting=0)) %>% 
  rows_insert(., tibble(Site ="Toster?n", Roadverge="SP", Traffic =5764, nesting=0)) %>%
  rows_insert(., tibble(Site ="Upplands-V?sby", Roadverge="SP", Traffic =6168, nesting=0)) 
nesters

nesting.mod <- glm(nesting ~ Roadverge * scale(Traffic) , family = "poisson", data=nesters)
summary(nesting.mod)
plot(allEffects(nesting.mod))
visreg(nesting.mod, "Traffic", by="Roadverge", scale="response")
mod_dharma2 <- nesting.mod %>% simulateResiduals(n=1000)
plot(mod_dharma2)
mod_dharma2 %>% testDispersion()
mod_dharma2 %>% testZeroInflation()
