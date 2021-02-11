rm(list=ls())
library(here)
library(tidyverse)
library(vegan)
library(lattice)
library(ggpubr)
library(effects)
library(visreg)
library(DHARMa)
library(MASS)
library(lme4)
library(car)
library(emmeans)

# Load and check data ----
queen.data <- read.csv2(here("data", "Database.csv" ))


# Take away non-queen observations
queens <- queen.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Roadverge = as.factor(Roadverge)) %>% 
  filter(Caste == "queen")
str(queens)
# GLMM with species as random effect
queen.mortality <- queens %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
queen.mortality

mortality.glmm <- glmer(dead ~ Roadverge + scale(Traffic) + (1| Species), family = "binomial", data=queen.mortality)
summary(mortality.glmm)
mod_dharma1 <- mortality.glmm %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(mortality.glmm))
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()

# Bombus terrestris ----
terrestris <- queens %>% 
  filter(Species == "B. terrestris") 
terrestris

# Does traffic and road verge quality affect queen mortality?
terrestris.dead <- terrestris %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
terrestris.dead

mortality.mod <- glm(dead ~ Roadverge * scale(Traffic), family = "binomial", data=terrestris.dead)
summary(mortality.mod)
mod_dharma1 <- mortality.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(mortality.mod, "Traffic", by="Roadverge", scale="response")

# Does traffic and road verge quality affect foraging behaviour?
terrestris.foragers <- terrestris %>% 
  filter(Behaviour == "foraging") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(foraging = n()) %>% 
  rows_insert(., tibble(Site ="Skogs-tibble", Roadverge="SP", Traffic =158, foraging=0)) %>%
  rows_insert(., tibble(Site ="Tierp", Roadverge="SR", Traffic =125, foraging=0)) %>%
  rows_insert(., tibble(Site ="Vänge", Roadverge="SP", Traffic =562, foraging=0)) %>%
  rows_insert(., tibble(Site ="Enköping", Roadverge="SP", Traffic =841, foraging=0)) %>%
  rows_insert(., tibble(Site ="Skutskär", Roadverge="SR", Traffic =930, foraging=0)) %>%
  rows_insert(., tibble(Site ="Hargshamn", Roadverge="SR", Traffic =1726, foraging=0)) %>%
  rows_insert(., tibble(Site ="Östhammar", Roadverge="SP", Traffic =1662, foraging=0)) %>%
  rows_insert(., tibble(Site ="Aspön", Roadverge="SR", Traffic =5675, foraging=0)) %>%
  rows_insert(., tibble(Site ="Tosterön", Roadverge="SP", Traffic =5764, foraging=0)) %>%
  rows_insert(., tibble(Site ="Sigtuna", Roadverge="SR", Traffic =6356, foraging=0))
terrestris.foragers

terrestris.forage.mod <- glm(foraging ~ Roadverge * scale(Traffic) , family = "poisson", data=terrestris.foragers)
summary(terrestris.forage.mod)
mod_dharma1 <- terrestris.forage.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(terrestris.forage.mod))

# Does traffic and road verge quality affect nesting behaviour?
terrestris.nesters <- terrestris %>% 
  filter(Behaviour == "nesting") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(nesting = n()) %>% 
  rows_insert(., tibble(Site ="Rovsättra", Roadverge="SR", Traffic =267, nesting=0)) %>%
  rows_insert(., tibble(Site ="Skärplinge", Roadverge="SR", Traffic =570, nesting=0)) %>%
  rows_insert(., tibble(Site ="Skutskär", Roadverge="SR", Traffic =930, nesting=0)) %>%
  rows_insert(., tibble(Site ="Rimbo", Roadverge="SP", Traffic =939, nesting=0)) %>%
  rows_insert(., tibble(Site ="Hargshamn", Roadverge="SR", Traffic =1726, nesting=0)) %>%
  rows_insert(., tibble(Site ="Östhammar", Roadverge="SP", Traffic =1662, nesting=0)) %>%
  rows_insert(., tibble(Site ="Gottröra", Roadverge="SP", Traffic =3718, nesting=0)) %>%
  rows_insert(., tibble(Site ="Tosterön", Roadverge="SP", Traffic =5764, nesting=0)) %>%
  rows_insert(., tibble(Site ="Upplands-Väsby", Roadverge="SP", Traffic =6168, nesting=0)) 
terrestris.nesters

terrestris.nesting.mod <- glm(nesting ~ Roadverge * scale(Traffic) , family = "poisson", data=terrestris.nesters)
summary(terrestris.nesting.mod)
visreg(terrestris.nesting.mod, "Traffic", by="Roadverge", scale="response")
mod_dharma2 <- terrestris.nesting.mod %>% simulateResiduals(n=1000)
plot(mod_dharma2)
mod_dharma2 %>% testDispersion()
mod_dharma2 %>% testZeroInflation()

# Look only at live bumblebees
live.terrestris <- terrestris %>% 
  filter(Behaviour != "dead")
live.terrestris

# foraging
terrestris.foraging.bin <- live.terrestris %>% 
  mutate(forager = ifelse(Behaviour =="foraging", 1, 0))
terrestris.foraging.bin

terrforage.bin.mod <- glm(forager ~ Roadverge * scale(Traffic) , family = "binomial", data=terrestris.foraging.bin)
summary(terrforage.bin.mod)
mod_dharma1 <- terrforage.bin.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
visreg(terrforage.bin.mod, "Traffic", by="Roadverge", scale="response")

# nesting
terrestris.nesting.bin <- live.terrestris %>% 
  mutate(nester = ifelse(Behaviour =="nesting", 1, 0))
terrestris.nesting.bin

ternesting.bin.mod <- glm(nester ~ Roadverge * scale(Traffic) , family = "binomial", data=terrestris.nesting.bin)
summary(ternesting.bin.mod)
mod_dharma1 <- ternesting.bin.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
visreg(ternesting.bin.mod, "Traffic", by="Roadverge", scale="response")

# Bombus pascuorum ----
pascuorum <- queens %>% 
  filter(Species == "B. pascuorum") 
pascuorum

# Does traffic and road verge quality affect queen mortality?
pascuorum.dead <- pascuorum %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
pascuorum.dead

mortality.mod <- glm(dead ~ Roadverge * scale(Traffic), family = "binomial", data=pascuorum.dead)
summary(mortality.mod)
mod_dharma1 <- mortality.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(mortality.mod, scale="response")

# Does traffic and road verge quality affect foraging behaviour?
pascuorum.foragers <- pascuorum %>% 
  filter(Behaviour == "foraging") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(foraging = n()) %>% 
  rows_insert(., tibble(Site ="Strängnäs", Roadverge="SP", Traffic =372, foraging=0)) %>% 
  rows_insert(., tibble(Site ="Enköping", Roadverge="SP", Traffic =841, foraging=0)) %>%
  rows_insert(., tibble(Site ="Östhammar", Roadverge="SP", Traffic =1662, foraging=0)) %>%
  rows_insert(., tibble(Site ="Häggeby", Roadverge="SR", Traffic =3312, foraging=0)) %>%
  rows_insert(., tibble(Site ="Gottröra", Roadverge="SP", Traffic =3718, foraging=0)) %>%
  rows_insert(., tibble(Site ="Aspön", Roadverge="SR", Traffic =5675, foraging=0)) %>%
  rows_insert(., tibble(Site ="Tosterön", Roadverge="SP", Traffic =5764, foraging=0)) 
pascuorum.foragers

pascuorum.forage.mod <- glm(foraging ~ Roadverge * scale(Traffic) , family = "quasipoisson", data=pascuorum.foragers)
summary(pascuorum.forage.mod)
mod_dharma1 <- pascuorum.forage.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(pascuorum.forage.mod))

# Bombus lucorum coll. ----
lucorum <- queens %>% 
  filter(Species %in% c("B. lucorum", "B. lucorum coll.", "B. cryptarum")) 
lucorum

# Does traffic and road verge quality affect queen mortality?
lucorum.dead <- lucorum %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
lucorum.dead

lucorum.mod <- glm(dead ~ Roadverge * scale(Traffic), family = "binomial", data=lucorum.dead)
summary(lucorum.mod)
mod_dharma1 <- lucorum.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)

# Does traffic and road verge quality affect foraging behaviour?
lucorum.foragers <- lucorum %>% 
  filter(Behaviour == "foraging") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(foraging = n()) %>% 
  rows_insert(., tibble(Site ="Skogs-tibble", Roadverge="SP", Traffic =158, foraging=0)) %>%
  rows_insert(., tibble(Site ="Strängnäs", Roadverge="SP", Traffic =372, foraging=0)) %>%
  rows_insert(., tibble(Site ="Vänge", Roadverge="SP", Traffic =562, foraging=0)) %>%
  rows_insert(., tibble(Site ="Arlanda", Roadverge="SR", Traffic =829, foraging=0)) %>%
  rows_insert(., tibble(Site ="Enköping", Roadverge="SP", Traffic =841, foraging=0)) %>%
  rows_insert(., tibble(Site ="Rimbo", Roadverge="SP", Traffic =939, foraging=0)) %>%
  rows_insert(., tibble(Site ="Hargshamn", Roadverge="SR", Traffic =1726, foraging=0)) %>%
  rows_insert(., tibble(Site ="Häggeby", Roadverge="SR", Traffic =3312, foraging=0)) %>%
  rows_insert(., tibble(Site ="Aspön", Roadverge="SR", Traffic =5675, foraging=0)) %>%
  rows_insert(., tibble(Site ="Tosterön", Roadverge="SP", Traffic =5764, foraging=0)) %>%
  rows_insert(., tibble(Site ="Sigtuna", Roadverge="SR", Traffic =6356, foraging=0)) %>% 
  rows_insert(., tibble(Site ="Upplands-Väsby", Roadverge="SP", Traffic =6168, foraging=0)) 
lucorum.foragers


# All species except terrestris
not.terrestris <- queens %>% 
  filter(Species != "B. terrestris") 
not.terrestris

# Does traffic and road verge quality affect queen mortality?
not.terrestris.dead <- not.terrestris %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
not.terrestris.dead

notterr.mortality.mod <- glm(dead ~ Roadverge + scale(Traffic), family = "binomial", data=not.terrestris.dead)
summary(notterr.mortality.mod)
mod_dharma1 <- notterr.mortality.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(notterr.mortality.mod, "Traffic", by="Roadverge", scale="response")

# Does traffic and road verge quality affect foraging behaviour?
terrestris.foragers <- terrestris %>% 
  filter(Behaviour == "foraging") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(foraging = n()) %>% 
  rows_insert(., tibble(Site ="Skogs-tibble", Roadverge="SP", Traffic =158, foraging=0)) %>%
  rows_insert(., tibble(Site ="Tierp", Roadverge="SR", Traffic =125, foraging=0)) %>%
  rows_insert(., tibble(Site ="Vänge", Roadverge="SP", Traffic =562, foraging=0)) %>%
  rows_insert(., tibble(Site ="Enköping", Roadverge="SP", Traffic =841, foraging=0)) %>%
  rows_insert(., tibble(Site ="Skutskär", Roadverge="SR", Traffic =930, foraging=0)) %>%
  rows_insert(., tibble(Site ="Hargshamn", Roadverge="SR", Traffic =1726, foraging=0)) %>%
  rows_insert(., tibble(Site ="Östhammar", Roadverge="SP", Traffic =1662, foraging=0)) %>%
  rows_insert(., tibble(Site ="Aspön", Roadverge="SR", Traffic =5675, foraging=0)) %>%
  rows_insert(., tibble(Site ="Tosterön", Roadverge="SP", Traffic =5764, foraging=0)) %>%
  rows_insert(., tibble(Site ="Sigtuna", Roadverge="SR", Traffic =6356, foraging=0))
terrestris.foragers

terrestris.forage.mod <- glm(foraging ~ Roadverge * scale(Traffic) , family = "poisson", data=terrestris.foragers)
summary(terrestris.forage.mod)
mod_dharma1 <- terrestris.forage.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(terrestris.forage.mod))

# Does traffic and road verge quality affect nesting behaviour?
notterrestris.nesters <- not.terrestris %>% 
  filter(Behaviour == "nesting") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(nesting = n()) %>% 
  rows_insert(., tibble(Site ="Rovsättra", Roadverge="SR", Traffic =267, nesting=0)) %>%
  rows_insert(., tibble(Site ="Rimbo", Roadverge="SP", Traffic =939, nesting=0)) %>%
  rows_insert(., tibble(Site ="Gottröra", Roadverge="SP", Traffic =3718, nesting=0)) %>%
  rows_insert(., tibble(Site ="Tosterön", Roadverge="SP", Traffic =5764, nesting=0)) %>%
  rows_insert(., tibble(Site ="Upplands-Väsby", Roadverge="SP", Traffic =6168, nesting=0)) 
notterrestris.nesters

notterrestris.nesting.mod <- glm(nesting ~ Roadverge * scale(Traffic) , family = "poisson", data=notterrestris.nesters)
summary(notterrestris.nesting.mod)
visreg(notterrestris.nesting.mod, "Traffic", by="Roadverge", scale="response")
mod_dharma2 <- notterrestris.nesting.mod %>% simulateResiduals(n=1000)
plot(mod_dharma2)
mod_dharma2 %>% testDispersion()
mod_dharma2 %>% testZeroInflation()

# Look only at live bumblebees
live.terrestris <- terrestris %>% 
  filter(Behaviour != "dead")
live.terrestris

# foraging
terrestris.foraging.bin <- live.terrestris %>% 
  mutate(forager = ifelse(Behaviour =="foraging", 1, 0))
terrestris.foraging.bin

terrforage.bin.mod <- glm(forager ~ Roadverge * scale(Traffic) , family = "binomial", data=terrestris.foraging.bin)
summary(terrforage.bin.mod)
mod_dharma1 <- terrforage.bin.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
visreg(terrforage.bin.mod, "Traffic", by="Roadverge", scale="response")

# nesting
terrestris.nesting.bin <- live.terrestris %>% 
  mutate(nester = ifelse(Behaviour =="nesting", 1, 0))
terrestris.nesting.bin

ternesting.bin.mod <- glm(nester ~ Roadverge * scale(Traffic) , family = "binomial", data=terrestris.nesting.bin)
summary(ternesting.bin.mod)
mod_dharma1 <- ternesting.bin.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
visreg(ternesting.bin.mod, "Traffic", by="Roadverge", scale="response")