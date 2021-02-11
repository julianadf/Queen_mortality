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

# Load and check data ----
queen.data <- read.csv2(here("data", "Database.csv" ))

# Take away non-queen observations
queens <- queen.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Roadverge = as.factor(Roadverge)) %>% 
  filter(Caste == "queen")
str(queens)

ggplot(queens, aes(x=fct_infreq(Site), fill=Behaviour)) + geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = c(0.9, 0.8), axis.text.x.bottom = element_text(size=15, family="serif"))

nesting <- queens[which(queens$Behaviour=="nesting"),]

ggplot(nesting, aes(x=Roadverge)) + geom_bar()

# Look at differences in behaviour between species poor and species rich sites
ggplot(queens, aes(x=Behaviour)) + geom_bar(aes(fill=Roadverge), stat="count", position = position_dodge()) + 
  theme_classic() + theme(text = element_text(size=14), axis.text.x.bottom = element_text(size=15, family="serif"), axis.text.y.left = element_text(size=14))

# Look at species richness between poor and rich sites
queens.obs <- queens %>% 
  group_by(Site, Roadverge, Traffic, Species) %>% 
  summarise(obs = n())
queens.obs
  
ggplot(queens, aes(x=fct_infreq(Species), fill=Behaviour)) + geom_bar(stat="count") +
  theme(axis.text.x = element_text(angle = 90), legend.position = c(0.9, 0.8), axis.text.x.bottom = element_text(size=15, family="serif"))

queen.rich <- queens.obs %>% 
  filter(Species != "B. terrestris?") %>% 
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(rich = n())
queen.rich

rich.mod <- lm(rich ~ Roadverge + Traffic, data = queen.rich)
summary(rich.mod)
visreg(rich.mod)

# Select only "dead" observations and create database
dead <- queens %>% 
  filter(Behaviour == "dead") %>%
  group_by(Site, Roadverge, Traffic) %>% 
  summarise(dead.indiv = n()) %>% 
  rows_insert(., tibble(Site ="Rovsättra", Roadverge="SR", Traffic =267, dead.indiv=0))
dead

hist(dead$indiv)
ggplot(dead, aes(x=Traffic, y= dead.indiv, colour = Roadverge)) + geom_point(size=3) + 
  geom_text(aes(label=Site), size=4, color="black", position = position_jitter()) + 
  theme_classic()

dead.mod <- glm(dead.indiv ~ Roadverge + scale(Traffic) + (1 | Site), family = "quasipoisson", data=dead)
summary(dead.mod)
mod_dharma1 <- dead.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(dead.mod))

# take away Enköping (and Arlanda)
dead.ol <- dead %>% 
  filter(Site != "Enköping" & Site != "Arlanda")
dead.ol

dead.mod2 <- glmer(dead.indiv ~ Roadverge + scale(Traffic) + (1 | Site), family = "poisson", data=dead.ol)
summary(dead.mod2)
mod_dharma1 <- dead.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(dead.mod))

# try a binomial model
trials <- queens %>% 
  group_by(Site) %>% 
  summarise(total.obs = n())
trials

dead.bin <- left_join(dead, trials, by="Site")
dead.bin$failures <- dead.bin$total.obs - dead.bin$dead.indiv
dead.bin$obs <- c(1:20)

bin.mod <- glm(cbind(dead.indiv, failures) ~ Roadverge + scale(Traffic) , family = "quasibinomial", data=dead.bin)
summary(bin.mod)
plot(allEffects(bin.mod))

queens.test <- queens %>% 
  mutate(bin = ifelse(Behaviour=="dead", 1, 0))
queens.test

mod.test <- glm(bin ~ Roadverge + scale(Traffic) , family = "binomial", data=queens.test)
summary(mod.test)
mod_dharma1 <- mod.test %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(mod.test))
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()

# foraging binomial - test
queen.forage <- queens %>% 
  mutate(foraging = ifelse(Behaviour=="foraging", 1, 0))
queen.forage

forage.mod <- glm(foraging ~ Roadverge + scale(Traffic) , family = "binomial", data=queen.forage)
summary(forage.mod)
mod_dharma2 <- forage.mod %>% simulateResiduals(n=1000)
plot(mod_dharma2)
plot(allEffects(forage.mod))
mod_dharma2 %>% testDispersion()
mod_dharma2 %>% testZeroInflation()

# Explore the role of road verge width
abund <- queens %>% 
  group_by(Site, Traffic, Roadverge, Verge.width) %>% 
  summarise(indiv= n())
abund

cor.test(abund$Traffic, abund$Verge.width, method="pearson", conf.level = 0.95)
plot(x=abund$Traffic, y=abund$Verge.width)

#Look at each category separately
# species rich
width.rich <- abund[which(abund$Roadverge=="SR"),]
plot(x=width.rich$Traffic, y=width.rich$Verge.width)
cor.test(width.rich$Traffic, width.rich$Verge.width, method="pearson", conf.level = 0.95)
# species poor
width.poor <- abund[which(abund$Roadverge=="SP"),]
plot(x=width.poor$Traffic, y=width.poor$Verge.width)
cor.test(width.poor$Traffic, width.poor$Verge.width, method="pearson", conf.level = 0.95)

width.mod <- aov(Verge.width ~ Roadverge, data= abund)
summary(width.mod)

width.mod <- glm(indiv ~  scale(Traffic) + Roadverge + Verge.width, family = "quasipoisson", data=abund)
summary(width.mod)
vif(width.mod)
visreg(width.mod)
mod_dharma <- width.mod %>% simulateResiduals(n=1000)
plot(mod_dharma)
mod_dharma %>% testDispersion()
mod_dharma %>% testZeroInflation()

