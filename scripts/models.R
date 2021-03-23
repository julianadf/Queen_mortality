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
library(ggeffects)
library(png)
library(jtools)

# Load and check data ----
queen.data <- read.csv2(here("data", "Database_for_figures.csv" ))

# Take away non-queen observations
queens <- queen.data %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Roadverge = as.factor(Roadverge)) %>% 
  filter(Caste == "queen")
str(queens)

# Riccardos model
queen.behaviour.0 <- queens %>% 
  group_by(Site, Roadverge, Traffic, Verge.width, Behaviour) %>% 
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
  filter(Behaviour %in% c("foraging", "nesting")) %>% 
  mutate(Behaviour = as.factor(Behaviour)) 
queen.behaviour

queen.behaviour$obs <- c(1:35)

behaviour.mod <- glmer(behaviour.count ~ Behaviour * Roadverge * scale(Traffic) + (1|Site), offset=log10(Verge.width), family = "poisson", data=queen.behaviour)
summary(behaviour.mod)
mod_dharma1 <- behaviour.mod %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(behaviour.mod))
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
# Chi-square
Anova(behaviour.mod, type="3")

# Does traffic and road verge quality affect queen mortality?
queen.mortality <- queens %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
queen.mortality

mortality.mod <- glm(dead ~ Roadverge + scale(Traffic) , family = "binomial", data=queen.mortality)
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

# Proportions -----
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

# Counts -----
# Does traffic and road verge quality affect foraging behaviour?
foragers <- queens %>% 
  filter(Behaviour == "foraging") %>%
  group_by(Site, Roadverge, Traffic, Verge.width) %>% 
  summarise(foraging = n()) 
foragers

forage.mod <- glm(foraging ~ Roadverge * scale(Traffic) , offset= log10(Verge.width), family = "quasipoisson", data=foragers)
summary(forage.mod)
plot(allEffects(forage.mod))


# Does traffic and road verge quality affect nesting behaviour?
nesters <- queens %>% 
  filter(Behaviour == "nesting") %>%
  group_by(Site, Roadverge, Traffic, Verge.width) %>% 
  summarise(nesting = n()) %>% 
  rows_insert(., tibble(Site ="Gottröra", Roadverge="SP", Traffic =3718, Verge.width= 4.4, nesting=0)) %>%
  rows_insert(., tibble(Site ="Rimbo", Roadverge="SP", Traffic =939, Verge.width= 5.19, nesting=0)) %>%
  rows_insert(., tibble(Site ="Rovsättra", Roadverge="SR", Traffic =267, Verge.width= 4.32, nesting=0)) %>% 
  rows_insert(., tibble(Site ="Tosterön", Roadverge="SP", Traffic =5764, Verge.width= 4.07, nesting=0)) %>%
  rows_insert(., tibble(Site ="Upplands-Väsby", Roadverge="SP", Traffic =6168, Verge.width= 4.76, nesting=0)) 
nesters

nesting.mod <- glm(nesting ~ Roadverge * scale(Traffic), offset=log10(Verge.width), family = "poisson", data=nesters)
summary(nesting.mod)
plot(allEffects(nesting.mod))
visreg(nesting.mod, "Traffic", by="Roadverge", scale="response", cond=list("Verge.width"))
mod_dharma2 <- nesting.mod %>% simulateResiduals(n=1000)
plot(mod_dharma2)
mod_dharma2 %>% testDispersion()
mod_dharma2 %>% testZeroInflation()

# Figure for publication
effects_nesters <- effects::effect(term= "Roadverge * scale(Traffic)",  mod= nesting.mod)
summary(effects_nesters)
x_traffic <- as.data.frame(effects_nesters)

# Species poor line
artfattiga <- x_traffic[x_traffic$Roadverge =="SP",]

conf.int <- ggplot(artfattiga) +
  stat_smooth(aes(x=Traffic, y=fit-se), method="loess", se=FALSE, size=0.3, linetype="dotted") +
  stat_smooth(aes(x=Traffic, y=fit+se), method="loess", se=FALSE, size=0.3, linetype="dotted") +
  guides(fill=guide_legend(override.aes = list(fill="2c7bb6", size=0.7))) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
conf.int

gg1 <- ggplot_build(conf.int)

# extract data for the loess lines from the 'data' slot
df2 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 

artfattiga.plot <- conf.int + geom_ribbon(data = df2, aes(x=x, ymin=ymin, ymax=ymax), fill="#2c7bb6", alpha=0.3) + 
  geom_smooth(data=artfattiga, aes(x=Traffic, y=fit), se=FALSE, size= 0.5)
artfattiga.plot
# get plotted values and turn into a data frame
# SP.data <- ggplot_build(artfattiga.plot)
# SP.upper95CI <- SP.data$data[[2]]
# SP.lower95CI <- SP.data$data[[1]]
SP.95CI <- SP.data$data[[3]]
SP.fittedline <- SP.data$data[[4]]
SP.all <- left_join( SP.fittedline[,1:2], SP.95CI[,1:3])
SP.names <- c("x","SPfit", "SPymin", "SPymax")
names(SP.all) <- SP.names
# Species rich line
artrika <- x_traffic[x_traffic$Roadverge =="SR",]

conf.int2 <- ggplot(artrika) +
  stat_smooth(aes(x=Traffic, y=fit-se), method="loess", se=FALSE, size=0.3, linetype="dotted") +
  stat_smooth(aes(x=Traffic, y=fit+se), method="loess", se=FALSE, size=0.3, linetype="dotted") +
  guides(fill=guide_legend(override.aes = list(fill="grey", size=0.7))) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
conf.int2

gg2 <- ggplot_build(conf.int2)

# extract data for the loess lines from the 'data' slot
df3 <- data.frame(x = gg2$data[[1]]$x,
                  ymin = gg2$data[[1]]$y,
                  ymax = gg2$data[[2]]$y) 

artrika.plot <- conf.int2 + geom_ribbon(data = df3, aes(x=x, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3) + 
  geom_smooth(data=artrika, aes(x=Traffic, y=fit), se=FALSE, size= 0.5)
artrika.plot
# get plotted values and turn into a data frame
SR.data <- ggplot_build(artrika.plot)
SR.95CI <- SR.data$data[[3]]
SR.fittedline <- SR.data$data[[4]]
SR.all <- left_join(SR.fittedline[,1:2], SR.95CI[,1:3])
SR.names <- c("x","SRfit", "SRymin", "SRymax")
names(SR.all) <- SR.names

# Join data 
nesting.results <- left_join(SP.all, SR.all, by="x")
#write.csv2(nesting.results, "data for plotting nesting.csv")

# Plot the crap out of it ----
nestingplot.data <- read.csv2(here("data", "data for plotting nesting.csv" ))

final.plot <- ggplot(nestingplot.data, aes(x=Traffic, y=fit, group=Quality, colour=Quality, ymin=ymin, ymax=ymax)) + 
  geom_line() +
  geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=Quality), alpha=0.15, colour=NA) + 
  labs(x="Traffic intensity (vehicles/day)", y = "Number of nesting queens") +
  scale_color_manual(values = c("#045a8d", "#9e0142"), c("Low quality", "High quality"), name="Road verge") +
  scale_fill_manual(values = c("#045a8d", "#9e0142"), c("Low quality", "High quality"), name="Road verge") +
  #facet_wrap(~Quality) + 
  theme(legend.position =c(0.85,0.85)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text.x.bottom = element_text(size=14, family="serif"), 
        axis.text.y.left = element_text(size=14, family="serif"),
        axis.title.y = element_text(size=18, family="serif"), axis.title.x = element_text(size=20, family="serif"))
final.plot  

# Analysis without cuckoos ----
cuckoos <- queens %>% 
  filter(Species %in% c("B. sylvestris", "B. bohemicus", "B. norvegicus", "B. campestris", "B. rupestris"))
cuckoos

queen.no.cuckoo <- queens %>% 
  filter(Species != "B. sylvestris") %>% 
  filter(Species != "B. bohemicus") %>%
  filter(Species != "B. norvegicus") %>%
  filter(Species != "B. campestris") %>%
  filter(Species != "B. rupestris") 
queen.no.cuckoo

# Does traffic and road verge quality affect queen mortality?
queen.mortality.cuckoo <- queen.no.cuckoo %>% 
  mutate(dead = ifelse(Behaviour=="dead", 1, 0))
queen.mortality.cuckoo

mortality.mod.cuckoo <- glm(dead ~ Roadverge + scale(Traffic) , family = "binomial", data=queen.mortality.cuckoo)
summary(mortality.mod.cuckoo)
Anova(mortality.mod.cuckoo, type=3)
mod_dharma1 <- mortality.mod.cuckoo %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(mortality.mod.cuckoo))
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation()
