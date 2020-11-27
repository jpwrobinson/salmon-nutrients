# setwd('salmon-nutrients')
library(tidyverse); library(cowplot)
library(funk); theme_set(theme_sleek())
source('scripts/read_data.R')
source('scripts/Figure2.R')

## Scenario A - business as usual
## estimate = 100g of salmon
sa<-nuts %>% 
      filter(species == 'Atlantic salmon') %>% 
      mutate(Scenario = 'A (business-as-usual)') %>%
      select(nutrient, yield, Scenario)

## volume = 2014 production in 100g portions
salmon_scot_2014 / 0.0001


## Scenario B - wild-fish only
## estimate =  wild edibles
## how to get to the wild fish captures?

## volume = theoretical production from trimmings + wild-caught for OIL

sb<-nuts %>% filter(species %in% edibles) %>%
  filter(species != 'Atlantic salmon') %>%
  select(species, nutrient, value, catch, yield) %>%
  mutate(yield = 0.5 * yield) %>%
  group_by(nutrient) %>%
  summarise(yield = sum(yield)) %>%
  mutate(Scenario = 'B (50% edible wild-caught fish)')


## Scenario C - trimmings-only + wild-fish
## estimate = 100g salmon + wild edibles
## how to get to the wild fish captures?

## volume = theoretical production from trimmings + wild-caught for OIL
# bvol<-salmon_theoretical

sc_salmon<-nuts %>% filter(species=='Atlantic salmon') %>%
  select(species, nutrient, value, catch) %>%
  mutate(yield = value * salmon_theoretical)

sc_wild<-nuts %>% filter(species %in% edibles) %>%
  filter(species != 'Atlantic salmon') %>%
  select(species, nutrient, value, catch, yield) %>%
  mutate(yield = 0.5 * yield)

sc<-rbind(sc_salmon, sc_wild) %>% group_by(nutrient) %>%
  summarise(yield = sum(yield)) %>%
  mutate(Scenario = 'C (trimmings-only salmon + 50% edible wild-caught fish)')

ss<-rbind(sa, sb, sc)

g1<-ggplot(ss, aes(nutrient, yield, fill = Scenario)) + 
    geom_bar(stat='identity', position='dodge') +
    facet_wrap(~nutrient, scales='free') +
    # coord_flip() + 
    theme(
      # strip.text.x=element_blank(),
      axis.text.x=element_blank()) +
  labs(x = '' , y = 'Nutrient yield')


pdf(file='figures/Figure3.pdf', height=7, width=12)
g1
dev.off()