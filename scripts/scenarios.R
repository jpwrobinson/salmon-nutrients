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
      select(nutrient, value, yield, Scenario) %>%
      mutate(portion = value, prop_portion = 100)

## volume = 2014 production (FO based only) in 100g portions
salmon_scot_2014 / 0.0001


## Scenario B - wild-fish + trimmings-produced salmon
om3_parity<-sa %>% filter(grepl( 'Omega-3', nutrient))

## RULES FOR EATING WILD FISH 
## 1) Eat all local + sustainable stocks
## 2) IF omega-3 parity, STOP
## 3) ELSE add 10% anchovy and 10% sardines
## 4) repeat until omega-3 parity
locals<-c('Capelin',  'Herring', 'Mackerel', 'Sprat')
sb <- nuts %>% filter(species %in% locals & !is.na(value)) %>%
        group_by(nutrient) %>%
        summarise(toty = sum(yield), totc = sum(catch)) %>%
        mutate(conc = toty / totc)
        
sb_forage<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(toty = sum(yield), totc = sum(catch)) %>%
  mutate(conc = toty / totc)

## how much needed to reach omega-3 parity?
epa_missing<-om3_parity[1,3] - sb$toty[sb$nutrient == 'Omega-3 (EPA)'] ## requires 18201 more yield
dha_missing<-om3_parity[2,3] - sb$toty[sb$nutrient == 'Omega-3 (DHA)']
## more EPA needed. DHA already covered.

## what is epa/dha yield deficit from the total forage fish conc?
epa_percent<-epa_missing$yield / sb_forage$toty[sb_forage$nutrient == 'Omega-3 (EPA)'] ## ~17% more yield
#dha_percent<-dha_missing$value / sb_forage$toty[sb_forage$nutrient == 'Omega-3 (DHA)'] 

## what is epa/dha deficit in yield required to reach scenario A concentration?
epa_missing<-om3_parity[1,2] - sb$conc[sb$nutrient == 'Omega-3 (EPA)'] ## need 0.078g more concentration
epa_yield_need<-epa_missing$value / sb$conc[sb$nutrient == 'Omega-3 (EPA)'] ### requires 11% more concentration

## so now take 11% of anchovy + sardines
sb_forage_7<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(toty = sum(yield), totc = sum(catch)) %>%
  mutate(toty = toty*0.43, totc = totc*0.43) %>%
  mutate(conc = toty / totc)

## combine forage with locals
sb$combined_y<-sb$toty + sb_forage_7$toty
sb$combined_c<-sb$totc + sb_forage_7$totc
sb$combined_conc<-sb$combined_y / sb$combined_c

## does conc == omega-3 parity???
om3_parity[1,3] - sb$combined_y[sb$nutrient == 'Omega-3 (EPA)'] ## equal EPA yield
om3_parity[2,3] - sb$combined_y[sb$nutrient == 'Omega-3 (DHA)'] ## more DHA yield

om3_parity[1,2] - sb$combined_conc[sb$nutrient == 'Omega-3 (EPA)'] ## equal EPA conc
om3_parity[2,2] - sb$combined_conc[sb$nutrient == 'Omega-3 (DHA)'] ## more DHA conc

## volume = eat all the edible sustainable wild-fish stocks, +trimmings

## now add trimmings
sb_wild_trim<-sb %>% select(nutrient, combined_c, combined_y) %>%
            mutate(Scenario = 'B (trimmings-only salmon + wild fish)')
sb_wild_trim$salmon_c<-salmon_theoretical
sb_wild_trim$salmon_y<-sa$value * salmon_theoretical
sb_wild_trim$portion<-with(sb_wild_trim, 
                           (combined_y + salmon_y) / (combined_c + salmon_c))

sb_diet<-nuts %>% filter(
  nutrient == 'calcium.mg',
  species %in%  c(locals, 'Anchovy', 'Sardine', 'Atlantic salmon') &!is.na(value)) %>%
        select(species, nutrient, catch) %>%
        mutate(portion = catch)

sb_diet$portion[sb_diet$species == 'Atlantic salmon']<-salmon_theoretical
sb_diet$portion[sb_diet$species == 'Anchovy']<-sb_diet$catch[sb_diet$species == 'Anchovy']*0.43
sb_diet$portion[sb_diet$species == 'Sardine']<-sb_diet$catch[sb_diet$species == 'Sardine']*0.43

sb_diet<-sb_diet %>% filter(nutrient == 'calcium.mg') %>%
      mutate(total = sum(portion), prop_portion = portion / total * 100,
             forage = ifelse(species == 'Atlantic salmon', 'Atlantic salmon', 'Wild fish'))

## Scenario C - trimmings-only + wild-fish + 1 mussels
## estimate = trimmings salmon  +  mussel
sc_sam<-nuts %>% 
  filter(species == 'Atlantic salmon') %>% 
  mutate(Scenario = 'C (trimmings-only salmon + mussels)') %>%
  select(nutrient, species, value, yield, Scenario) %>%
  mutate(portion = value, 
         prop_portion = sb_diet$prop_portion[sb_diet$species == 'Atlantic salmon'],
         forage='Atlantic salmon')

mussel2<-mussel %>% filter(species == 'Mussels') %>%
  mutate(species = 'Mussel', forage='Farmed mussels', 
         yield = NA, Scenario = sc$Scenario, portion = value, 
         prop_portion = (100 - sc$prop_portion)/2)

forage<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(value = mean(value)) %>%
  mutate(species = 'Anchovy/sardine', forage='Wild fish', yield = NA, Scenario = sc$Scenario, 
         portion = value, prop_portion = (100 - sc_sam$prop_portion) / 2)
  

## get diet composition
sc<-rbind(sc_sam,
          mussel2 %>% select(colnames(sc)),
          forage %>% select(colnames(sc))
          )

## get nutrient concentration in diet
sc_conc <- sc %>% group_by(nutrient, Scenario) %>% 
      summarise(portion = weighted.mean(value, w = prop_portion))

sc_diet <- sc 


## how much omega-3 rich wild fish?
om3_parity[1,2] - sc_conc$portion[sc_conc$nutrient == 'Omega-3 (EPA)'] ## equal EPA conc
om3_parity[2,2] - sc_conc$portion[sc_conc$nutrient == 'Omega-3 (DHA)'] ## more DHA conc



## scenario 4 = carp + wild fish
carp<-mussel %>% filter(species == 'Carp') %>%
  mutate(forage='Farmed carp', 
         yield = NA, Scenario = sc_sam$Scenario, portion = value, 
         prop_portion = (100 - sc_sam$prop_portion)/2)

sd<-sc_sam 

## get diet composition
sd<-rbind(sd,
          carp %>% select(colnames(sd)),
          forage %>% select(colnames(sd))
) %>% mutate(Scenario = 'D (trimmings-only salmon + carp')

## get nutrient concentration in diet
sd_conc <- sd %>% group_by(nutrient, Scenario) %>% 
  summarise(portion = weighted.mean(value, w = prop_portion))

sd_diet <- sd 

## how much omega-3 rich wild fish?
om3_parity[1,2] - sd_conc$portion[sd_conc$nutrient == 'Omega-3 (EPA)'] ## equal EPA conc
om3_parity[2,2] - sd_conc$portion[sd_conc$nutrient == 'Omega-3 (DHA)'] ## more DHA conc

