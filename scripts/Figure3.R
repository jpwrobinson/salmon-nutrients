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
      select(nutrient, value, yield, Scenario)

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
epa_missing<-om3_parity[1,3] - sb$toty[sb$nutrient == 'Omega-3 (EPA)']
dha_missing<-om3_parity[2,3] - sb$toty[sb$nutrient == 'Omega-3 (DHA)']
## more EPA needed. DHA already covered.

## what is epa/dha yield deficit from the total forage fish conc?
epa_percent<-epa_missing$yield / sb_forage$toty[sb_forage$nutrient == 'Omega-3 (EPA)'] ## ~17%
#dha_percent<-dha_missing$value / sb_forage$toty[sb_forage$nutrient == 'Omega-3 (DHA)'] 

## what is epa/dha deficit in yield required to reach scenario A concentration?
epa_missing<-om3_parity[1,2] - sb$conc[sb$nutrient == 'Omega-3 (EPA)'] ## need 7.8% more concentration
epa_yield_need<-epa_missing$value / sb$conc[sb$nutrient == 'Omega-3 (EPA)'] ### requires 11% more yield

## so now take 11% of anchovy + sardines
sb_forage_7<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(toty = sum(yield), totc = sum(catch)) %>%
  mutate(toty = toty*0.5, totc = totc*0.5) %>%
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
sb_wild_trim<-sb %>% select(nutrient, combined_c, combined_y)
sb_wild_trim$salmon_c<-salmon_theoretical
sb_wild_trim$salmon_y<-sa$value * salmon_theoretical
sb_wild_trim$portion<-with(sb_wild_trim, (combined_y + salmon_y) / (combined_c + salmon_c))


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