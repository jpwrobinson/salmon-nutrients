# setwd('salmon-nutrients')
library(tidyverse); library(cowplot)
library(funk); theme_set(theme_sleek())
source('scripts/read_data.R')
source('scripts/Figure2.R')

## Scenario A - business as usual
## estimate = 100g of salmon
sa<-nuts %>% 
      filter(species == 'Atlantic salmon') %>% 
      mutate(Scenario = 'I (business-as-usual)') %>%
      select(nutrient, value, yield, Scenario) %>%
      mutate(portion = value, prop_portion = 140)

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

## how much needed to reach omega-3 parity?
epa_missing<-om3_parity[1,3] - sb$toty[sb$nutrient == 'Omega-3 (EPA)'] ## requires 18201 more yield
dha_missing<-om3_parity[2,3] - sb$toty[sb$nutrient == 'Omega-3 (DHA)']
## more EPA needed. DHA already covered.

## what is epa/dha yield deficit from the total forage fish conc?
epa_percent<-epa_missing$yield / sb$toty[sb$nutrient == 'Omega-3 (EPA)'] ## ~23% more EPA yield needed
dha_percent<-dha_missing$yield / sb$toty[sb$nutrient == 'Omega-3 (DHA)'] ## DHA met

## what is EPA deficit in yield required to reach scenario A concentration?
epa_missing<-om3_parity[1,2] - sb$conc[sb$nutrient == 'Omega-3 (EPA)'] ## need 0.078g more concentration
epa_conc_need<-epa_missing$value / sb$conc[sb$nutrient == 'Omega-3 (EPA)'] ### requires 11% more concentration

## so now take 44% of anchovy + sardines
sb_forage_7<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(toty = sum(yield), totc = sum(catch)) %>%
  mutate(toty = toty*0.44, totc = totc*0.44) %>%
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
            mutate(Scenario = 'II (trimmings-only salmon + wild fish)')
sb_wild_trim$salmon_c<-salmon_theoretical
sb_wild_trim$salmon_y<-sa$value[match(sb_wild_trim$nutrient, sa$nutrient)] * salmon_theoretical
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
      mutate(total = sum(portion), prop_portion = portion / total * 140,
             forage = ifelse(species == 'Atlantic salmon', 'Atlantic salmon', 'Wild fish'))

## Scenario C - trimmings-only + wild-fish + 1 mussels
## estimate = trimmings salmon  +  mussel
sc_sam<-nuts %>% 
  filter(species == 'Atlantic salmon') %>% 
  mutate(Scenario = 'III (Trimmings-only salmon, wild fish & mussels)') %>%
  select(nutrient, species, value, yield, Scenario) %>%
  mutate(portion = value, 
         prop_portion = sb_diet$prop_portion[sb_diet$species == 'Atlantic salmon'],
         forage='Atlantic salmon')

mussel2<-mussel %>% filter(species == 'Mussels') %>%
  mutate(species = 'Mussel', forage='Farmed mussels', 
         yield = NA, Scenario = sc_sam$Scenario, portion = value, 
         prop_portion = (140 - sc_sam$prop_portion)/2)

forage<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(value = mean(value)) %>%
  mutate(species = 'Anchovy/sardine', forage='Wild fish', yield = NA, Scenario = sc_sam$Scenario, 
         portion = value, prop_portion = (140 - sc_sam$prop_portion) / 2)
  

## get diet composition
sc<-rbind(sc_sam,
          mussel2 %>% select(colnames(sc_sam)),
          forage %>% select(colnames(sc_sam))
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
         prop_portion = (140 - sc_sam$prop_portion)/2)

sd<-sc_sam 

## get diet composition
sd<-rbind(sd,
          carp %>% select(colnames(sd)),
          forage %>% select(colnames(sd))
) %>% mutate(Scenario = 'IV (Trimmings-only salmon, wild fish & carp')

## get nutrient concentration in diet
sd_conc <- sd %>% group_by(nutrient, Scenario) %>% 
  summarise(portion = weighted.mean(value, w = prop_portion))

sd_diet <- sd 

## how much omega-3 rich wild fish?
om3_parity[1,2] - sd_conc$portion[sd_conc$nutrient == 'Omega-3 (EPA)'] ## equal EPA conc
om3_parity[2,2] - sd_conc$portion[sd_conc$nutrient == 'Omega-3 (DHA)'] ## more DHA conc


# 
# ## scenario 5 = carp + wild fish + mussels
# mix<-mussel %>%
#   mutate(
#         forage=ifelse(species == 'Carp', 'Farmed carp', 'Farmed mussels'),
#          yield = NA, Scenario = unique(sc_sam$Scenario), portion = value,
#          prop_portion = unique((140 - sc_sam$prop_portion)/3)
#          )
# 
# se<-sc_sam 
# 
# ## get diet composition
# se<-rbind(se,
#           mix %>% select(colnames(se)),
#           forage %>% select(colnames(se)) %>% mutate(prop_portion = unique(mix$prop_portion))
# ) %>% mutate(Scenario = 'E (Trimmings-only salmon, wild fish, mussels & carp')
# 
# ## get nutrient concentration in diet
# se_conc <- se %>% group_by(nutrient, Scenario) %>% 
#   summarise(portion = weighted.mean(value, w = prop_portion))
# 
# se_diet <- se 

## how much omega-3 rich wild fish?
om3_parity[1,2] - sd_conc$portion[sd_conc$nutrient == 'Omega-3 (EPA)'] ## equal EPA conc
om3_parity[2,2] - sd_conc$portion[sd_conc$nutrient == 'Omega-3 (DHA)'] ## more DHA conc


## fish in the SEA
wild_limits<-c(wild_for_33T[1]/wild_for_33T[2], 1, wild_for_33T[3]/wild_for_33T[2])
## for c and d, estimate wild fish (corr. by edible yield) and aquaculture needed to make up seafood production of A
wild_cd<-(salmon_scot_2014*0.88 - sb_wild_trim$salmon_c[1]*0.88) / 2 / 0.62
carp_muss_cd<-(salmon_scot_2014*0.88 - sb_wild_trim$salmon_c[1]*0.88) / 2 

## is wild fish eaten in C and D more than available? (eating only anchovy and sardine)
## no
sum(wild$mean_tonnes[wild$Species %in% c('Anchovy', 'Sardine')]) - wild_cd


# repeat steps above to get catch limits per scenario
yl_a<-nuts %>% filter(species == 'Atlantic salmon') %>%
  group_by(nutrient) %>%
  summarise(tot_min = sum(catch_min), tot_max = sum(catch_max))  %>%
  filter(nutrient == 'calcium.mg') %>% mutate(scenario = 'I')

yl_b<-nuts %>% filter(species %in% locals & !is.na(value)) %>%
  group_by(nutrient) %>%
  summarise(tot_min = sum(catch_min), tot_max = sum(catch_max))  %>%
  filter(nutrient == 'calcium.mg') %>% mutate(scenario = 'II')
yl_b2<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(tot_min = sum(catch_min)*0.44, tot_max = sum(catch_max)*0.44) %>%
  filter(nutrient == 'calcium.mg')
yl_b$tot_min<-yl_b$tot_min + yl_b2$tot_min
yl_b$tot_max<-yl_b$tot_max + yl_b2$tot_max

yl_c<-nuts %>% filter(species %in% c('Anchovy', "Sardine")) %>%
  group_by(nutrient) %>%
  summarise(tot_min = sum(catch_min)*0.44, tot_max = sum(catch_max)*0.44) %>%
  filter(nutrient == 'calcium.mg') %>% mutate(scenario = 'III')

yl_d<-yl_c %>% mutate(scenario = 'IV')

catch_limits<-rbind(yl_a, yl_b, yl_c, yl_d)
  
catch_org<-wild_for_33T
catch_sb<-sb$combined_c[1]
catch_sc<-wild_cd
catch_sd<-wild_cd

sea<-data.frame(scenario = c('I', 'II', 'III', 'IV'), catch = c(catch_org[2], catch_sb, catch_sc, catch_sd))
sea$unfished<-catch_org[2] - sea$catch
sea$wild<-catch_org[2]
sea$stat<-'mean'


## get upper and lower catches, depending on trimmings production. combine with mean estimate
upper<-sea %>% mutate(stat = 'upper')
upper$catch<-catch_limits$tot_min
upper$unfished<-catch_org[3] - catch_limits$tot_min
upper$wild<-catch_org[3]

lower<-sea %>% mutate(stat = 'lower')
lower$catch<-catch_limits$tot_max
lower$unfished<-catch_org[1] - catch_limits$tot_max
lower$wild<-catch_org[1]

sea<-rbind(sea, lower, upper)
sea$unfished_prop<- with(sea, unfished / wild * 100)


## fish in the BELLY
tonnes_org<-data.frame(scenario = 'I', t = salmon_scot_2014, s= 'Atlantic salmon')
tonnes_sb<-data.frame(scenario = 'II', t = c(sb_wild_trim$combined_c[1], sb_wild_trim$salmon_c[1]),
                      s =c('Wild fish', 'Atlantic salmon'))

tonnes_sc<-data.frame(scenario = 'III', 
                      t = c(wild_cd, sb_wild_trim$salmon_c[1], carp_muss_cd),
                      s =c('Wild fish', 'Atlantic salmon', 'Mussels'))
tonnes_sd<-data.frame(scenario = 'IV', t = c(wild_cd, sb_wild_trim$salmon_c[1],  carp_muss_cd),
                      s =c('Wild fish', 'Atlantic salmon', 'Carp'))
tonnes<-rbind(tonnes_org, tonnes_sb, tonnes_sc, tonnes_sd)

## add edible portions
tonnes$t[tonnes$s == 'Atlantic salmon']<- tonnes$t[tonnes$s == 'Atlantic salmon'] * 0.88
tonnes$t[tonnes$s == 'Wild fish']<- tonnes$t[tonnes$s == 'Wild fish'] * 0.62

tonnes %>% group_by(scenario) %>% summarise(sum(t))
## diff in scenario II to I
148509 / 157539 * 100 ## 94%

## back-calculate total carp/mussels using edible portion (carp_muss_cd is seafood produced)
tot_mussel<-carp_muss_cd / 0.4
tot_carp<-carp_muss_cd / 0.54


##fishmeal required

## get carp FM to food ratio from global stats
rat<- 92 / 7709 

fm_carp <- tot_carp * rat

fm<-data.frame(scenario = unique(tonnes$scenario),
              fishmeal = c(total_fm_from_wild, fm_trimmings[2], fm_trimmings[2], fm_trimmings[2] + fm_carp))
  


