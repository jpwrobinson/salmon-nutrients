library(tidyverse); library(funk); theme_set(theme_sleek())

## Panel A = business as usual (2018 values)

## total volumes in FMFO (2016)
wild_caught_in_FMFO <- 15000
total_fish_in_FMFO <- wild_caught_in_FMFO / 0.67
trimmings_FMFO <- total_fish_in_FMFO - wild_caught_in_FMFO

## mean global production in 2010-16
fm_prod = 4445
fo_prod = 878

## FMFO usage by aquaculture
fo_sp<-data.frame(
  species = c('Salmonids', 'Marine fish', 'Eels', 'Tilapia', "Other freshwater fish", 'Crustaceans', 'Direct human consumption', 'Other'),
  prop = c(0.45, 0.135, 0.0250, 0.045, 0.0525, 0.045, 0.18, 0.07)
)

fm_sp<-data.frame(
  species = c('Salmonids', 'Marine fish', 'Eels', 'Tilapia', "Other freshwater fish", 'Crustaceans', 'Cyprinids','Pig', 'Poultry', 'Other'),
  prop = c(0.1587, 0.1035, 0.0414, 0.0621, 0.0897, 0.2139, 0.0207, 0.23, 0.05, 0.03)
)

fo_sp$vol <- fo_sp$prop * fo_prod
fm_sp$vol <- fm_sp$prop * fm_prod

fmfo<-merge(fo_sp, fm_sp, by = 'species', all.x=TRUE, all.y=TRUE)
fmfo <- fmfo %>% select(species, vol.x, vol.y)
fmfo$vol.x[is.na(fmfo$vol.x)]<-0
fmfo$vol.y[is.na(fmfo$vol.y)]<-0
fmfo$tot<-fmfo$vol.x + fmfo$vol.y


## Aquaculture production in 2018 (unless stated)
prod<-data.frame(
  species = c('Salmonids', 'Marine fish', 'Eels', 'Tilapia', "Other freshwater fish",
              'Crustaceans', 'Cyprinids','Pig', 'Poultry'),
  tonnes = c(3079, 2610, 304, 4675, 3398, 6326, 7709, 112940, 94567),
  edible =c(0.82, 0.49, 0.5, 0.45, 0.63, 0.55, 0.54, 1,1)
)
prod$edible_volume<-prod$tonnes * prod$edible

tot_seafood <- sum(prod$tonnes[c(1:7)])



## Panel B = trimmings only salmon

## total volumes in FMFO (2016)
wild_caught_in_FMFO2 <- 10061
spare_wild<-3753
wild_eaten<-1185

wild_caught_in_FMFO2 + wild_eaten

## mean global production in 2010-16
fm_prod2 = 2866
fo_prod2 = 641

## FMFO usage by aquaculture
fo_sp2<-data.frame(
  species = c('Salmonids', 'Marine fish', 'Eels', 'Tilapia', "Other freshwater fish", 
              'Crustaceans', 'Direct human consumption', 'Other'),
  prop = c(0.24658, 0.18493, 0.0308, 0.0616, 0.0719, 0.0616, 0.24658, 0.09589)
)

fm_sp2<-data.frame(
  species = c('Salmonids', 'Marine fish', 'Eels', 'Tilapia', "Other freshwater fish", 
              'Crustaceans', 'Cyprinids','Pig', 'Poultry', 'Other'),
  prop = c(0.0492, 0.16, 0.064, 0.096, 0.139, 0.199, 0.096, 0.07, 0.077, 0.0465)
)

fo_sp2$vol <- fo_sp2$prop * fo_prod2
fm_sp2$vol <- fm_sp2$prop * fm_prod2


fmfo2<-merge(fo_sp2, fm_sp2, by = 'species', all.x=TRUE, all.y=TRUE)
fmfo2 <- fmfo2 %>% select(species, vol.x, vol.y)
fmfo2$vol.x[is.na(fmfo2$vol.x)]<-0
fmfo2$vol.y[is.na(fmfo2$vol.y)]<-0
fmfo2$tot<-fmfo2$vol.x + fmfo2$vol.y



## Aquaculture production in 2018 (unless stated)
# prod2<-data.frame(
#   species = c('Salmonids', 'Marine fish', 'Eels', 'Tilapia', "Other freshwater fish", 'Crustaceans', 'Cyprinids','Pig', 'Poultry'),
#   tonnes = c(3079, 2610, 304, 4675, 3398, 6326, 7709, 112940, 94567)
# )

# tot_seafood2<- sum(prod2$tonnes[c(1:7)])
