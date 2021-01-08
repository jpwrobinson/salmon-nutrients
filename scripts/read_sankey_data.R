library(tidyverse); library(funk); theme_set(theme_sleek())

## Panel A = business as usual (2018 values)

## total volumes in FMFO (2016)
wild_caught_in_FMFO <- 15000
total_fish_in_FMFO <- wild_caught_in_FMFO / 0.67
trimmings_FMFO <- total_fish_in_FMFO - wild_caught_in_FMFO

## mean global production in 2010-16
fm_prod = 4445
fo_prod = 878

# # new estimates by James
# fm_prop = fm_prod / (fm_prod + fo_prod)
# fo_prop = fo_prod / (fm_prod + fo_prod)
# 
# fo_from_trimmings<-0.4 * fo_prop
# fm_from_trimmings<-0.22 * fm_prop
# fo_from_wild<-fo_prop - fo_from_trimmings
# fm_from_wild<-fm_prop - fm_from_trimmings
# 




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

## Aquaculture production in 2018 (unless stated)
prod<-data.frame(
  species = c('Salmonids', 'Marine fish', 'Eels', 'Tilapia', "Other freshwater fish", 'Crustaceans', 'Cyprinids','Pig', 'Poultry'),
  tonnes = c(3079, 2610, 304, 4675, 3398, 6326, 7709, 112940, 94567)
)

tot_seafood <- sum(prod$tonnes[c(1:7)])
