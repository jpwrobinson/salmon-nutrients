library(tidyverse); library(funk); theme_set(theme_sleek())

## Panel A = business as usual (2018 values)

## total volumes in FMFO
wild_caught_in_FMFO <- 18000
total_fish_in_FMFO <- wild_caught_in_FMFO / 0.67
trimmings_FMFO <- total_fish_in_FMFO - wild_caught_in_FMFO

# new estimates by James
trimmings_FO<-0.22 * fo_prod
trimmings_FM<-0.22 * fm_prod
wild_FO<-fo_prod / 0.05
wild_FM<-fm_prod - trimmings_FM

## mean global production in 2010-16
fm_prod = 4857
fo_prod = 951


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
  tonnes = c(3284, 3024, 304, 5166, 3211, 7730, 8201, 112940, 94567)
)

tot_seafood <- sum(prod$tonnes[c(1:7)])
