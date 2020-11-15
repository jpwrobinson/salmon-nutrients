# setwd('salmon-nutrients')

library(tidyverse); library(funk); theme_set(theme_sleek())

nut<-read.csv('data/feedback_species_nutrient_profiles.csv')


## from Feedback-salmon-feed-calculations-final.xlxs

## tab 1: FMFO salmon - shrimp
# Fish oil in Scottish salmon feed in 2014	(tonnes)
FO_salmon_scot_2014<-33000 

# Fish oil yield (fish oil produced relative to whole fish used) %
FO_yield<-4.8


# Total amount of wet fish needed to produce 33,000 tonnes of of fish oil. 	 (tonnes)
wetfish_required <- (FO_salmon_scot_2014/FO_yield)*100 ## 687500 

# Percentage of fish currently used in fishmeal and oil production that is from trimmings (percent)
## This is global average. Could also use weighted average for Scottish salmon (0.24) and EU value (0.47)
current_trimmings_percent<-c(0.24, 0.33, 0.47)


# Trimmings and by-products needed to produce 33,000 tonnes of fish oil	(tonnes)
trimmings_for_33T<-current_trimmings_percent*wetfish_required # 226875 

# Percentage of fish currently used in fishmeal and oil production that is wild-caught fish
wild_FMFO_percent<-1 - current_trimmings_percent #.67

# Total amount of wild-caught fish needed to produce 33,000 tonnes of fish oil	 (tonnes)
wild_for_33T<-wetfish_required - trimmings_for_33T #460625  (also wild_FMFO_percent * wetfish_required)

# Fish meal yield	(%)
FM_yield<-22.5

# Total amount of fishmeal from the total whole fish (wild-caught and trimmings) at the basis of this calculation	 
total_fm_from_wild<-154688 



# Total amount of fishmeal needed in salmon production 2014	 (tonnes)
FM_2014<-55000 

# Total amount of fishmeal "left-over / spare" (tonnes)
FM_spare<-total_fm_from_wild - FM_2014  # 99688 


# Global aquaculture shrimp production 2012	 4,000,000 
# Global fishmeal used in shrimp feed 2012	 1,000,000 
# Amount of shrimp that can be produced per tonne of fishmeal	 4 
# Amount of shrimp that can be produced with the "leftover / spare" fishmeal in our model	 398,750 

# Salmon production 2014	(tonnes)
salmon_scot_2014<-179022


# Amount of shrimp that could theoretically be produced relative to each tonne of salmon actually produced in 2014. 	 2.23 


# Fish oil from trimmings	 (tonnes)
fo_trimmings<-FO_salmon_scot_2014 * current_trimmings_percent #10890 

# Fishmeal from trimmings	 (tonnes)
fm_trimmings<-total_fm_from_wild * current_trimmings_percent # 51047 

# https://www.gov.scot/publications/scottish-fish-farm-production-survey-2019/pages/5/
production<-read.csv('data/scottish_salmon_production_99-20.csv')



## wild species in fmfo
# wild<-read.csv('data/wildspecies_in_fmfo.csv') %>% pivot_longer(-Species, names_to = 'FOFM', values_to='percent') %>%
#         group_by(Species) %>%
#         summarise(min = min(percent), max = max(percent), mean=mean(percent))
# 
# wild$mean_tonnes<-wild$mean * wild_for_33T[2]
# wild$min_tonnes<-wild$mean * wild_for_33T[3]
# wild$max_tonnes<-wild$mean * wild_for_33T[1]


### updated wild species in fmfo
wild <- read.csv('data/FMFO_species.csv') %>% 
      filter(Company !='Cargill') %>%
      mutate(FM_cor = ifelse(is.na(FM_cor), 0, FM_cor),
             F0_cor = ifelse(is.na(F0_cor), 0, F0_cor)) %>%
      select(FM_cor, F0_cor, Species, Company, Year, wild_caught_FM, wild_caught_FO) %>%
      pivot_longer(cols=starts_with('F'), names_to = 'type', values_to = 'value') %>% 
      mutate(prop = ifelse(type == 'F0_cor', 
                           wild_caught_FO/(wild_caught_FM + wild_caught_FO), 
                           wild_caught_FM/(wild_caught_FM + wild_caught_FO)))

props<-wild %>% group_by(Species, Year, Company, wild_caught_FM, wild_caught_FO) %>%
      summarise(prop_species = weighted.mean(value, weight=prop) / 100)

## check sums 
# props%>% mutate(tot = wild_caught_FM + wild_caught_FO,
#              species_catch = prop_species * tot)


species_prop<-props %>% group_by(Species) %>%
  summarise(mean_proportion = mean(prop_species),
            min_proportion = min(prop_species),
            max_proportion = max(prop_species))

wild<-species_prop
wild$mean_tonnes<-wild$mean_proportion * wild_for_33T[2]
wild$min_tonnes<-wild$min_proportion * wild_for_33T[2]
wild$max_tonnes<-wild$max_proportion * wild_for_33T[2]


pdf(file='figures/wild_caught_species.pdf', height=7, width=11)
ggplot(species_prop, aes(fct_reorder(Species, mean_proportion*100), mean_proportion*100, ymin = min_proportion*100, ymax=max_proportion*100)) +
  geom_pointrange() + coord_flip() + labs(x = '', y = '% of wild-caught')
dev.off()
write.csv(species_prop, file = 'data/results/weighted_wildcaught_species_percent.csv')
