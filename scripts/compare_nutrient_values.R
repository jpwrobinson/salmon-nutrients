setwd('salmon-nutrients')
library(tidyverse)
library(funk); theme_set(theme_sleek())


### compare Feedback's original model estimates with Aaron's latest

fb<-read.csv('data/feedback_species_nutrient_profiles.csv')
nut<-read.delim('data/SppNutrients_Oct2020_MarineFish.csv', sep= ';')
  
fb<-fb %>% 
  select(species, ScientificName, calcium.mg:vitamin.A.mug) %>%
  pivot_longer(-c(species, ScientificName), names_to = 'nutrient', values_to = 'value') %>%
  mutate(source = 'Feedback UK')
  
nut<-nut %>% 
  select(Species, ScientificName_corrected, Calcium_mu, Iron_mu, Selenium_mu, Zinc_mu, Omega3_mu, Vitamin_A_mu) %>%
  pivot_longer(-c(Species, ScientificName_corrected), names_to = 'nutrient', values_to = 'value') %>%
  filter(ScientificName_corrected %in% fb$ScientificName) %>%
  mutate(source = 'Hicks/MacNeil Model')

nut$nutrient<-recode(nut$nutrient,  'Calcium_mu'='calcium.mg')
nut$nutrient<-recode(nut$nutrient,  'Iron_mu'='iron.mg')
nut$nutrient<-recode(nut$nutrient,  'Selenium_mu'='selenium.mug')
nut$nutrient<-recode(nut$nutrient,  'Zinc_mu'='zinc.mg')
nut$nutrient<-recode(nut$nutrient,  'Vitamin_A_mu'='vitamin.A.mug')

colnames(nut)<-colnames(fb)



mast<-rbind(nut,fb) %>% filter(!species %in% c('Mussels', 'Vannamei shrimp'),
                               nutrient != 'Omega3_mu')

pdf(file='figures/nutrient_profiles.pdf', height=5, width=12)
ggplot(mast, aes(ScientificName, value, col=source)) +
      geom_point() +
      facet_wrap(~nutrient, nrow=1, scales='free_x') +
      coord_flip() +
      labs(x = '', y = 'micronutrient value')
dev.off()