setwd('salmon-nutrients')
library(tidyverse)
library(funk); theme_set(theme_sleek())



fb<-read.csv('data/feedback_species_nutrient_profiles.csv')
nut<-read.delim('data/SppNutrients_Oct2020_MarineFish.csv', sep= ';')

colnames(fb)[c(1,9,10)]<-c('product', 'Omega-3 (EPA)', 'Omega-3 (DHA)')
fb<-fb %>% 
  select(species, ScientificName, calcium.mg:'Omega-3 (DHA)') %>%
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
nut$species<-fb$species[match(nut$ScientificName_corrected, fb$ScientificName)]
colnames(nut)[2]<-'ScientificName'
nut<-nut %>% select(colnames(fb))

## combine nutrient profiles depending on purpose,

## wild-fish for food, use UK govt values
plated<-c('Atlantic salmon', 'Herring', 'Sardine', 'Anchovy')
nonplated<-c('Menhaden', 'Blue whiting', 'Capelin', 'Sprat')

nuts<-rbind(fb %>% filter(species %in% plated ), 
		nut %>% filter(species %in% nonplated),
		fb %>% filter(species %in% nonplated & nutrient %in% c( 'Omega-3 (EPA)', 'Omega-3 (DHA)'))) %>%
	mutate(salmon = ifelse(species == 'Atlantic salmon', TRUE, FALSE))

nuts$species<-factor(nuts$species, levels=rev(unique(nuts$species)[c(1,4,7,6,2,5,3,8)]))

nuts<-nuts %>% filter(nutrient != 'Omega3_mu')


nuts$nutrient<-factor(nuts$nutrient, levels = unique(nuts$nutrient))
levels(nuts$nutrient)<-c("'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'), 
	"'Zinc, mg'",expression('Vitamin A, '*mu*'g'), "'Omega-3 (EPA), g'","'Omega-3 (DHA), g'")

nuts$product<-nuts$species
nuts$product<-recode(nuts$product,  'Atlantic salmon'='Atlantic salmon\n(hot smoked)')
nuts$product<-recode(nuts$product,  'Herring'='Herring\n(grilled)')
nuts$product<-recode(nuts$product,  'Sardine'='Sardine\n(canned, brine)')
nuts$product<-recode(nuts$product,  'Anchovy'='Anchovy\n(canned,??)')

pdf(file = 'figures/Figure2.pdf', height=5, width=15)
ggplot(nuts, aes(product, value)) +
      geom_bar(stat='identity', aes(fill=salmon), alpha=0.5) +
      facet_wrap(~nutrient, nrow=1, scales='free_x', labeller=label_parsed)+
      coord_flip() +
      guides(fill=FALSE) +
      labs(x = '', y = 'Micronutrient concentration') +
      scale_fill_manual(values=c('darkgrey', 'red'))
dev.off()