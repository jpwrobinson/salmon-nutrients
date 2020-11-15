setwd('salmon-nutrients')
library(tidyverse); library(cowplot)
library(funk); theme_set(theme_sleek())


basesize=11
th<-theme(
  strip.text = element_text(colour='black', size=basesize),
  axis.text.x = element_text(colour='black', size=basesize),
  axis.text.y = element_text(colour='black', size=basesize),
  axis.title = element_text(colour='black', size=basesize +1),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.line.y = element_line(colour='grey'),
  axis.line.x = element_line(colour='grey')) 



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
nuts$lab<-nuts$nutrient
levels(nuts$lab)<-c("'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'), 
	"'Zinc, mg'",expression('Vitamin A, '*mu*'g'), "'Omega-3 (EPA), g'","'Omega-3 (DHA), g'")

nuts$product<-nuts$species
# nuts$product<-recode(nuts$product,  'Atlantic salmon'='Atlantic salmon\n(hot smoked)')
# nuts$product<-recode(nuts$product,  'Herring'='Herring\n(grilled)')
# nuts$product<-recode(nuts$product,  'Sardine'='Sardine\n(canned, brine)')
# nuts$product<-recode(nuts$product,  'Anchovy'='Anchovy\n(canned,??)')



## micronutrient in : micronutrient out
nuts$catch<-NA
nuts$catch<-wild$mean_tonnes[match(nuts$species, wild$Species)]
nuts$catch_min<-wild$min_tonnes[match(nuts$species, wild$Species)]
nuts$catch_max<-wild$max_tonnes[match(nuts$species, wild$Species)]
nuts$catch[nuts$species == 'Atlantic salmon']<-salmon_scot_2014
nuts$catch_min[nuts$species == 'Atlantic salmon']<-salmon_scot_2014
nuts$catch_max[nuts$species == 'Atlantic salmon']<-salmon_scot_2014

nuts$group<-ifelse(nuts$species == 'Atlantic salmon', 'Out', 'In')
nuts$yield<-nuts$catch * nuts$value
nuts$yield_min<-nuts$catch_min * nuts$value
nuts$yield_max<-nuts$catch_max * nuts$value

mn<-nuts %>% group_by(nutrient, group) %>% 
            summarise(yield = sum(yield, na.rm=TRUE), 
                                                     yield_min = sum(yield_min, na.rm=TRUE), 
                                                     yield_max = sum(yield_max, na.rm=TRUE)) %>% 
        pivot_wider(names_from=group, values_from=c(yield, yield_min, yield_max)) %>%
        mutate(nutrient_deficit = yield_Out / yield_In * 100,
               nutrient_deficit_min = yield_Out / yield_min_In * 100,
               nutrient_deficit_max = yield_Out / yield_max_In * 100)
levels(mn$nutrient)<-c("Calcium", "Iron", 'Selenium','Zinc', 'Vitamin A', "Omega-3 (EPA)","Omega-3 (DHA)")


top<-ggplot(nuts, aes(product, value)) +
      geom_bar(stat='identity', aes(fill=salmon), alpha=0.5) +
      facet_wrap(~lab, nrow=1, scales='free_x', labeller=label_parsed)+
      coord_flip() +
      guides(fill=FALSE) +
      labs(x = '', y = expression(paste('micronutrient concentration, 100 g'^-1))) +
      scale_fill_manual(values=c('darkgrey', 'red')) +
      th

bot<-ggplot(mn, aes(nutrient, nutrient_deficit)) + 
      geom_pointrange(aes(ymin = nutrient_deficit_min, ymax = nutrient_deficit_max)) +
      geom_text(aes(y = nutrient_deficit_max, label = nutrient), hjust=1.2) +
      coord_flip() +
  scale_y_continuous(limits=c(-20, 400), breaks=seq(0, 400, 100)) +
  labs(x = '', y = '% nutrients retained') + th +
        theme(axis.text.y =element_blank(), 
              axis.line.y = element_blank(), 
              plot.margin=unit(c(0,2,0,4), 'cm'))


pdf(file = 'figures/Figure2.pdf', height=5, width=12)
plot_grid(top, bot, nrow=2, rel_heights=c(0.8, 1), labels=c('a', 'b'))
dev.off()
