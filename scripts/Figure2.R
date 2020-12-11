# setwd('salmon-nutrients')
library(tidyverse); library(cowplot)
library(funk); theme_set(theme_sleek())
source('scripts/read_data.R')

basesize=11
th<-theme(
strip.text = element_text(colour='black', size=basesize-1),
axis.text.x = element_text(colour='black', size=basesize-1),
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

## remove duplicates
nut <- nut[!duplicated(nut$Species),]

colnames(fb)[c(1,9,10,11,12)]<-c('product', 'Omega-3 (EPA)', 'Omega-3 (DHA)', 'Vitamin D', 'Vitamin B12')
fb<-fb %>% 
select(species, ScientificName, calcium.mg:'Vitamin B12') %>%
pivot_longer(-c(species, ScientificName), names_to = 'nutrient', values_to = 'value') %>%
mutate(source = 'Feedback UK')

nut<-nut %>% select(ScientificName_corrected, Calcium_mu, Iron_mu, Selenium_mu, Zinc_mu, Omega3_mu, Vitamin_A_mu) %>%
pivot_longer(-c(ScientificName_corrected), names_to = 'nutrient', values_to = 'value') %>%
filter(ScientificName_corrected %in% unique(fb$ScientificName)) %>%
mutate(source = 'Hicks/MacNeil Model')

nut$nutrient<-recode(nut$nutrient,  'Calcium_mu'='calcium.mg')
nut$nutrient<-recode(nut$nutrient,  'Iron_mu'='iron.mg')
nut$nutrient<-recode(nut$nutrient,  'Selenium_mu'='selenium.mug')
nut$nutrient<-recode(nut$nutrient,  'Zinc_mu'='zinc.mg')
nut$nutrient<-recode(nut$nutrient,  'Vitamin_A_mu'='vitamin.A.mug')
nut$species<-fb$species[match(nut$ScientificName_corrected, fb$ScientificName)]
colnames(nut)[1]<-'ScientificName'
nut<-nut %>% select(colnames(fb))

## combine nutrient profiles depending on purpose,

## wild-fish for food, use UK govt values
plated<-c('Atlantic salmon', 'Herring', 'Sardine', 'Anchovy',  'Blue whiting', 'Anchoveta')
nonplated<-c('Capelin','Menhaden', 'Sprat', 'Sand Eel', 'Norway Pout', 'Mackerel')

nuts<-rbind(
  fb %>% filter(species %in% plated ), 
	nut %>% filter(species %in% nonplated),
	fb %>% filter(species %in% nonplated & nutrient %in% c( 'Omega-3 (EPA)', 'Omega-3 (DHA)', 'Vitamin D', 'Vitamin B12'))) %>%
mutate(salmon = ifelse(species == 'Atlantic salmon', TRUE, FALSE))

nuts$species<-factor(nuts$species, levels=rev(unique(nuts$species)[c(1,4,7,10,9,8,2,6,12,3,5,11)]))

nuts<-nuts %>% filter(nutrient != 'Omega3_mu')


nuts$nutrient<-factor(nuts$nutrient, levels = unique(nuts$nutrient)[c(1:4,9, 7,6,5,8)])
nuts$lab<-nuts$nutrient
levels(nuts$lab)<-c("'Calcium (mg)'", "'Iron (mg)'", expression('Selenium ('*mu*'g)'), "'Zinc (mg)'",
expression('Vitamin B12 ('*mu*'g)'),
"'Om-3 DHA (g)'","'Om-3 EPA (g)'",expression('Vitamin A ('*mu*'g)'), expression('Vitamin D ('*mu*'g)'))

nuts$product<-nuts$species
# nuts$product<-recode(nuts$product,  'Atlantic salmon'='Atlantic salmon\n(hot smoked)')
# nuts$product<-recode(nuts$product,  'Herring'='Herring\n(grilled)')
# nuts$product<-recode(nuts$product,  'Sardine'='Sardine\n(canned, brine)')
# nuts$product<-recode(nuts$product,  'Anchovy'='Anchovy\n(canned,??)')



## micronutrient in : micronutrient out
nuts$catch<-NA
nuts$catch<-wild$mean_tonnes[match(nuts$species, wild$Species)]
## add uncertainty by prop wild fish needed (trimmings prop)
nuts$catch_min<-wild$max_trimmings[match(nuts$species, wild$Species)]
nuts$catch_max<-wild$min_trimmings[match(nuts$species, wild$Species)]
nuts$catch[nuts$species == 'Atlantic salmon']<-salmon_scot_2014_FO
nuts$catch_min[nuts$species == 'Atlantic salmon']<-salmon_scot_2014_FO
nuts$catch_max[nuts$species == 'Atlantic salmon']<-salmon_scot_2014_FO

nuts$group<-ifelse(nuts$species == 'Atlantic salmon', 'Out', 'In')
nuts$yield<-nuts$catch * nuts$value
nuts$yield_min<-nuts$catch_min * nuts$value
nuts$yield_max<-nuts$catch_max * nuts$value

catch<-data.frame(nutrient = 'Wet weight', yield_In =  wild_for_33T[2], yield_Out = salmon_scot_2014_FO, 
                    yield_min_In = wild_for_33T[3], yield_min_Out = salmon_scot_2014_FO,
                yield_max_In = wild_for_33T[1], yield_max_Out = salmon_scot_2014_FO)
catch$nutrient_deficit<-with(catch, yield_Out / yield_In * 100)
catch$nutrient_deficit_min<-with(catch, yield_Out / yield_min_In * 100)
catch$nutrient_deficit_max<-with(catch, yield_Out / yield_max_In * 100)
  
mn<-nuts %>% group_by(nutrient, group) %>% 
          summarise(yield = sum(yield, na.rm=TRUE), 
                                                   yield_min = sum(yield_min, na.rm=TRUE), 
                                                   yield_max = sum(yield_max, na.rm=TRUE)) %>% 
      pivot_wider(names_from=group, values_from=c(yield, yield_min, yield_max)) %>%
      mutate(nutrient_deficit = yield_Out / yield_In * 100,
             nutrient_deficit_min = yield_Out / yield_min_In * 100,
             nutrient_deficit_max = yield_Out / yield_max_In * 100)
# mn<-rbind(mn, catch) %>% mutate(nutrient = factor(nutrient))


mn$nutrient<-recode(mn$nutrient,  'calcium.mg' = 'Calcium')
mn$nutrient<-recode(mn$nutrient,  'iron.mg' = 'Iron')
mn$nutrient<-recode(mn$nutrient,  'selenium.mug' = 'Selenium')
mn$nutrient<-recode(mn$nutrient,  'zinc.mg' = 'Zinc')
mn$nutrient<-recode(mn$nutrient,  'vitamin.A.mug' = 'Vitamin A')
mn$nutrient<-recode(mn$nutrient,  'Omega-3 (EPA)' = 'Om-3 (EPA)')
mn$nutrient<-recode(mn$nutrient,  'Omega-3 (DHA)' = 'Om-3 (DHA)')
# levels(mn$nutrient)<-unique(mn$nutrient)

mn_edibles<-nuts %>% filter(species %in% edibles) %>%
        group_by(nutrient, group) %>% 
  summarise(yield = sum(yield, na.rm=TRUE), 
            yield_min = sum(yield_min, na.rm=TRUE), 
            yield_max = sum(yield_max, na.rm=TRUE)) %>% 
  pivot_wider(names_from=group, values_from=c(yield, yield_min, yield_max)) %>%
  mutate(nutrient_deficit = yield_Out / yield_In * 100,
         nutrient_deficit_min = yield_Out / yield_min_In * 100,
         nutrient_deficit_max = yield_Out / yield_max_In * 100)

mn_edibles$nutrient<-recode(mn_edibles$nutrient,  'calcium.mg' = 'Calcium')
mn_edibles$nutrient<-recode(mn_edibles$nutrient,  'iron.mg' = 'Iron')
mn_edibles$nutrient<-recode(mn_edibles$nutrient,  'selenium.mug' = 'Selenium')
mn_edibles$nutrient<-recode(mn_edibles$nutrient,  'zinc.mg' = 'Zinc')
mn_edibles$nutrient<-recode(mn_edibles$nutrient,  'vitamin.A.mug' = 'Vitamin A')
mn_edibles$nutrient<-recode(mn_edibles$nutrient,  'Omega-3 (EPA)' = 'Om-3 (EPA)')
mn_edibles$nutrient<-recode(mn_edibles$nutrient,  'Omega-3 (DHA)' = 'Om-3 (DHA)')
# levels(mn_edibles$nutrient)<-unique(mn_edibles$nutrient)

mn<-rbind(mn %>% mutate(type = 'All species'), mn_edibles %>% mutate(type='Edible fish'))

nuts$type<-ifelse(nuts$species%in% edibles, 'Edible', 'Non-edible')
nuts$type<-ifelse(nuts$species=='Atlantic salmon', 'Atlantic salmon', nuts$type)
nuts$species<-factor(nuts$species, levels=unique(nuts$species)[rev(c(1,5,4,6,8,2,9,7,3,10))])

top<-ggplot(nuts %>% filter(!product %in% c('Anchoveta', 'Menhaden')), 
            aes(species, value, fill=type)) +
      geom_bar(stat='identity', aes(fill=type), alpha=0.8) +
      facet_wrap(~lab, nrow=2, scales='free_x', labeller=label_parsed)+
      coord_flip() +
      guides(fill=FALSE) +
      labs(x = '', y = expression(paste('Micronutrient concentration (100 g'^-1, ')'))) +
      scale_fill_manual(values=cols) +
      th


bot<-ggplot(mn %>% filter(type=='Edible fish'), aes(fct_reorder(nutrient, nutrient_deficit), nutrient_deficit)) +
      geom_hline(yintercept=100, linetype=5, col='grey') +
      geom_pointrange(aes(ymin = nutrient_deficit_min, ymax = nutrient_deficit_max), 
                      fill=cols[2], col=cols[2],pch=21, size=0.8,position=position_dodge(width=0.5)) +
      # geom_text(aes(y = nutrient_deficit_min, labe  l = nutrient), hjust=1.1) +
      coord_flip() +
  scale_y_continuous(limits=c(-10, 150), breaks=seq(0, 150, 25)) +
  labs(x = '', y ='Edible nutrients from wild fish\nretained in farmed salmon (%)') + th +
        theme(#axis.text.y =element_blank(), 
              # axis.line.y = element_blank(), 
              axis.ticks = element_blank(),
              legend.title=element_blank(),
              legend.position = c(0.8, 0.2),
              plot.margin=unit(c(1,1,1,1), 'cm'))


pdf(file = 'figures/Figure2.pdf', height=5, width=13)
print(plot_grid(top, bot, nrow=1, rel_widths=c(1, 0.7), labels=c('a', 'b')))
dev.off()


