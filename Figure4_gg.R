
source('scripts/read_sankey_data.R')
library(alluvial)
salmon.col<-'#d95f02'
## create a dataframe with 10 nodes
nodes = data.frame(node = c("Wild-caught fish", "Trimmings", 
                              'Fish meal & fish oil reduction',
                              'Fish oil', 'Fish meal',
                              "Salmonids_oil","Salmonids_meal",
                            "Eels_oil", "Eels_meal", 
                            "Marine fish_oil", "Marine fish_meal",
                            "Tilapia_oil", "Tilapia_meal", 
                              "Cyprinids_meal", 
                            "Other freshwater fish_oil","Other freshwater fish_meal",
                            "Crustaceans_oil","Crustaceans_meal", 
                              "Direct human consumption_oil",
                            'Other_oil','Other_meal',
                              'Pig_meal', 'Poultry_meal', 
                              "Salmonids", "Eels", "Marine fish", "Tilapia", 
                              "Other freshwater fish", "Cyprinids", "Crustaceans",
                              'Pig', 'Poultry'),
                   value = c(wild_caught_in_FMFO, trimmings_FMFO, trimmings_FMFO +wild_caught_in_FMFO,
                             fo_prod, fm_prod,
                             fm_sp$vol[fm_sp$species == 'Salmonids'],
                             fo_sp$vol[fo_sp$species == 'Salmonids'],
                             fm_sp$vol[fm_sp$species == 'Marine fish'],
                             fo_sp$vol[fo_sp$species == 'Marine fish'],
                             fm_sp$vol[fm_sp$species == 'Eels'],
                             fo_sp$vol[fo_sp$species == 'Eels'],
                             fm_sp$vol[fm_sp$species == 'Tilapia'],
                             fo_sp$vol[fo_sp$species == 'Tilapia'],
                             fm_sp$vol[fm_sp$species == 'Cyprinids'],
                             fm_sp$vol[fm_sp$species == 'Other freshwater fish'],
                             fo_sp$vol[fo_sp$species == 'Other freshwater fish'],
                             fm_sp$vol[fm_sp$species == 'Crustaceans'],
                             fo_sp$vol[fo_sp$species == 'Crustaceans'],
                             fo_sp$vol[fo_sp$species == 'Direct human consumption'],
                             fm_sp$vol[fm_sp$species == 'Other'],
                             fo_sp$vol[fo_sp$species == 'Other'],
                             fm_sp$vol[fm_sp$species == 'Pig'],
                             fm_sp$vol[fm_sp$species == 'Poultry'], 
                             prod$tonnes[prod$species == 'Salmonids'], 
                             prod$tonnes[prod$species == 'Eels'], 
                             prod$tonnes[prod$species == 'Marine fish'], 
                             prod$tonnes[prod$species == 'Tilapia'], 
                             prod$tonnes[prod$species == 'Other freshwater fish'], 
                             prod$tonnes[prod$species == 'Cyprinids'], 
                             prod$tonnes[prod$species == 'Crustaceans'], 
                             prod$tonnes[prod$species == 'Pig'], 
                             prod$tonnes[prod$species == 'Poultry']),
                   stage = c(rep('wild', 2), 'reduction', rep('FMFO', 2),
                             rep('feed', 18),
                             rep('food', 9)),
                   type = c('wild', 'wild', 'wild', 'Fish oil', 'Fishmeal',
                            rep(c('Fish oil', 'Fishmeal'), times = 4), 'Fishmeal', 'Fishmeal', 'Fish oil', 'Fishmeal', 'Fish oil',  'Fish oil',  'Fishmeal',  'Fish oil', 'Fishmeal', 'Fishmeal',rep('food', times = 9)),
                   allu = c('wild_fish', 'wild_trim', 'wild_fish', 'wild_fish', 'wild_fish', 'Salmonids', 'Salmonids', 'Eels', 'Eels', 'Marine fish', 'Marine fish', 
                            'Tilapia', 'Tilapia', 'Cyprinids', 'Other freshwater fish','Other freshwater fish', rep('Crustaceans',2), 'Direct human consumption',
                            'Other', 'Other','Pig', 'Poultry',  'Salmonids', 'Eels',"Marine fish", "Tilapia", 
                            "Other freshwater fish", "Cyprinids", "Crustaceans",'Pig', 'Poultry'))


n<-nodes %>% filter(stage%in% c('feed')) %>%
    mutate(type = factor(type, levels=c('Fishmeal', 'Fish oil', 'food')),
           allu = factor(allu, levels=rev(c('Salmonids', 'Eels', 'Tilapia', 'Marine fish', 'Direct human consumption',
                                        'Other', 'Other freshwater fish', 'Crustaceans', 'Cyprinids', 'Pig', 'Poultry'))))

pdf(file='figures/ill/figure4_for_illustrator_FMFO.pdf', width=3, height=4)

## plot FMFO allocations
with(n %>% filter(stage %in% c('feed')), 
      alluvial(x = type, y = allu, freq=value,
               col=ifelse(type == 'Fish oil', salmon.col,'#67a9cf'),
               layer = type == "Fishmeal", cw=0.1,
     axes=FALSE,
     ann=FALSE))
dev.off()

pdf(file='figures/ill/figure4_for_illustrator_FMFO_labels.pdf', width=3, height=4)
with(n %>% filter(stage %in% c('feed')), 
     alluvial(x = type, y = allu, freq=value,
              col=ifelse(type == 'Fish oil', salmon.col,'#67a9cf'),
              layer = type == "Fishmeal",
              axes=FALSE))
dev.off()

pdf(file='figures/ill/figure4_for_illustrator_wild.pdf', width=3, height=4)
## plot wild allocations
n2<-nodes[1:2,]
with(n2, 
     alluvial(x = type, y = allu, freq=value, ann=FALSE, cw=0.1))
dev.off()


ggplot(nodes, aes(area = value, fill=node)) + geom_treemap()

