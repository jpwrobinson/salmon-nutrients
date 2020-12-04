
source('scripts/scenarios.R')


ggplot(sb_diet, aes(species, prop_portion)) +
  geom_bar(stat='identity') +
  coord_polar()



g1<-ggplot(ss, aes(nutrient, yield, fill = Scenario)) + 
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~nutrient, scales='free') +
  # coord_flip() + 
  theme(
    # strip.text.x=element_blank(),
    axis.text.x=element_blank()) +
  labs(x = '' , y = 'Nutrient yield')


pdf(file='figures/Figure3.pdf', height=7, width=12)
g1
dev.off()