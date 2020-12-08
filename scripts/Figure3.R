
source('scripts/scenarios.R')

# ss_conc<-rbind(
#   sa %>% select(nutrient, Scenario, portion),
#   sb_wild_trim %>% select(nutrient, Scenario, portion)
# )

ss_conc<-rbind(
  sb_wild_trim %>% select(nutrient, Scenario, portion),
  sc_conc %>% select(nutrient, Scenario, portion),
  sd_conc %>% select(nutrient, Scenario, portion)
  )
  
ss_conc$baseline<-sa$portion[match(ss_conc$nutrient, sa$nutrient)]
ss_conc$relative<-ss_conc$portion / ss_conc$baseline * 100 - 100
ss_conc$scenario_lab<-substring(ss_conc$Scenario, 1, 1)

ss_conc$nutrient<-factor(ss_conc$nutrient, levels = unique(ss_conc$nutrient)[c(6,7,1:5,8,9)])
ss_conc$lab<-ss_conc$nutrient
unique(ss_conc$nutrient)
levels(ss_conc$lab)<-c("'Om-3 (EPA), g'","'Om-3 (DHA), g'", 
                       "'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'), 
                    "'Zinc, mg'",expression('Vitamin A, '*mu*'g'), '"Vitamin D"', '"Vitamin B12"')


ss_diet<-rbind(
  sa %>% filter(nutrient == 'calcium.mg') %>%
    mutate(species = 'Atlantic salmon', 
                forage = 'Atlantic salmon', 
                Scenario = 'A (business-as-usual)') %>%
    select(species, prop_portion, forage, Scenario),
  sb_diet %>% filter(nutrient == 'calcium.mg') %>%
    mutate(Scenario = 'B (trimmings-only salmon + wild fish)') %>%
    select(species, prop_portion, forage, Scenario),
  sc_diet %>% filter(nutrient == 'calcium.mg') %>%
    select(species, prop_portion, forage, Scenario),
  sd_diet %>% filter(nutrient == 'calcium.mg') %>%
    select(species, prop_portion, forage, Scenario)
  ) %>% mutate(species = factor(species)) %>% 
  group_by(Scenario, forage) %>%
  summarise(prop_portion = sum(prop_portion)) %>% ungroup() %>%
  group_by(Scenario) %>%
  arrange(desc(prop_portion)) %>%
  mutate(lab.ypos = round(cumsum(prop_portion) - 0.5*prop_portion, 0),
         prop_portion_max = cumsum(prop_portion))

ss_diet$prop_portion_min<-c(0, head(ss_diet$prop_portion_max, n=-1))
ss_diet$prop_portion_min[c(2,3,5)]<-0
ss_diet$prop_portion_min[c(8)]<-ss_diet$prop_portion_max[c(4)]
ss_diet$prop_portion_min[c(9)]<-ss_diet$prop_portion_max[c(6)]

g1<-ggplot(ss_conc, aes(scenario_lab, relative)) + 
  geom_segment(aes(x=scenario_lab, xend=scenario_lab, y=0, yend=relative), col='grey') +
  geom_point(size=4, alpha=0.8, shape=21, aes(col=scenario_lab, fill=scenario_lab)) +
  geom_hline(aes(yintercept = 0), linetype=2, col=cols2[1]) +
  scale_colour_manual(values=cols2[c(2,3,4)]) +
  scale_fill_manual(values=cols2[c(2,3,4)]) +
  facet_wrap(~lab,nrow=1,scales='free_y', labeller=label_parsed) +
  theme(
    legend.position = 'none'
    # strip.text.x=element_blank()
    ) +
  labs(x = '', 
       # y = expression(paste('concentration, 100 g'^-1)),
       y = '% change from scenario A') 
  
g2<-ggplot(ss_diet, aes(xmin=2,xmax=3, ymin=prop_portion_min, ymax=prop_portion_max)) +
      geom_rect(aes(fill=forage), col='white') + 
      xlim(0.5, 3.5) +
      facet_wrap(~Scenario, nrow =1) +
      coord_polar(theta='y', start=0) +
      scale_fill_manual(values=cols2[c(1,4,3, 2)]) +
      geom_text(data = ss_diet %>% filter(forage == 'Atlantic salmon'),
                aes(x=2.5, y = lab.ypos, label = paste0(round(prop_portion,0), 'g')),size=3.5, color = "grey90") +
      geom_text(data = ss_diet %>% filter(forage != 'Atlantic salmon'),
            aes(x=2.5, y = lab.ypos, label = paste0(round(prop_portion,0), 'g')),size=3.5, color = "grey90") +
      theme_void() + theme(legend.title = element_blank())
                           # plot.margin = unit(c(0.5,0.5,0.5, 0.5), 'cm'))

g3<-ggplot(sea, aes(scenario, unfished_prop)) +
  geom_segment(aes(x=scenario, xend=scenario, y=0, yend=unfished_prop), col='grey') +
  geom_point(size=4, alpha=0.8, shape=21, aes(col=scenario, fill=scenario)) +
  scale_colour_manual(values=cols2[c(2,3,4)]) +
  scale_fill_manual(values=cols2[c(2,3,4)]) +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0.5,2,0.5,3), 'cm')
    # strip.text.x=element_blank()
  ) +
  labs(x = '', 
       # y = expression(paste('concentration, 100 g'^-1)),
       y = 'unfished biomass, %') 

g4<-ggplot(tonnes, aes(scenario, tonnes)) +
  geom_bar(aes(x=scenario, y=t, fill=s),col='white', stat='identity') +
  scale_fill_manual(values=cols2[c(1,4,3,2)]) +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0.5,3,0.5,2), 'cm')
    # strip.text.x=element_blank()
  ) +
  labs(x = '', 
       # y = expression(paste('concentration, 100 g'^-1)),
       y = 'edible seafood produced, t') 

panel_c<-plot_grid(g3, g4, align='h')

pdf(file='figures/Figure3.pdf', height=8, width=12)
plot_grid(g2, g1, panel_c, nrow=3, labels=c('a', 'b', 'c'))
dev.off()




