
source('scripts/scenarios.R')

# ss_conc<-rbind(
#   sa %>% select(nutrient, Scenario, portion),
#   sb_wild_trim %>% select(nutrient, Scenario, portion)
# )

ss_conc<-rbind(
  sb_wild_trim %>% select(nutrient, Scenario, portion),
  sc_conc %>% select(nutrient, Scenario, portion),
  sd_conc %>% select(nutrient, Scenario, portion)
  # se_conc %>% select(nutrient, Scenario, portion)
  )
  
ss_conc$baseline<-sa$portion[match(ss_conc$nutrient, sa$nutrient)]
ss_conc$relative<-ss_conc$portion / ss_conc$baseline * 100 - 100
ss_conc$scenario_lab<-str_split_fixed(ss_conc$Scenario, '\\ ', 2)[,1]

ss_conc$nutrient<-factor(ss_conc$nutrient, levels = unique(ss_conc$nutrient)[c(7,6,1:4,8,5,9)])
ss_conc$lab<-ss_conc$nutrient

levels(ss_conc$lab)<-c("Om-3 EPA","Om-3 DHA", 
                       "Calcium", "Iron", 'Selenium', 
                    "Zinc",'Vitamin A','Vitamin B12', 'Vitamin D')


ss_diet<-rbind(
  sa %>% filter(nutrient == 'calcium.mg') %>%
    mutate(species = 'Atlantic salmon', 
                forage = 'Atlantic salmon', 
                Scenario = 'I (Business-as-usual)') %>%
    select(species, prop_portion, forage, Scenario),
  sb_diet %>% filter(nutrient == 'calcium.mg') %>%
    mutate(Scenario = 'II (Trimmings-only salmon & wild fish)') %>%
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

ss_diet$tit<-str_split_fixed(ss_diet$Scenario, '\\ ', 2)[,1]
ss_diet$subtit<-str_split_fixed(ss_diet$Scenario, '\\ ', 2)[,2]
ss_diet$subtit<-str_replace_all(ss_diet$subtit, c('[()]'), '')

g1<-ggplot(ss_conc, aes(scenario_lab, relative)) + 
  geom_segment(aes(x=scenario_lab, xend=scenario_lab, y=0, yend=relative), col='grey') +
  geom_point(size=4, alpha=0.8, shape=21, aes(col=scenario_lab, fill=scenario_lab)) +
  geom_hline(aes(yintercept = 0), linetype=2, col=cols2[1]) +
  scale_colour_manual(values=cols2[c(2,3,4,4)]) +
  scale_fill_manual(values=cols2[c(2,3,4,4)]) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  facet_wrap(~lab,nrow=1,scales='free_y') +
  theme(
    legend.position = 'none'
    # strip.text.x=element_blank()
    ) +
  labs(x = '', 
       # y = expression(paste('concentration, 100 g'^-1)),
       y = 'Micronutrient conc.\n relative to scenario I (%)') 
  
g2<-ggplot(ss_diet, aes(xmin=2,xmax=3, ymin=prop_portion_min, ymax=prop_portion_max)) +
      geom_rect(aes(fill=forage), col='white') + 
      xlim(0.5, 3.5) +
      facet_wrap(~Scenario, nrow =1) +
      geom_text(x = Inf, y = 0.5, aes(label = tit), fontface = 'bold',size=4, vjust=-1) +
      geom_text(x = Inf, y = 0.5, aes(label = subtit), size=3) +
      coord_polar(theta='y', start=0) +
      scale_fill_manual(values=cols2[c(1,4,3, 2)]) +
      geom_text(data = ss_diet %>% filter(forage == 'Atlantic salmon'),
                aes(x=2.5, y = lab.ypos, label = paste0(round(prop_portion,0), 'g')),size=3.5, color = "grey90") +
      geom_text(data = ss_diet %>% filter(forage != 'Atlantic salmon'),
            aes(x=2.5, y = lab.ypos, label = paste0(round(prop_portion,0), 'g')),size=3.5, color = "grey90") +
      theme_void() + theme(legend.title = element_blank(),
                           strip.text.x =element_blank())
                           # plot.margin = unit(c(0.5,0.5,0.5, 0.5), 'cm'))

sea2 <- sea %>% select(scenario, unfished, stat) %>% pivot_wider(names_from = 'stat', values_from = 'unfished')
g3<-ggplot(sea2, aes(scenario, mean)) +
  # geom_pointrange(size=1, alpha=0.8,
  #                 aes(ymin = lower, ymax= upper, col=scenario, fill=scenario)) +
  # geom_point(size=4, alpha=0.8, 
  #                 aes(col=scenario, fill=scenario)) +
  geom_bar(stat='identity', fill=cols[2], col='white') +
  scale_y_continuous(labels=scales::comma) +
  # scale_colour_manual(values=cols2[c(2,3,4)]) +
  # scale_fill_manual(values=cols2[c(2,3,4)]) +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0.5,0,0.5,3), 'cm')
    # strip.text.x=element_blank()
  ) +
  labs(x = '', 
       # y = expression(paste('concentration, 100 g'^-1)),
       y = 'Spare wild-caught fish (t)') 

g4<-ggplot(fm, aes(scenario, fishmeal)) +
  geom_bar(col='white', stat='identity', fill='#67a9cf') +
  scale_y_continuous(limits=c(0, max(tonnes$t)), labels=scales::comma) +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0.5,1.5,0.5,1.5), 'cm')
    # strip.text.x=element_blank()
  ) +
  labs(x = '', 
       # y = expression(paste('concentration, 100 g'^-1)),
       y = 'Fishmeal required (t)') 


g5<-ggplot(tonnes, aes(scenario, tonnes)) +
  geom_bar(aes(x=scenario, y=t, fill=s),col='white', stat='identity') +
  scale_fill_manual(values=cols2[c(1,4,3,2)]) +
  scale_y_continuous(labels=scales::comma) +
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0.5,3,0.5,0), 'cm')
    # strip.text.x=element_blank()
  ) +
  labs(x = '', 
       # y = expression(paste('concentration, 100 g'^-1)),
       y = 'Edible seafood (t)') 

panel_c<-plot_grid(g3, g4, g5, nrow=1, align='h')

pdf(file='figures/Figure3.pdf', height=8, width=12)
plot_grid(g2, g1, panel_c, nrow=3, labels=c('a', 'b', 'c'), rel_heights=c(1,0.8, 1))
dev.off()





