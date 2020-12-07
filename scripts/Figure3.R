
source('scripts/scenarios.R')

ss_conc<-rbind(
  sa %>% select(nutrient, Scenario, portion),
  sb_wild_trim %>% select(nutrient, Scenario, portion)
)

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
    select(species, prop_portion, forage, Scenario)
) %>% mutate(species = factor(species)) %>% 
  group_by(Scenario) %>%
  arrange(desc(prop_portion)) %>%
  mutate(lab.ypos = round(cumsum(prop_portion) - 0.5*prop_portion, 0),
         prop_portion_max = cumsum(prop_portion))

ss_diet$prop_portion_min<-c(0, head(ss_diet$prop_portion_max, n=-1))
ss_diet$prop_portion_min[2]<-0

g1<-ggplot(ss_conc, aes(Scenario, portion)) + 
  geom_segment(aes(x=Scenario, xend=Scenario, y=0, yend=portion)) + 
  geom_point(size=4, alpha=0.8, shape=21) +
  facet_wrap(~lab, nrow=1, scales='free_y',labeller=label_parsed) +
  theme(
    legend.position = 'none'
    # strip.text.x=element_blank()
    ) +
  labs(x = '', y = expression(paste('micronutrient concentration, 100 g'^-1))) 
  
g2<-ggplot(ss_diet, aes(xmin=2,xmax=3, ymin=prop_portion_min, ymax=prop_portion_max)) +
      geom_rect(aes(fill=forage), col='white') + 
      xlim(0.5, 3.5) +
      facet_wrap(~Scenario, nrow =1) +
      coord_polar(theta='y', start=0) +
      scale_fill_manual(values=cols[c(1,2)]) +
      geom_text(data = ss_diet %>% filter(species == 'Atlantic salmon'),
                aes(x=2.5, y = lab.ypos, label = paste0(round(prop_portion,0), 'g')),size=3.5, color = "grey90") +
      geom_text(data = ss_diet %>% filter(species != 'Atlantic salmon'),
            aes(x=3.25, y = lab.ypos, label = species),size=3.5, color = "black") +
      theme_void() + theme(legend.title = element_blank())

pdf(file='figures/Figure3.pdf', height=5, width=12)
plot_grid(g1, g2, nrow=2)
dev.off()
