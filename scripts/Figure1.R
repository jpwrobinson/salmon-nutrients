
source('scripts/read_data.R')

basesize=11
salmon.col<-'#e6550d'
th<-theme(
      axis.text.x = element_text(colour='black', size=basesize), 
          axis.text.y = element_text(colour='black', size=basesize),
          axis.title = element_text(colour='black', size=basesize +1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line.y = element_line(colour='grey')) 

labb<-production$Tonnes[production$Year==2014]

## offset for labels
k = 20000

bar<-data.frame(y = c(salmon_scot_2014, FO_salmon_scot_2014,FM_2014+FO_salmon_scot_2014,FM_spare),
                y2 = c(salmon_scot_2014, FO_salmon_scot_2014-k,FM_2014+FO_salmon_scot_2014-k,FM_spare-k),
                x = c(1,0.6,0.6, 3.3),
                lab = c(paste(scales::comma(labb), 'tonnes in 2014'), 'Fish oil', 'Fish meal', 'Spare fishmeal\nfrom wild-caught'))
# levels(bar$x)<-unique(bar$x[c(1,2)])

bar_ui<-data.frame(y = c(trimmings_for_33T, rev(wild_for_33T)),
                   x = c(rep(3,3), rep(3.5,3)),
                   lab =c(rep('Trimmings',3), rep('Wild-caught fish', 3)),
                   stat=rep(c('lower', 'mean', 'upper'), times=2)) %>%
  pivot_wider(names_from='stat', values_from = 'y')


goil<-ggplot() + 
  # geom_bar(data=bar[2,], aes(1, y), stat='identity', position='stack') +
  geom_segment(data=bar[1,], aes(1,xend=1, y=FO_salmon_scot_2014+FM_2014, yend=y), alpha=0.3, col=salmon.col, size=22.8) +
  geom_segment(data=bar[3,], aes(1,xend=1, y=FO_salmon_scot_2014, yend=y),size=22.8, alpha=0.5, col='#67a9cf') +
  geom_segment(data=bar[2,], aes(1,xend=1, y = 0, yend=y), size=22.8, alpha=1,col=salmon.col) +
  geom_segment(data=bar[4,], aes(3,xend=3, y = 0, yend=y), size=22.8, alpha=0.3,col='#67a9cf') +
  geom_hline(yintercept = bar$y[1], col=salmon.col) +	
  geom_label(data = bar[1,], aes(x = x, y = y-20000, label = lab), size=basesize-8, fill='grey90', alpha=0.5, label.size=0) +
  geom_text(data = bar[1,], aes(x = x, y = y), label = 'Salmon production', size=basesize-8, vjust=-0.5) +
  # geom_text(data = bar[3,], aes(x = x, y = y, label = lab), size=basesize-7, vjust=2.5,hjust=0.5) +
  geom_text(data = bar_ui, aes(x = x, y = mean, label = lab), size=basesize-7, hjust=1.1, vjust=-0.7) +
  geom_pointrange(data = bar_ui, aes(x, mean, ymin=lower, ymax=upper)) +
  th + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_curve(aes(xend = 1.5, yend = 50000, x = 2.85, y = 210000),
             arrow = arrow(length = unit(0.02, "npc")), size=0.8, curvature = -0.1, angle=90, col='grey') +
  geom_curve(aes(xend = 1.5, yend = 40000, x = 3.59, y = 450000),
             arrow = arrow(length = unit(0.02, "npc")), size=0.8, curvature = -0.5, angle=90, col='grey') +
  geom_label(data = bar[2,], aes(x = x, y = y2, label = lab), size=basesize-7.5, label.padding = unit(0.2, "lines"),fontface='bold', col=salmon.col, fill='white') +
  geom_label(data = bar[3,], aes(x = x, y = y2, label = lab), size=basesize-7.5, label.padding = unit(0.2, "lines"), fontface='bold',col='#67a9cf' ) +
  geom_label(data = bar[4,], aes(x = x, y = y2-k, label = lab), size=basesize-8, label.padding = unit(0.2, "lines"), fontface='bold',col='#67a9cf' ) +
  scale_y_continuous(limits=c(0, 540000), labels=scales::comma, expand=c(0,0)) +
  # scale_x_reverse(breaks=rev(c(1:4)), expand=c(0.3, 0)) +
  scale_x_continuous(breaks=(c(1:4)),limits=c(0.5,3.6), expand=c(0.1, 0)) +
  guides(fill=FALSE) +
  labs(y = 'Wet weight, t', x ='')

gprod<-ggplot(data = production, aes(Year, Tonnes)) + 
    geom_segment(x = 0, xend = 2014, y = labb, yend = labb, col=salmon.col, alpha=0.5) +  
    annotate(geom='text', x = 2011, y = labb, 
             label=paste(scales::comma(labb), 'tonnes in 2014'), 
             col='black', alpha=1, vjust=-1, size=3) +  
    geom_line(size=1) + 
      labs(x = '', y='Salmon production, t') + th +
  scale_y_continuous(labels=scales::comma) +
    theme(axis.line.x= element_line(colour='grey'))


drops<-c('Cod', 'Silver smelt', 'Boarfish', 'Hake')
gwild<-ggplot(wild %>% filter(!Species %in% drops), aes(fct_reorder(Species, mean_tonnes), mean_tonnes))   +
      geom_pointrange(aes(ymin = min_tonnes, ymax=max_tonnes)) +
      coord_flip() + 
      geom_text(y = 165000, aes(x=Species, label=Species)) + 
      th +
      scale_y_continuous(labels=scales::comma, limits=c(0, 180000), breaks=c(20000, 50000, 100000, 150000)) +
      # scale_x_discrete(position='top') +
      labs(x = '', y = 'Catch in fish meal & fish oil, t') +
      theme(axis.line.x= element_line(colour='grey'), 
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

pdf(file='figures/Figure1.pdf', height=4.5, width=14)
cowplot::plot_grid(gprod, goil, gwild, nrow=1, labels=c('a', 'b', 'c'))
dev.off()








