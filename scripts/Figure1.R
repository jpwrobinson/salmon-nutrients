
source('scripts/read_data.R')

basesize=11
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

bar<-data.frame(y = c(salmon_scot_2014, FO_salmon_scot_2014),
                x = c(1,1),
                lab = c(paste(scales::comma(labb), 'tonnes in 2014'), 'Fish oil'))
# levels(bar$x)<-unique(bar$x[c(1,2)])

goil<-ggplot() + 
  geom_bar(data=bar[2,], aes(1, y, fill=lab), stat='identity', position='stack') +
  geom_bar(data=bar[1,], aes(1, y, fill=lab), stat='identity', position='stack', alpha=0.25, fill='red') +
  geom_hline(yintercept = bar$y[1], col='red') +	
  geom_text(data = bar[1,], aes(x = x, y = y, label = lab), size=basesize-7, vjust=-0.25) +
  geom_text(data = bar[2,], aes(x = x, y = y, label = lab), size=basesize-7, vjust=1.3) +
  geom_text(data = bar_ui, aes(x = x, y = upper, label = lab), size=basesize-7, vjust=-.25) +
  geom_pointrange(data = bar_ui, aes(x, mean, ymin=lower, ymax=upper)) +
  th + theme(axis.text.x = element_blank()) +
  scale_y_continuous(limits=c(0, 540000), labels=scales::comma, expand=c(0,0)) +
  scale_x_reverse(breaks=rev(c(1:4)), expand=c(0.3, 0)) +
  guides(fill=FALSE) +
  labs(y = 'Wet weight, t', x ='')

gprod<-ggplot(data = production, aes(Year, Tonnes)) + 
    geom_segment(x = 0, xend = 2014, y = labb, yend = labb, col='red', alpha=0.5) +  
    annotate(geom='text', x = 2011, y = labb, 
             label=paste(scales::comma(labb), 'tonnes in 2014'), 
             col='black', alpha=1, vjust=-1, size=3) +  
    geom_line(size=1) + 
      labs(x = '', y='Salmon production, t') + th +
  scale_y_continuous(labels=scales::comma) +
    theme(axis.line.x= element_line(colour='grey'))
  

pdf(file='figures/Figure1.pdf', height=4.5, width=10)
cowplot::plot_grid(gprod, goil, nrow=1, labels=c('a', 'b'))
dev.off()


