
source('scripts/read_sankey_data.R')
library(networkD3)

## create a dataframe with 10 nodes
nodes = data.frame("name" = c(#"Whole_fish", "Trimmings", 
                              'Fish meal', 'Fish oil', 
                              "Salmonids", "Eels", "Marine fish", "Tilapia", 
                              "Cyprinids", "Other freshwater fish", "Crustaceans", 
                              "Direct human consumption",
                              'Pig', 'Poultry', 'Other'))


## create edges with weights
links = as.data.frame(matrix(c(#0, 2, fo_prod, # node 0 -> node 2 
                               #0, 3, fm_prod, # node 0 -> node 3 
                               #1, 2, fm_prod, # node 1 -> node 2 with weight 2
                               #1, 3, fo_prod, # node 1 -> node 3 with weight 2
                               2, 4, fm_sp$vol[fm_sp$species == 'Salmonids'],
                               3, 4, fo_sp$vol[fo_sp$species == 'Salmonids'],
                               2, 6, fm_sp$vol[fm_sp$species == 'Marine fish'],
                               3, 6, fo_sp$vol[fo_sp$species == 'Marine fish'],
                               2, 5, fm_sp$vol[fm_sp$species == 'Eels'],
                               3, 5, fo_sp$vol[fo_sp$species == 'Eels'],
                               2, 7, fm_sp$vol[fm_sp$species == 'Tilapia'],
                               3, 7, fo_sp$vol[fo_sp$species == 'Tilapia'],
                               2, 8, fm_sp$vol[fm_sp$species == 'Cyprinids'],
                               2, 9, fm_sp$vol[fm_sp$species == 'Other freshwater fish'],
                               3, 9, fo_sp$vol[fo_sp$species == 'Other freshwater fish'],
                               2, 10, fm_sp$vol[fm_sp$species == 'Crustaceans'],
                               3, 10, fo_sp$vol[fo_sp$species == 'Crustaceans'],
                               3, 11, fo_sp$vol[fo_sp$species == 'Direct human consumption'],
                               2, 14, fm_sp$vol[fm_sp$species == 'Other'],
                               3, 14, fo_sp$vol[fo_sp$species == 'Other'],
                               2, 12, fm_sp$vol[fm_sp$species == 'Pig'],
                               3, 13, fm_sp$vol[fm_sp$species == 'Poultry']), 
                             byrow = TRUE, ncol = 3))

## set column names for links
names(links) = c("source", "target", "value")
links$source<-links$source - 2
links$target<-links$target - 2

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain([ "Fish meal", "Fish oil", 
                              "Salmonids", "Eels", "Marine fish", "Tilapia", 
                              "Cyprinids", "Other freshwater fish", "Crustaceans", 
                              "Direct human consumption",
                              "Pig", "Poultry", "Other"]) .range(["#67a9cf", "#d95f02",  
            "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77","grey", "grey", "grey"])'


## Draw Sankey Diagram
p = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",  colourScale = node_color)

pdf(file='figures/Figure4A.pdf', height=7, width=12)
print(p)
dev.off()



