
source('scripts/read_sankey_data.R')
library(networkD3)

## create a dataframe with 10 nodes
nodes = data.frame("name" = c("Wild-caught fish", "Trimmings", 
                              'Fish meal & fish oil reduction',
                              'Fish oil', 'Fish meal',
                              "Salmonids", "Eels", "Marine fish", "Tilapia", 
                              "Cyprinids", "Other freshwater fish", "Crustaceans", 
                              "Direct human consumption",
                              'Pig', 'Poultry', 'Other',
                              "Salmonids", "Eels", "Marine fish", "Tilapia", 
                              "Other freshwater fish", "Cyprinids", "Crustaceans",
                              'Pig', 'Poultry'
                              ))


## create edges with weights
links = as.data.frame(matrix(c(0, 2, wild_caught_in_FMFO, # node 0 -> node 2 
                               1, 2, trimmings_FMFO, # node 0 -> node 3 
                               2, 3, fo_prod, # node 0 -> node 2 
                               2, 4, fm_prod, # node 0 -> node 3 
                               # 1, 2, fm_prod, # node 1 -> node 2 with weight 2
                               # 1, 3, fo_prod, # node 1 -> node 3 with weight 2
                               4, 5, fm_sp$vol[fm_sp$species == 'Salmonids'],
                               3, 5, fo_sp$vol[fo_sp$species == 'Salmonids'],
                               4, 7, fm_sp$vol[fm_sp$species == 'Marine fish'],
                               3, 7, fo_sp$vol[fo_sp$species == 'Marine fish'],
                               4, 6, fm_sp$vol[fm_sp$species == 'Eels'],
                               3, 6, fo_sp$vol[fo_sp$species == 'Eels'],
                               4, 8, fm_sp$vol[fm_sp$species == 'Tilapia'],
                               3, 8, fo_sp$vol[fo_sp$species == 'Tilapia'],
                               4, 9, fm_sp$vol[fm_sp$species == 'Cyprinids'],
                               4, 10, fm_sp$vol[fm_sp$species == 'Other freshwater fish'],
                               3, 10, fo_sp$vol[fo_sp$species == 'Other freshwater fish'],
                               4, 11, fm_sp$vol[fm_sp$species == 'Crustaceans'],
                               3, 11, fo_sp$vol[fo_sp$species == 'Crustaceans'],
                               3, 12, fo_sp$vol[fo_sp$species == 'Direct human consumption'],
                               4, 15, fm_sp$vol[fm_sp$species == 'Other'],
                               3, 15, fo_sp$vol[fo_sp$species == 'Other'],
                               4, 13, fm_sp$vol[fm_sp$species == 'Pig'],
                               3, 14, fm_sp$vol[fm_sp$species == 'Poultry'], 
                               5, 16, prod$tonnes[prod$species == 'Salmonids'], 
                               6, 17, prod$tonnes[prod$species == 'Eels'], 
                              7, 18, prod$tonnes[prod$species == 'Marine fish'], 
                              8, 19, prod$tonnes[prod$species == 'Tilapia'], 
                              10, 20, prod$tonnes[prod$species == 'Other freshwater fish'], 
                              9, 21, prod$tonnes[prod$species == 'Cyprinids'], 
                              11, 22, prod$tonnes[prod$species == 'Crustaceans'], 
                              13, 23, prod$tonnes[prod$species == 'Pig'], 
                              14, 24, prod$tonnes[prod$species == 'Poultry']),
                             byrow = TRUE, ncol = 3))

## set column names for links
names(links) = c("source", "target", "value")
# links$source<-links$source - 2
# links$target<-links$target - 2

## Create custom color list using d3 for each node
node_color <- 'd3.scaleOrdinal() .domain([ "Fish meal", "Fish oil", 
                              "Salmonids", "Eels", "Marine fish", "Tilapia", 
                              "Cyprinids", "Other freshwater fish", "Crustaceans", 
                              "Direct human consumption",
                              "Pig", "Poultry", "Other",
                              "Salmonids", "Eels", "Marine fish", "Tilapia", 
                              "Cyprinids", "Other freshwater fish", "Crustaceans", "Pig", "Poultry"]) .range(["#67a9cf", "#d95f02",  
            "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77","grey", "grey", "grey",
             "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77", "#1b9e77","grey", "grey"])'


## Draw Sankey Diagram
p = sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",  
                  colourScale = node_color,
                 sinksRight = FALSE)

pdf(file='figures/Figure4A.pdf', height=7, width=12)
print(p)
dev.off()




