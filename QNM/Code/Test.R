#############################################################################
# Salish Sea Marine Survival Project                                        #
# Qualitative Network Analysis                                              #
# Using diagraphs created in Dia software, use QNA packages and scripts to  # 
#   evaluate outcomes of press perturbations for Salish Sea QN model        #
#                                                                           #
# Network Analysis Component                                                #
#                                                                           #
# Generated on 10/20/2016                                                   #
# K. Sobocinski                                                             #
# kathryn.sobocinski@noaa.gov                                               #
# Long Live the Kings/NOAA NWFSC                                            #
#############################################################################

setwd("C:\\Users\\Kathryn.Sobocinski\\Documents\\SSMSP\\QNA")

#Load XML first--dependency not inlcuded in QPress
library(XML)
library(tcltk2)
library(QPress)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(igraph)
library(network)
library(intergraph)

#Run model and generate simulations
ss=model.dia("SSv7.dia") #~ 122,000 model runs gets 10000 stable
simSS <- system.simulate(10000, ss)
#simSS5 <- system.simulate(1000, ss5)

#To get the total number of runs to produce x accepted runs
simSS$total
#Output (inverse community matrices)
simSS$As
#Potential edges  
PotEdge=length(nodes)*length(nodes)
PotEdge
#Extant edges
is.data.frame(edges)
dim(edges) #148x5
ExtEdge=length(edges$From)

NetworkDens=ExtEdge/PotEdge #0.1359045 for v8


#For generating an adjacency matrix (-1,1)
edges <- simSS$edges
Salish=adjacency.matrix(edges=edges, labels = T)
adjacency.image(edges=edges)
## http://kateto.net/networks-r-igraph
## https://cran.r-project.org/web/packages/network/network.pdf

demo(package="igraph")
demo("centrality", package="igraph")


#This seems to be working, except directions aren't accounted for
g=graph_from_adjacency_matrix(Salish, mode="directed", weighted=T)
g
gsize(g) #148
as_edgelist(g)
plot.igraph(g)
plot.igraph(g, layout=layout_with_fr(g))
V(g)
E(g)
degSalish=degree(g) #May be able to use this as nodal distance?
hist(degree(g))
betweenness(g)

#LondonR_Workshop_Network_Analysis
sp=shortest_paths(g, 28, 1:33)


edge_connectivity(g, source=27, target=28 )
edge_connectivity(g, source=NULL, target=NULL )

distance_table(g, directed = TRUE)
mean_distance(g)
all_shortest_paths(g, from=27, to = V(g), mode = c("all"))

distances(g)
shortest_paths(g, 5)
all_shortest_paths(g, 1, 6:8)
mean_distance(g)


#This does not seem to be working
# g=graph.incidence(as.matrix(Salish),directed=T)
# gsize(g)
# incident(g)
# is_bipartite(g)
# knn(g)
# as_edgelist(g)

