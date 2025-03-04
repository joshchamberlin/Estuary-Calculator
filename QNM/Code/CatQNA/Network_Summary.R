setwd("C:\\Users\\Catalina.Burch\\Documents\\GitHub\\Estuary-Calculator\\Code\\CatQNA")

#load libraries
library(XML)
library(tcltk2)
library(QPress)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(tidyverse)
library(igraph) #network analysis
library(network)
library(intergraph)

#Load Model
estuary<-model.dia("V1Directed.dia")

#load dia network
e_edgelist <- estuary %>% 
  select(From, To)

#convert to igraph format
e_igraph = graph_from_data_frame(e_edgelist, directed = T)

#Network Clustering Coefficient
transitivity(e_igraph, type = "global")


#Node Metrics
#Total Degree: measures the total connections of a node, regardless of direction of connections
  #note that the feedback loops count as 2 connections
degree <- as.data.frame(degree(e_igraph)) %>% 
  rownames_to_column()
mean(degree$`degree(e_igraph)`)
#In Degree: measures the total connections that flow into a node
in_degree <- as.data.frame(degree(e_igraph, mode = c('in')))%>% 
  rownames_to_column()

#Out Degree: measures the total connections that flow out of a node
out_degree <- as.data.frame(degree(e_igraph, mode = c('out')))%>% 
  rownames_to_column()

#Closeness Centrality: measures the closeness of one node to all other nodes in the network
#A high value means that a node, on average, can reach all other nodes in a few steps.

closeness <- as.data.frame(closeness(e_igraph, vids = V(e_igraph), mode = 'in')) %>% 
  rownames_to_column()

#Betweenness Centrality: measures the importance of a node in calculating the shortest paths
#of all nodes in a network. A high value means that a node, if removed from the network, 
#will make the shortest path calculation longer for many other nodes in the network. 
#It is calculated as the number of shortest paths that pass through the node.

between <- as.data.frame(betweenness(e_igraph, v = V(e_igraph), directed = T)) %>% 
  rownames_to_column()

#Eigenvector Centrality (sometimes called PageRank Centrality) measures the influence a node 
#has on a network. A node has high influence if it is connected to many nodes who themselves 
#have high influences.

eigen <- as.data.frame(eigen_centrality(e_igraph, directed = T)$vector) %>% 
  rownames_to_column()

#(Local) Clustering Coefficient measures how close the neighbors of a node all connect to 
#each other, and thus how embedded a node is in its local networks. It is also called the 
#local clustering coefficient.

cluster <- as.data.frame(transitivity(e_igraph, type = 'local')) %>% 
  rownames_to_column()

#Merge node data into one df
node_summary <- merge(degree, in_degree, by = "rowname") %>% 
  merge(out_degree) %>%
  merge(closeness) %>% 
  merge(between) %>% 
  merge(eigen) %>% 
  merge(cluster)

#rename columns
colnames(node_summary) <- c("node", "degree", "in degree", "out degree", "closeness",
                            "betweeness", "eigan", "cluster")

#export to excel
write_csv(node_summary, "node_summary.csv")




#Figures

node_summary %>% 
  ggplot(aes(x=degree, y = fct_reorder(node, degree))) + 
  geom_col()+
  theme_minimal()+
  ggtitle("Degree")

#Degrees
node_summary %>% 
  pivot_longer(cols = c(`in degree`:`out degree`), names_to = "in_out", values_to = "degrees") %>% 
  ggplot(aes(x=degrees, y = fct_reorder(node, degree), fill = in_out)) + 
  geom_bar(position = "stack", stat = "identity")+
  scale_x_continuous(expand = c(0,0))+
  labs(y = "Node")+
  theme_minimal()+
  ggtitle("QNM Node Degree")

#Closeness
node_summary %>%
  na.omit() %>% 
  ggplot(aes(x=closeness, y = fct_reorder(node, closeness)))+
  geom_col()+
  scale_x_continuous(expand = c(0,0))+
  labs(y = "Node")+
  theme_minimal()+
  ggtitle("QNM Closeness Centrality")
  
#Betweeness
node_summary %>%
  na.omit() %>% 
  ggplot(aes(x=betweeness, y = fct_reorder(node, betweeness)))+
  geom_col()+
  scale_x_continuous(expand = c(0,0))+
  labs(y = "Node")+
  theme_minimal()+
  ggtitle("QNM Betweeness Centrality")

#Eigen
node_summary %>%
  na.omit() %>% 
  ggplot(aes(x=eigan, y = fct_reorder(node, eigan)))+
  geom_col()+
  scale_x_continuous(expand = c(0,0))+
  labs(y = "Node")+
  theme_minimal()+
  ggtitle("QNM Eigenvector Centrality")

#Cluster
node_summary %>%
  na.omit() %>% 
  ggplot(aes(x=cluster, y = fct_reorder(node, cluster)))+
  geom_col()+
  scale_x_continuous(expand = c(0,0))+
  labs(y = "Node")+
  theme_minimal()+
  ggtitle("QNM Clustering Coefficient")

