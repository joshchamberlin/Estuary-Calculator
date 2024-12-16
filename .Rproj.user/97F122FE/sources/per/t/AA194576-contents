#load libraries
library(readr)
library(tidyverse)
library(here)
library(patchwork)
library(igraph)
library(ggraph)
library(readxl)



##NETWORK VISUALIZATION
#load data
edges <- read_excel(here("data/QNMEdges.xlsx"))
nodes <- read_excel(here("data/QNMNodes.xlsx"))

#Converting to igraph object

network <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)

#ggraph
ggraph(network)+
  geom_edge_fan0(aes(color = relation)) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = "outward")+
  theme_void()

ggraph(network, layout = 'linear', circular = T)+
  geom_edge_arc(aes(color = relation)) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = "outward")+
  theme_void()

ggraph(network, layout = 'dendrogram', circular = T)+
  geom_edge_elbow()+
  coord_fixed()+
  geom_node_text(aes(label = name), vjust = "outward")+
  theme_void()
