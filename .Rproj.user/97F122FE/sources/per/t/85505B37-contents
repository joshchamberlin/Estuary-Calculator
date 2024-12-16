#Load libraries
library(readr) 
library(tidyverse)
library(here)
library(patchwork)
library(igraph)
library(ggraph)

#Useful Websites
  #https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
    #layouts in ggraph
    #this blog series continues to talk about nodes, edges, and connections
  #https://ggraph.data-imaginist.com/articles/Layouts.html
    #a similar series with layouts, nodes, and edges
    #the reference page may come in handy

#Some basic data manipulation to start
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))
pred_names <- read_csv(here("output/pred_names.csv"))

sc_groupings <- left_join(raw_stomach_contents2021,groupings,by="Prey_Name")

stomach_contents_2021 <- sc_groupings %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_"),
         Len_bin_20 = cut(PRED_LEN, breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200,
                                               220, 240, 260, 280)),
         Len_bin_10 = cut(PRED_LEN, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 
                                               120, 130, 140, 150, 160, 170, 180, 190, 200, 210,
                                               220, 230, 240, 250, 260, 270, 280)),
         Len_bin_AF = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 280)),
         Len_bin_HB = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 
                                               120, 280)),
         Len_bin_CD = cut(PRED_LEN, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 280)),
         Len_bin_PC_broad = cut(PRED_LEN, breaks = c(0, 30, 60, 280)),
         Len_bin_WP_broad = cut(PRED_LEN, breaks = c(0, 30, 40, 50, 280)),
         Len_bin_AF_broad = cut(PRED_LEN, breaks = c(0, 20, 40, 280)),
         Len_bin_PH_broad = cut(PRED_LEN, breaks = c(0, 30, 60, 280)))

stomach_contents_2021 <- left_join(stomach_contents_2021, pred_names, by = 'Pred_Species')

#general ggraph notes
geom_edge_fan() #I think this allows for multiple edges between two nodes and fans them out so that you can see them,
#instead of having a straight line edge
aes(alpha = , colour = , size = ) #these are some attributes you can adjust within the node/edge plot lines
ggtitle() #I think this is an argument for a title?
geom_edge_link(arrow = arrow(length = unit(4, 'mm')),
               start_cap = circle(3, 'mm'),
               end_cap = circle(3, 'mm')) #this creates directional arrows

#Layout: alternative to putting it directly in the plot you could assign it separately
layout <- create_layout(p_WP_2021, layout = "tree")

#----------------------
#Network 1 Example
#Edgelist of all predators and prey in the dataset
#predator, prey

edge_list <- stomach_contents_2021 %>% 
  filter(Pred_common == c("Walleye pollock", "Pacific halibut", "Arrowtooth flounder", "Pacific cod")) %>% 
  select(Pred_common, stock_groupings) %>% 
  distinct(Pred_common, stock_groupings) %>% 
  rename(from = Pred_common, to = stock_groupings)

p <- graph.edgelist(as.matrix(edge_list), directed = T)

gorder(p)
gsize(p) #this is obviously very big and messy

#igraph plot
plot(p)

#ggraph plot default layout was "sugiyama"
ggraph(p) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))




#-----------------------
#Network 2 Example
#this is a simplified network with only one predator so I can focus on how to create edge and node attributes
#manipulating data
#NOTE THAT THE SAMPLE SIZES IN THE NODES DATAFRAME ARE CURRENTLY USELESS
edges.df <- stomach_contents_2021 %>% 
  filter(Pred_common == "Walleye pollock", Prey_Name != "Empty") %>% 
  group_by(stock_groupings) %>% 
  summarise(Pred_common = "Walleye pollock", TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100) %>% 
  select(Pred_common, stock_groupings, PW, n) %>% 
  rename(from = Pred_common, to = stock_groupings)

nodes.df <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock", Prey_Name != "Empty") %>% 
  group_by(stock_groupings) %>% 
  summarise(n = length(unique(uniqueID))) %>% 
  select(stock_groupings, n) %>% 
  rename(name = stock_groupings) %>% 
  add_row(name = "Walleye pollock", n = 1000)


#Converting to igraph object
p_WP_2021 <- graph.edgelist(as.matrix(WP_2021_EL), directed = T)
work <- graph_from_data_frame(d = edges.df, vertices = nodes.df, directed = F)

#igraph
plot(p_WP,
     edge.width = 'PW')

#ggraph
ggraph(work, layout = "tree") +
  geom_edge_link(aes(width = PW, alpha = n)) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = "outward")

#__________________________
#Next I want to add the 4 major predators to this plot
#cod
edges.df.cod <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific cod", Prey_Name != "Empty") %>% 
  group_by(stock_groupings) %>% 
  summarise(Pred_common = "Pacific cod", TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100) %>% 
  select(Pred_common, stock_groupings, PW, n) %>% 
  rename(from = Pred_common, to = stock_groupings)

#halibut
edges.df.hal <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific halibut", Prey_Name != "Empty") %>% 
  group_by(stock_groupings) %>% 
  summarise(Pred_common = "Pacific halibut", TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100) %>% 
  select(Pred_common, stock_groupings, PW, n) %>% 
  rename(from = Pred_common, to = stock_groupings)

#arrowtooth
edges.df.arrow <- stomach_contents_2021 %>% 
  filter(Year = ,Pred_common == "Arrowtooth flounder", Prey_Name != "Empty") %>% 
  group_by(stock_groupings) %>% 
  summarise(Pred_common = "Arrowtooth flounder", TotalWt = sum(PREY_TWT), n = length(unique(uniqueID))) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100) %>% 
  select(Pred_common, stock_groupings, PW, n) %>% 
  rename(from = Pred_common, to = stock_groupings)

nodes.com <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock", Prey_Name != "Empty") %>% 
  group_by(stock_groupings) %>% 
  summarise(n = length(unique(uniqueID))) %>% 
  select(stock_groupings, n) %>% 
  rename(name = stock_groupings) %>% 
  add_row(name = "Walleye pollock", n = 1000) %>% 
  add_row(name = "Pacific cod", n = 10000) %>% 
  add_row(name = "Pacific halibut", n = 10000) %>% 
  add_row(name = "Arrowtooth flounder", n = 10000)

#merging data
edges.com <- rbind(edges.df, edges.df.cod, edges.df.hal, edges.df.arrow)

#converting to igraph object
main4 <- graph_from_data_frame(d = edges.com, vertices = nodes.com, directed = F)

#graph
#ggraph
Pred_Network <- ggraph(main4, layout = "dendrogram") +
  geom_edge_link(aes(width = PW, alpha = n)) +
  geom_node_point() +
  geom_node_label(aes(label = name), color = '#fc8d59')+
  theme_void()

ggsave("PredNet.pdf", plot = Pred_Network, device = "pdf",
       path = here("output/Networks"),
       height = 7, width = 9)

#----------------------
#creating plot for the two flatfish preds

nodes.flat <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock", Prey_Name != "Empty") %>% 
  group_by(stock_groupings) %>% 
  summarise(n = length(unique(uniqueID))) %>% 
  select(stock_groupings, n) %>% 
  rename(name = stock_groupings) %>% 
  add_row(name = "Pacific halibut", n = 10000) %>% 
  add_row(name = "Arrowtooth flounder", n = 10000)

#merging data
edges.flat <- rbind(edges.df.hal, edges.df.arrow)

#converting to igraph object
flat <- graph_from_data_frame(d = edges.flat, vertices = nodes.flat, directed = F)

#graph
#ggraph
flatfish <- ggraph(flat, layout = "tree") +
  geom_edge_link(aes(width = PW, alpha = n)) +
  geom_node_point() +
  geom_node_label(aes(label = name), color = '#fc8d59')+
  theme_void()

ggsave("Flatfish.pdf", plot = flatfish, device = "pdf",
       path = here("output/Networks"),
       height = 6, width = 16)


#________________________________
#Next I want to see if I can have nodes that are broken out by length so that they can be both predator and prey

#load prey length data
raw_prey_length <- read_csv(here("data/GOA_Raw_PreyLength.csv"))

#manipulating data
range(pl$Prey_sz1)
range(pl$Pred_len)

pl <- raw_prey_length %>% 
  mutate(uniqueID = paste(Hauljoin, Pred_nodc, Pred_specn,  sep = "_"),
         preylen_cm = Prey_sz1/10, #converting prey length to cm
         Len_bin_prey = cut(preylen_cm, breaks = c(0, 30, 60, 70)),
         Len_bin_pred = cut(Pred_len, breaks = c(0, 30, 60, 300)))

#join with groupings, DAMN I realized that the prey names are different arg
pl_groupings <- left_join(raw_prey_length, groupings,by="Prey_Name")

#I'm going to try to start with something simple
#I know that walleye pollock are the most common prey item that is also a predator

test <- pl %>% 
  filter(Prey_Name == "Walleye pollock Gadus chalcogrammus", Pred_name == "Gadus chalcogrammus (walleye pollock)") %>% 
  select(Pred_name, Len_bin_pred, Prey_Name, Len_bin_prey, uniqueID, prey_) %>% 
  summarize(from = paste("Walleye pollock", Len_bin_prey, sep = "_"),
            to = paste("Walleye pollock", Len_bin_pred, sep = "_"), uniqueID) 


walleye <- graph.edgelist(as.matrix(test), directed = T)

plot(walleye)

ggraph(walleye) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

#That worked for a simple example now I need to debug/clean the data to get it to work in a more broad context.

#I created a copy of the file so that I can do a bunch of editing within the csv.
prey_length_df <- read_csv(here("data/GOA_PreyLength_NEWNAMES.csv"))

#What are all the predators and which ones show up as prey that I need to match the names for
unique(prey_length_df$Pred_name)
unique(prey_length_df$Prey_Name)

#renaming predators
prey_length_df$Pred_name[prey_length_df$Pred_name == 'Hippoglossus stenolepis (Pacific halibut)'] <- 'Pacific halibut'
prey_length_df$Prey_Name[prey_length_df$Prey_Name == 'Pacific halibut Hippoglossus stenolepis'] <- 'Pacific halibut'

prey_length_df$Pred_name[prey_length_df$Pred_name == 'Atheresthes stomias (arrowtooth flounder)'] <- 'Arrowtooth flounder'
prey_length_df$Prey_Name[prey_length_df$Prey_Name == 'Arrowtooth flounder Atheresthes stomias'] <- 'Arrowtooth flounder'

prey_length_df$Pred_name[prey_length_df$Pred_name == 'Anoplopoma fimbria (sablefish)'] <- 'Sablefish'
prey_length_df$Prey_Name[prey_length_df$Prey_Name == 'Anoplopoma fimbria (sablefish)'] <- 'Sablefish'

prey_length_df$Pred_name[prey_length_df$Pred_name == 'Gadus chalcogrammus (walleye pollock)'] <- 'Walleye pollock'
prey_length_df$Prey_Name[prey_length_df$Prey_Name == 'Walleye pollock Gadus chalcogrammus'] <- 'Walleye pollock'

prey_length_df$Pred_name[prey_length_df$Pred_name == 'Gadus macrocephalus (Pacific cod)'] <- 'Pacific cod'
prey_length_df$Prey_Name[prey_length_df$Prey_Name == 'Gadus macrocephalus (Pacific cod)'] <- 'Pacific cod'

prey_length_df$Pred_name[prey_length_df$Pred_name == 'Sebastes alutus (Pacific ocean perch)'] <- 'Pacific ocean perch'
prey_length_df$Prey_Name[prey_length_df$Prey_Name == 'Pacific ocean perch Sebastes alutus'] <- 'Pacific ocean perch'

prey_length_df$Pred_name[prey_length_df$Pred_name == 'Ophiodon elongatus (lingcod)'] <- 'Lincod'
prey_length_df$Prey_Name[prey_length_df$Prey_Name == 'Lingcod Ophiodon elongatus'] <- 'Lincod'

#Manipulating data
morepreds <- prey_length_df %>%
  mutate(uniqueID = paste(Hauljoin, Pred_nodc, Pred_specn,  sep = "_"),
         preylen_cm = Prey_sz1/10, #converting prey length to cm
         Len_bin_prey = cut(preylen_cm, breaks = c(0, 30, 60, 70)),
         Len_bin_pred = cut(Pred_len, breaks = c(0, 30, 60, 300))) %>% 
  select(Pred_name, Len_bin_pred, Prey_Name, Len_bin_prey, uniqueID) %>% 
  summarize(from = paste(Pred_name, Len_bin_prey, sep = "_"),
            to = paste(Prey_Name, Len_bin_pred, sep = "_")) %>% 
  distinct(from, to)

more_pred_gg <- graph.edgelist(as.matrix(morepreds), directed = T)

plot(more_pred_gg)

#this is a beautiful mess
ggraph(more_pred_gg, layout = "tree") +
  geom_edge_link() +
  geom_node_point()

#------------------------------------
#I'm moving to a new section because I want to be able to reference that previous rat nest figure

#This section is a bit of name cleaning to get things to match properly
groupings_preylen <- raw_prey_length %>% 
  select(Prey_Name) %>% 
  distinct(Prey_Name)

#DONT run line below or it will rewrite my file that I have manually updated
#write.csv(groupings_preylen, file = here("output/groupings_preylen.csv"))

#I need to know which predators occur in the data
pred_names <- raw_stomach_contents2021 %>%
  select(Pred_Species) %>% 
  distinct(Pred_Species)

#write.csv(pred_names, file = here("output/pred_names.csv"), row.names = F)  

groupings_preylen <- read_csv(here("output/groupings_preylen.csv"))


pl_groupings <- left_join(raw_prey_length, groupings_preylen, by="Prey_Name")

pl_groupings <- pl_groupings %>% 
  filter(Pred_Prey_Occur != "NA") %>%
  mutate(uniqueIDPrey = paste(Hauljoin, Pred_nodc, Pred_specn, Prey_nodc,  sep = "_")) %>% 
  select(uniqueIDPrey, Pred_Prey_Occur, Prey_sz1)

sc_groupings <- sc_groupings %>% 
  mutate(uniqueIDPrey = paste(HAULJOIN, PRED_NODC, PRED_SPECN, PREY_NODC,  sep = "_"))


pl_combine <- left_join(sc_groupings, pl_groupings, by = "uniqueIDPrey")

test <- pl_combine %>%
  filter(Pred_Prey_Occur != "NA") %>% 
  select(Pred_common, Pred_Species, PRED_LEN, Prey_Name_Clean, Pred_Prey_Occur, Prey_sz1)

#I talked to Anne and she said to abandon the work using prey lengths for now.
#I'm going to move forward working with species as single nodes not broken up by length distinctions





#---------------------------------------------------
#I still want to match predator to prey nodes. So that means that I need to find which predators occur as prey
  #and make sure the names match. I created a new column in the groupings doc for prey network_grouping

#Creating a network for before the marine heat wave 1990-2013
before.MHW <- stomach_contents_2021 %>% 
  filter(Year >= 1990, Year <= 2013) 

preds <- before.MHW %>% 
  select(Pred_group) %>% 
  distinct(Pred_group)

prey <- before.MHW %>% 
  select(network_grouping) %>% 
  distinct(network_grouping)

#I had to do this because the column names were diferent and not binding
predprey <- preds
colnames(prey) <- colnames(preds)

#creating nodes dataframe
nodes.BMHW <- rbind(preds, prey) %>% 
  distinct(Pred_group)

#creating basic edges dataframe
  #This shows all the edges, but with no edge attributes
basic.edges.BMHW <- before.MHW %>% 
  distinct(Pred_group, network_grouping) %>% 
  select(Pred_group, network_grouping)

#converting to igraph object
BMHW.ig <- graph_from_data_frame(d = basic.edges.BMHW, vertices = nodes.BMHW, directed = T)

#graph
#ggraph
#Dendrogram
BMHW.plot <- ggraph(BMHW.ig, layout = 'dendrogram', circular = T) +
  geom_edge_diagonal() +
  geom_node_point(aes(filter = leaf)) +
  geom_node_text(aes(label = name))+
  theme_void()

gorder(BMHW.ig)

#star
BMHW.plot <- ggraph(BMHW.ig, layout = 'star') +
  geom_edge_link() +
  geom_node_point() +
  theme_void()

#default
BMHW.plot <- ggraph(BMHW.ig) +
  geom_edge_link() +
  geom_node_point() +
  theme_void()

#The network still looks crazy so I'm going to try using the stock groupings

preds.sim <- before.MHW %>% 
  select(Pred_group) %>% 
  distinct(Pred_group)

prey.sim <- before.MHW %>% 
  select(stock_groupings) %>% 
  distinct(stock_groupings)

#I had to do this because the column names were diferent and not binding
predprey.sim <- preds.sim
colnames(prey.sim) <- colnames(preds.sim)

#creating nodes dataframe
nodes.BMHW.sim <- rbind(preds.sim, prey.sim) %>% 
  distinct(Pred_group)

#creating basic edges dataframe
#This shows all the edges, but with no edge attributes
basic.edges.BMHW.sim <- before.MHW %>% 
  distinct(Pred_group, stock_groupings) %>% 
  select(Pred_group, stock_groupings)

#converting to igraph object
BMHW.ig.sim <- graph_from_data_frame(d = basic.edges.BMHW.sim, vertices = nodes.BMHW.sim, directed = T)


#default
ggraph(BMHW.ig.sim) +
  geom_edge_link() +
  geom_node_point() +
  theme_void()




#Just curious. What if I look at a network of predators around one prey item...


krill <- stomach_contents_2021 %>%
  filter(Prey_Name_Clean == "Euphausiacea")
  
krill.edge <- krill %>% 
  select(Pred_Species, Prey_Name_Clean) %>% 
  distinct(Pred_Species, Prey_Name_Clean)

krill.node <- krill.edge %>% 
  select(Pred_Species) %>% 
  add_row(Pred_Species = "Euphausiacea")

krill.ig <- graph_from_data_frame(d = krill.edge, vertices = krill.node, directed = T)

ggraph(krill.ig, layout = "kk") +
  geom_edge_link() +
  geom_node_point() 

plot(krill.ig)
