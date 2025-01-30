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
library(igraph)
library(network)
library(intergraph)

#Run Model and Generate Simulations
estuary<-model.dia("V1Directed.dia")
simestuary <- system.simulate(10000, estuary)    #note: the model won't run unless all the nodes have a feedback loop


#Raw look at edge weights for accepted models
simestuary$edges
head(simestuary$w)
tail(simestuary$w)

#Average edge weight across the whole model
mean(abs(simestuary$w)) #The average edge weight is 0.501 which is very similar to K&C paper

#Distribution of edge weights
weight <- as.data.frame(simestuary$w)
hist(weight[,16]) #this can be used to check the distributions of the edge weights

#Transform data longer
wts <- melt(weight) #this is basically the same as pivot longer
colnames(wts) = c("Edge", "Value")
head(wts)

#Group by edge and calculate mean, min, max, sd of weights
w <- wts %>% 
  group_by(Edge) %>% 
  dplyr::summarize(mean= mean(abs(Value)), min = min(abs(Value)), max = max(abs(Value)), sd = sd(abs(Value))) %>% 
  mutate(uppersd = mean+sd, lowersd = mean-sd)

#Histograms of stats by edge group
hist(w$mean)
hist(w$min)
hist(w$max)
hist(w$sd)

#Plot of all the edges
ggplot(w, aes(x=mean, y = reorder(Edge, mean)))+
  geom_errorbar(aes(xmax=uppersd, xmin = lowersd))+
  ylab("Edge")+
  xlab("Mean Weight")+
  theme_bw()+
  geom_point()+
  geom_vline(xintercept = 0.5, linetype = "dotted")

#Plot of sensitive edges
w %>% 
  filter(mean > 0.52 | mean < 0.48) %>% 
ggplot(aes(x=mean, y = reorder(Edge, mean)))+
  geom_errorbar(aes(xmax=uppersd, xmin = lowersd))+
  ylab("Edge")+
  xlab("Mean Weight")+
  theme_bw()+
  geom_point()+
  geom_vline(xintercept = 0.5, linetype = "dotted")+
  ggtitle("Edge Weight Sensitivity")
