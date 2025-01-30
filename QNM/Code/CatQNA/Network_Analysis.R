#-------------------------------------
#Project Set up
#-------------------------------------


#Generally I prefer to use projects and the here package rather than setting wd, but for some
#reason I couldn't get the dea functions to run with here
setwd("C:\\Users\\Catalina.Burch\\Documents\\GitHub\\Estuary-Calculator\\Code\\CatQNA")

#--------------------------------------
## QNM Analysis
#--------------------------------------

#This code is for installing QPress off of GitHub
#options(repos = c(SCAR = "https://scar.r-universe.dev",
#                  CRAN = "https://cloud.r-project.org"))

#install.packages("QPress")

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


simestuary$total #19686 runs to produce 10000 accepted runs


#Interactive Widget to explore node perturbations
impact.barplot(simestuary)

#Sensitivity Analysis????

# Extract the bits we need
edges <- simestuary$edges
As <- simestuary$A
nodes <- node.labels(edges)
monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses

#Call specific nodes of interest
#To show only a subset of node responses (e.g. Survival, Abundance, and Other Salmon), run this instead of standard plot:
myplot <- function(nodes,As,perturb,monitor,epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("firebrick4", "#808080", "lightblue")
  results <- matrix(0,length(nodes),3)
  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
    }
  }
  rownames(results) <- nodes
  nodes <- nodes[c(19, 23,7, 25, 1, 28)]
  results <- results[c(19, 23,7, 25,1, 28),]
  lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
  opar <- par(mai=c(0.5,lwidth+0.15,0.15,0.15)+0.2)
  barplot(t(results),horiz=T,las=1,border=F,col=pal,
          xlab="Simulations",main=main,cex.axis=cex.axis)
  par(opar)
}

# Standard slider plot of all response nodes, perturbing each in turn given the vector of perturbations (press):
# windows()
# To output to PDF

Indiv_Perturb="C:\\Users\\Catalina.Burch\\Documents\\GitHub\\Estuary-Calculator\\Code\\CatQNA\\CatQNA.pdf"
pdf(file=Indiv_Perturb)
# For function
opar <- par
par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:34) {
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses
  press=c(-1,1,1,-1,1,-1,-1,-1,1,1,1,1,1,-1,1,1,1,1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,-1,1,-1, 1) #33
  
  #length(press)
  presses=diag(press, nrow=34, ncol=34)
  perturb=presses[i,]
  
  perturb2=ifelse(perturb==1,"(Increase)","(Decrease)")
  
  #If all press perturbs are positive, use this code:
  #perturb <- c(rep(0,length(nodes)))
  #perturb[i] <- 1 ## perturb the ith node, this is a positive perturbation
  
  #Choose: all nodes (impact.barplot.action) or a subset of nodes (myplot)--myplot code below
  impact.barplot.action(nodes,As,perturb,monitor,main=c(nodes[i],perturb[i]))
  #myplot(nodes,As,perturb,monitor,main=c(nodes[i],perturb2[i]))
}
par(opar)
dev.off()
