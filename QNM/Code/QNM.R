#-------------------------------------
#Project Set up
#-------------------------------------


#Generally I prefer to use projects and the here package rather than setting wd, but for some
#reason I couldn't get the dea functions to run with here
setwd("C:\\Users\\Catalina.Burch\\Documents\\GitHub\\Estuary-Calculator\\Code\\CatQNA")
setwd("C:\\Users\\Catalina.Burch\\Documents\\GitHub\\Estuary-Calculator\\Code\\CorreighQNA")

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

# In order to run the model (below) you need to first open the dia.R file to generate
## the functions in your environment. Or maybe you dont if you have the package loaded...



#Run model and generate simulations
#example
ss <- model.dia("SSv7.dia")
simtest <- system.simulate(10000, ss)
#Estuary Model
estuary<-model.dia("test.dia")
simestuary <- system.simulate(10000, estuary)

simtest$total #119126
simestuary$total #20056 total number of runs to produce x accepted runs

names(simestuary) #edges, A, w, total, stable, accepted

#Interactive exploratory widget
impact.barplot(simestuary)
length(unique(estuary$From))
length(unique(ss$From))
#---------------------------------------------
#Sensitivity

simestuary$edges
head(simestuary$w) #weight values in accepted model runs
tail(simestuary$w)
mean(abs(simestuary$w)) #0.4999 compared to example below
mean(abs(simtest$w)) #0.503

#--------------------------------------------
#Figures
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
Indiv_Perturb="C:\\Users\\Catalina.Burch\\Documents\\GitHub\\Estuary-Calculator\\Code\\CatQNA.pdf"
pdf(file=Indiv_Perturb)
# For function NOTE!! I had to change this function to match the number of from nodes
opar <- par
par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:35) {
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses
  press=c(-1,1,1,-1,1,-1,-1,-1,1,1,1,1,1,-1,1,1,1,1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,-1,1,-1, 1, -1)
  
  #length(press)
  presses=diag(press, nrow=35, ncol=35)
  perturb=presses[i,]
  
  perturb2=ifelse(perturb==1,"(Increase)","(Decrease)")
  
  #If all press perturbs are positive, use this code:
  #perturb <- c(rep(0,length(nodes)))
  #perturb[i] <- 1 ## perturb the ith node, this is a positive perturbation
  
  #Choose: all nodes (impact.barplot.action) or a subset of nodes (myplot)--myplot code below
  #impact.barplot.action(nodes,As,perturb,monitor,main=c(nodes[i],perturb[i]))
  myplot(nodes,As,perturb,monitor,main=c(nodes[i],perturb2[i]))
}
par(opar)
dev.off()




#####################################################################################
##  For making plots for influential and not influential vars.
##  Fig 4. in paper
######################################################################################
par(mfrow=c(2,1))
#Influential
press=c(0,1,1,-1,-1,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,-1)
presses=diag(press, nrow=35, ncol=35)
perturb=press
Influential=myplot(nodes,As,perturb,monitor,main="Influential")
#Neutral
press=c(0,0,0,0,0,0,0,1,0,1,0,0,0,-1,1,1,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,-1,0,0)

presses=diag(press, nrow=35, ncol=35)
perturb=press
Neutral=myplot(nodes,As,perturb,monitor,main="Neutral")

#This is not working
#Node.influence<-grid.arrange(Influential, Neutral + theme(axis.title.y=element_blank()), ncol=1)
#Influential
Neutral


tiff('Node.Influence.7.5.10 000.tiff', units="in", width=7, height=10.5, res=300)
par(mfrow=c(2,1))
#Influential
press=c(0,1,1,-1,-1,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,-1)
presses=diag(press, nrow=35, ncol=35)
perturb=press
Influential=myplot(nodes,As,perturb,monitor,main="Influential")
#Neutral
press=c(0,0,0,0,0,0,0,1,0,1,0,0,0,-1,1,1,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,-1,0,0)
presses=diag(press, nrow=35, ncol=35)
perturb=press
Neutral=myplot(nodes,As,perturb,monitor,main="Neutral")
dev.off()

#CAT: THIS IS NOT WORKING
grid.arrange(Influential, Neutral  + theme(axis.title.y=element_blank()), ncol=1)
par(mfrow=c(2,1))


QPress:::
  toshow=rep(TRUE,length(nodes))

######################################################################################
##  For paper                                                                       ##
######################################################################################
myplot <- function(nodes,As,perturb,monitor,epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("midnightblue", "#808080", "lightblue")
  results <- matrix(0,length(nodes),3)
  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
    }
  }
  rownames(results) <- nodes
  nodes <- nodes[c(19, 7, 23, 25, 1, 28)]
  results <- results[c(19, 7, 23, 25, 1, 28),]
  lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
  opar <- par(mai=c(1,lwidth+0.2,0.2,0.2)+0.2)
  barplot(t(results),horiz=T,las=1,border=F,col=pal,
          xlab="Simulations", xaxt= "n", main=main,cex.axis=cex.axis)
  axis(1, at=c(0, 2000, 4000, 6000, 8000, 10000), labels=c("0", "2000", "4000", "6000", "8000", "10 000"))
  par(opar)
}


# To show the output of survival only (or any other single driver) for all nodes 
edges <- simSS$edges
As <- simSS$A
nodes <- node.labels(edges)
monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses

#Specify epsilon
epsilon=1.0E-5
#perturb <- c(rep(0,length(nodes)))
#perturb[i] <- 1 ## perturb the ith node, this is a positive perturbation

#Set up matrix for all survival results
survsum <- matrix(0,length(nodes),3)
rownames(survsum) <- nodes
#Set up desired directions of perturbations--based upon direction of press (-1,1)
press=c(-1,1,1,-1,1,-1,-1,-1,1,1,1,1,1,-1,1,1,1,1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,-1,1,-1)
presses=diag(press, nrow=33, ncol=33)

#Run loop to collect results
for (i in 1:33) {
  perturb=presses[i,]
  results <- matrix(0,length(nodes),3)
  for(k in 1:length(As)) {
    impact <- signum(drop(As[[k]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
      rownames(results) <- nodes
      survresults=results[28,] #This specifies the node of interest, e.g. Survival
    }
  }
  survsum[i,]=survresults
}
survsum 

opar <- par
pal <- c("firebrick1", "gray", "lightblue")
cex.axis = 1
lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
opar <- par(mai=c(1,lwidth+0.2,0.4,0.4)+0.2)
barplot(t(survsum),horiz=T,las=1,border=F,col=pal,
        xlab="Simulations",main="Salmon Survival",cex.axis=cex.axis)
par(opar)


################################################################################################
## Driver Group Analysis Output (Figure 3 in paper--data from excel file Driver Group Output)
## First you must gather output from interactive widget: input all perturbations and gather output from 
#printed results (collect in Excel with copy/paste)
################################################################################################

drivers=read.csv('Driver Group Model v7.5.csv', header=TRUE)
resp2=factor(drivers$Response, c("Other Salmon", "Residency", "Size", "Fitness", "Abundance", "Survival"))
NegPos=factor(drivers$PosNeg, c("Negative", "Positive"))

driver.group=ggplot(arrange(drivers, PosNeg), aes(x = resp2 , y = Value, fill = PosNeg)) +
  geom_bar(position=position_stack(reverse=TRUE), stat="identity") +
  coord_flip() +
  scale_fill_manual(values=c("midnightblue", "slategray2")) +
  facet_wrap(~Driver.Group) +
  theme(legend.position="none") + ylab("Model Output") + xlab("Response")

#Change to 10 000
#Generate function for space as thousands delineator
space <- function(x, ...) { 
  format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}

#This creates custom labels
driver.group2=ggplot(arrange(drivers, PosNeg), aes(x = resp2 , y = Value, fill = PosNeg)) +
  geom_bar(position=position_stack(reverse=TRUE), stat="identity") +
  coord_flip() +
  scale_fill_manual(values=c("midnightblue", "slategray2")) +
  facet_wrap(~Driver.Group) +
  scale_y_continuous(breaks=c(0, 2000, 4000, 6000, 8000, 10000), 
                     labels=c("0", "2000", "4000", "6000", "8000", "10 000")) +
  theme_minimal()+ theme(legend.position="none")+ ylab("Model Output") + xlab("Response")



tiff("driver.group.10 000.tiff", width = 7, height = 4, units = 'in', res = 500)
driver.group2 # Make plot
dev.off()

#geom_bar(position="stack", stat="identity") 
#################################################################################################
## Code from Ben Raymond, QPress designer ##
#########
library(QPress)
## example data
modelA <- parse.text(c("V-*V", "P-*P", "V*->H", "H*->P")) ## from the snowshoe vignette
sim <- system.simulate(1000, modelA)
## extract the bits we need
edges <- sim$edges
As <- sim$A
nodes <- node.labels(edges)
monitor <- c(NA,NA,NA)) ## don't enforce any required responses

#Standard plot of all response nodes, perturbing each in turn:

opar <- par
par(mfrow=c(2,2))
for (i in 1:3) {
  perturb <- c(0,0,0)
  perturb[i] <- 1 ## perturb the ith node
  QPress:::impact.barplot.action(nodes,As,perturb,monitor,main=nodes[i])
}
par(opar)

#Or if we want to show only a subset of node responses:

## adjust impact.barplot.action to only show the subset of nodes indicated by toshow
myplot <- function(nodes,As,perturb,monitor,toshow=rep(TRUE,length(nodes)),epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("#92C5DE", "#808080", "#F4A582")
  results <- matrix(0,length(nodes),3)
  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
    }
  }
  rownames(results) <- nodes
  nodes <- nodes[toshow]
  results <- results[toshow,]
  lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
  opar <- par(mai=c(1,lwidth+0.2,0.4,0.4)+0.2)
  barplot(t(results),horiz=T,las=1,border=F,col=pal,
          xlab="Simulations",main=main,cex.axis=cex.axis)
  par(opar)
}

## same as above, but showing only the 1st and 3rd node responses
opar <- par
par(mfrow=c(2,2))
for (i in 1:3) {
  perturb <- c(0,0,0)
  perturb[i] <- 1 ## perturb the ith node
  myplot(nodes,As,perturb,monitor,toshow=c(TRUE,FALSE,TRUE),main="title text")
}
par(opar)

######################################################################################################
#####   JUNK    #####
######################################################################################################

#Trial runs

#Some trial dia models for the salmon part of the system
#salmon1=model.dia("SSSalmon.dia")
#salmon2=model.dia("SSSalmon2.dia")
#salsim=model.dia("SSSalmonSimple2.dia")


#Oceanography component
#ocean=model.dia("SSOcean.dia")
simsalmon <- system.simulate(1000, salmon2)
impact.barplot(simsalmon)
simocean.1 <- system.simulate(1000, ocean)
impact.barplot(simocean.1)

meso1=model.dia("mesocosm1.dia")
meso2=model.dia("mesocosm2.dia")

mac=model.dia("macquarie.dia")

simmeso1= system.simulate(1000, meso1)
impact.barplot(simmeso1)

simmac=system.simulate(1000, mac)
impact.barplot(simmac)

