#############################################################################
# Salish Sea Marine Survival Project                                        #
# Qualitative Network Analysis                                              #
# Using diagraphs created in Dia software, use QNA packages and scripts to  # 
#   evaluate outcomes of press perturbations for Salish Sea QN model        #
#                                                                           #
# Generated on 3/20/2016                                                    #
# K. Sobocinski                                                             #
# kathryn.sobocinski@noaa.gov                                               #
# Long Live the Kings/NOAA NWFSC                                            #
#############################################################################
if(nchar(system.file(package="QPress"))) citation("QPress")


install.packages(c("tcltk2","XML"))
install.packages("QPress",repos="http://www.rforge.net/",type="source")

vignette()
#Examples in this vignette for how widget works and interpretation
vignette("Mesocosm")

setwd("C:\\Users\\Kathryn.Sobocinski\\Documents\\SSMSP\\QNA")
setwd("/Users/ksobo/Dropbox/QNA")

#Load XML first--dependency not inlcuded in QPress
library(XML)
library(tcltk2)
library(QPress)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
#library(igraph)
#library(network)
#library(intergraph)
??QPress



# Salmon and Oceanography merged
# Includes some drivers to perturb as press perturbations in simulation models
#ss=model.dia("SS.dia") #~36,000 model runs gets 1000 stable
# 2nd Version
#ss=model.dia("SSv2.dia") #~9,500 model runs gets 1000 stable

# 4th Version (This is the working model as of 4/29/2016)
#ss=model.dia("SSv4.dia") #~ 95,000 model runs gets 1000 stable

# 4.5th version--removed bidirectional arrow between size/fitness.
# This is the working model as of 5/5/2016
#ss=model.dia("SSv4.5.dia") #~ 52,000 model runs gets 1000 stable

# 5th version includes new box for disease, removal of mixing box, replpaced bidirectional arrow between size/fitness.  
#ss=model.dia("SSv5.dia") #~ 120,000 model runs gets 1000 stable

# 6th version (adds links from contaminants to forage fish, marine mammals and other salmon, and link from 
# hatcheries to disease)

#7th version (this is the version used for all analyses submitted in Env. Cons. manuscript)
#ss=model.dia("SSv7.dia") #~ 122,000 model runs gets 10000 stable

#v7.5
#Adds in link from microplankton to zooplankton (C. Krembs, feedback August 2017)
ss=model.dia("SSv7.5 MicroZoo.dia")

#8th version: 
#Updates link from CO2 to diatoms as neutral, since CO2 is not limiting (E. Lessard, feedback, but it does have a
#positive effect, so opting to leave the linkage as positive and use model 7.5)
#ss=model.dia("SSv7_CO2.dia") #~ 120,000 model runs gets 10000 stable
#ss=model.dia("SSv8.dia") #~ 123,000 model runs gets 10000 stable


# Salish Sea System Simulation (use 1000 accepted runs for exploratory work, but increase to 
# 10,000 for output based on model size (33 nodes))
simSS <- system.simulate(10000, ss)
#simSS5 <- system.simulate(1000, ss5)

#To get the total number of runs to produce x accepted runs
simSS$total

names(simSS)


#To explore code
View(impact.barplot)
View(model.dia)
View(QPress:::parse.dia)
View(QPress:::impact.barplot.action)
View(QPress:::model.dia)
View(QPress:::system.simulate)
View(QPress:::radiogrid)
View(QPress:::interactive.selection)
View(QPress:::adjacency.matrix)
?impact.barplot
?system.simulate
?model.dia

#########################################################################################################
# Interactive exploratory widget

impact.barplot(simSS)


##########################################################################################################
##          Sensitivity Analysis
##########################################################################################################


#To extract the weight values in the accepted model runs:
simSS$edges
head(simSS$w)
tail(simSS$w)
mean(abs(simSS$w))#0.503, no update with model 7.5

is.matrix(simSS$w)
Weight=as.data.frame(simSS$w)
head(Weight)
#Check distributions of nodes to see how variable they are
hist(Weight[,66]) #31, 37, 40, 46, 47, 61, 63
#I checked about half and there is not much variability, but some have more than others


#To assess the sensitivity of the weights
#Extract edges and weights from simulations
Wts=melt(Weight)
colnames(Wts)=c("Edge", "Value")
head(Wts)


dim(simSS$w) #10000 x 150, updated with 1 additional linkage for v7.5
#Get means for each edge
Edgemean=as.data.frame(apply(simSS$w, 2, mean))
summary(Edgemean)
Edgemin=as.data.frame(apply(simSS$w, 2, min))
Edgemax=as.data.frame(apply(simSS$w, 2, max))
Edgestdev=as.data.frame(apply(simSS$w, 2, sd))
hist(abs(Edgemean[,1]))
hist(abs(Edgemin[,1]))
hist(abs(Edgemax[,1]))
hist(abs(Edgestdev[,1]))
lowerSD=abs(Edgemean[,1])-abs(Edgestdev[,1])
upperSD=abs(Edgemean[,1])+abs(Edgestdev[,1])

Edges=as.data.frame(levels(Wts$Edge)) 
#Edges2=cbind(Edges)
Edges.vals=cbind(Edges, abs(Edgemean[,1]), abs(Edgemin[,1]), 
                          abs(Edgemax[,1]))

#USe max and min values with mean
Edges.vals
head(Edges.vals)
dim(Edges.vals) #149 x 4
colnames(Edges.vals)=c("Edge", "Mean", "Min", "Max")
head(Edges.vals)
str(Edges.vals)

#Use SD
EdgesSD=cbind(Edges, abs(Edgemean[,1]), lowerSD, upperSD)
head(EdgesSD)
colnames(EdgesSD)=c("Edge", "Mean", "LowerSD", "UpperSD")

#Plot all edges and means, maxes, and mins
ggplot(Edges.vals, aes(x=Mean, y=Edge)) +
  geom_errorbarh(data=Edges.vals, aes(xmax=Max, xmin=Min), colour = "grey50") + 
  geom_point()
  
#Reorder so easier to see
ggplot(EdgesSD, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=EdgesSD, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  geom_point()

#Pull out outliers (top/bottom 15 values)
#Reorder EdgesSD by mean
ro=EdgesSD[order(-EdgesSD$Mean),]
top=ro[1:15,]
bottom=ro[133:148,]

outliers=rbind(top, bottom)

#To plot all outliers
ggplot(outliers, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=outliers, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Mean Weight") +
  theme_bw() +
  geom_point() +
  geom_vline(xintercept=0.5, linetype="dotted")



#To plot top (minus self-reg. loops) and bottom:
ggplot(outliers[8:30,], aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=outliers[8:30,], aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Mean Weight") +
  theme_bw() +
  geom_point()

#To plot bottom (most sensitive edges only):
ggplot(bottom, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=bottom, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Mean Weight") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  geom_point()



#Junk

colour = "grey50"
Weights=as.data.frame(Edges)
head(Weights)
dim(Weights)

apply(simSS$w, 2, stats::quantile)

summarise_each(simSS$w, funs(mean))

par(mfrow=c(1,1))


##################################################################################################
##################################################################################################

# For generating plots that are not part of the QPress package, use code below
# Some example code was provided by Ben Raymond

# Extract the bits we need
edges <- simSS$edges
As <- simSS$A
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
Indiv_Perturb="C:\\Users\\Kathryn.Sobocinski\\Documents\\SSMSP\\QNA\\Indiv_Perturb_Salmon_Traits_v7.5.pdf"
pdf(file=Indiv_Perturb)
# For function
opar <- par
par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:33) {
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses
  press=c(-1,1,1,-1,1,-1,-1,-1,1,1,1,1,1,-1,1,1,1,1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,-1,1,-1)
 
  #length(press)
  presses=diag(press, nrow=33, ncol=33)
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
press=c(0,1,1,-1,-1,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,-1)
presses=diag(press, nrow=33, ncol=33)
perturb=press
Influential=myplot(nodes,As,perturb,monitor,main="Influential")
#Neutral
press=c(0,0,0,0,0,0,0,1,0,1,0,0,0,-1,1,1,0,1,0,0,0,1,0,0,0,1,1,0,0,0,-1,0,0)

presses=diag(press, nrow=33, ncol=33)
perturb=press
Neutral=myplot(nodes,As,perturb,monitor,main="Neutral")

#This is not working
#Node.influence<-grid.arrange(Influential, Neutral + theme(axis.title.y=element_blank()), ncol=1)
#Influential
Neutral


tiff('Node.Influence.7.5.10 000.tiff', units="in", width=7, height=10.5, res=300)
par(mfrow=c(2,1))
#Influential
press=c(0,1,1,-1,-1,0,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,-1)
presses=diag(press, nrow=33, ncol=33)
perturb=press
Influential=myplot(nodes,As,perturb,monitor,main="Influential")
#Neutral
press=c(0,0,0,0,0,0,0,1,0,1,0,0,0,-1,1,1,0,1,0,0,0,1,0,0,0,1,1,0,0,0,-1,0,0)
presses=diag(press, nrow=33, ncol=33)
perturb=press
Neutral=myplot(nodes,As,perturb,monitor,main="Neutral")
dev.off()

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

