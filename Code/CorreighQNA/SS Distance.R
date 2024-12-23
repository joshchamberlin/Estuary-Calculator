## This example demonstrates the calculation of distance measures
## (and associated clustering) for node responses to a perturbation
## scenario (suppression of rabbits, rats and mice) for Raymond et al.'s 
## (2011, Journal of Applied Ecology ) Subantarctic Macquarie Island model 
## ("macquarie.dia").

setwd("C:\\Users\\Kathryn.Sobocinski\\Documents\\SSMSP\\QNA")
#install.packages("QPress",,"http://rforge.net/",type="source")
#library(QPress)

source("dia.r")
source("community.r")

## Read model specification
edges <- model.dia("SSv6.dia")
edges <- model.dia("SSSalmonSimple.dia")

## Examine unweighted adjacency matrix
A <- adjacency.matrix(edges)
A

## Function to generate the community matrix
s <- community.sampler(edges)

## Function to check the validation condition
press <- press.validate(edges,
                        perturb=c("Forage Fish"=-1,"Marine Mammals"=1, "Diatoms"=-1),
                        monitor=c("Forage Fish"=-1,"Survival"=-1, "Marine Mammals"=1, "Diatoms"=-1))

## Function to define the perturbation scenario
impact <- press.impact(edges,perturb=c("Forage Fish"=-1,"Marine Mammals"=1, "Diatoms"=-1))

## Use 10000 simulations
n.sims <- 10000
dist <- list(0,0,0)
i <- 0
while(i < n.sims) {


  ## Sample community matrix
  z <- s$select(runif(1))
  W <- s$community()
  
  ## Check stability
  if(!(press(W) && stable.community(W))) next
  
  ## Monitor impact post press
  imp <- sign(impact(W))
  
  ## Compute distances
  dist[[1]] <- dist[[1]] + outer(imp,imp,function(x,y) abs(x-y))
  dist[[2]] <- dist[[2]] + outer(imp,imp,function(x,y) as.numeric(x!=y & x!=0 & y!=0))
  dist[[3]] <- dist[[3]] + outer(imp,imp,function(x,y) x!=y)
  i <- i+1
}

dist.scale <- list(0,0,0)
for(i in 1:3){
  rownames(dist[[i]]) <- levels(edges$From)
  colnames(dist[[i]]) <- levels(edges$From)
  dist.scale[[i]] <- dist[[i]]/max(dist[[i]])
}

## Use clustering to show similarity in model variable responses
fit1 <- hclust(as.dist(dist.scale[[1]]))
fit2 <- hclust(as.dist(dist.scale[[2]]))
fit3 <- hclust(as.dist(dist.scale[[3]]))
plot(fit1,xlab='',ylab='Distance (d1)')
dev.new()
plot(fit2,xlab='',ylab='Distance (d2)')
dev.new()
plot(fit3,xlab='',ylab='Distance (d3)')

