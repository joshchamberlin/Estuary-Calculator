## Functions to process and analyse community matrices
library(utils)


## Extract node labels
node.labels <- function(edges) {
  levels(edges$From)
}


## Convert edge descriptions to an adjacency matrix.  The
## required.groups argument determines which edge groups will appear
## in the matrix.
adjacency.matrix <- function(edges,labels=F,required.groups=c(0)) {
  
  z <- ifelse(edges$Group %in% required.groups,1,0)
  n <- nlevels(edges$From)
  nms <- levels(edges$From)
  A <- matrix(0L,n,n,dimnames=if(labels) list(nms,nms))
  type <- c("N","P","U","Z")
  weight <- c(-1,1,NA,0)
  A[cbind(edges$To,edges$From)] <- z*weight[match(edges$Type,type)]
  A
}


## Add self loops to enforce self limitation
enforce.limitation <- function(edges) {
  loops <- edges$To[edges$To==edges$From]
  limit <- setdiff(levels(edges$From),edges$From[loops])
  n <- length(limit)
  rbind(edges,
        data.frame(From=factor(limit,levels=levels(edges$From)),
                   To=factor(limit,levels=levels(edges$From)),
                   Group=rep(0,n),
                   Type=factor(rep("N",n),levels(edges$Type)),
                   Pair=max(edges$Pair)+1:n))
}


## Create functions to generate random community matrices given the
## edge list describing the web topology.  This returns a list of two
## functions, "community" draws a random community matrix, and
## "select" determines which optional edges will be retained in the
## web topology.  The user can specify a list of the edge groups that are
## required to be retained in the model.
community.sampler <- function(edges,required.groups=c(0)) {
  
  n.nodes <- nlevels(edges$From)
  n.edges <- nrow(edges)
  W <- matrix(0,n.nodes,n.nodes)
  
  ## Ranges and indices of non-zero matrix entries
  lower <- ifelse(edges$Type=="U" | edges$Type=="N",-1L,0L)
  upper <- ifelse(edges$Type=="U" | edges$Type=="P",1L,0L)
  k.edges <- as.vector(unclass(edges$To)+(unclass(edges$From)-1)*n.nodes)
  
  ## The indices of the matrix entries that can be omitted (zeroed), the
  ## expansion index that relates matching edges of a pair, and the
  ## number of edges that can be omitted.
  required <- edges$Group %in% required.groups
  k.optional <- k.edges[!required]
  optional <- factor(edges$Pair[!required])
  expand <- as.vector(unclass(optional))
  n.omit <- max(0,expand)
  
  zs <- rep(1,n.omit)
  
  if(n.omit > 0) {
    community <- function() {
      W[k.edges] <- runif(n.edges,lower,upper)
      W[k.optional] <- W[k.optional]*zs[expand]
      W
    }
    select <- function(p) {
      zs <<- rbinom(n.omit,1,p)
      zs
    }
  } else  {
    community <- function(p) {
      W[k.edges] <- runif(n.edges,lower,upper)
      W
    }
    select <- function(p) {
      zs
    }
  }
  list(community=community,select=select,optional.pairs=levels(optional))
}


## Check the stability of a simulated community matrix W
stable.community <- function(W) {
  all(Re(eigen(W,symmetric=FALSE,only.values=T)$values)<0)
}

## Return sign of s, with values of magnitude less than epsilon
## rounded down to zero
signum <- function(s,epsilon=1.0E-5) {
  (s > epsilon) - (s < -epsilon)
}


## Mutual information for discrete x, y
mutual.info <- function(x,y) {
  tab <- table(factor(x),factor(y))
  p <- tab/sum(tab)
  sum(ifelse(tab==0,0,p*log2(p/(rowSums(p)%o%colSums(p)))))
}


## Generate a function to check a press condition.  User must supply a
## vector of named elements that specify the relative magnitude of the
## press perturbation, and a vector of named elements that specify the
## signs of the change in the monitored nodes.
press.validate <- function(edges,perturb,monitor,epsilon=1.0E-5) {
  
  index <- function(name) {
    k <- match(name,levels(edges$From))
    if(any(is.na(k)))
      warning("Unknown nodes:",paste(name[is.na(k)],collapse=" "))
    k
  }
  
  ## Indices of perturb
  k.perturb <- index(names(perturb))
  k.monitor <- index(names(monitor))
  S.press <- double(nlevels(edges$From))
  S.press[k.perturb] <- -perturb
  monitor <- sign(monitor)
  
  ## Return function to check condition
  function(W) {
    s <- tryCatch(solve(W,S.press),error=function(e) NULL)
    !is.null(s) && all(signum(s[k.monitor],epsilon)==monitor)
  }
}


## Generate a function to determine the impact of a press perturbation
press.impact <- function(edges,perturb,monitor=NULL) {
  
  index <- function(name) {
    k <- match(name,levels(edges$From))
    if(any(is.na(k)))
      warning("Unknown nodes:",paste(name[is.na(k)],collapse=" "))
    k
  }
  
  ## Indices of perturb
  k.perturb <- index(names(perturb))
  S.press <- double(nlevels(edges$From))
  S.press[k.perturb] <- -perturb
  
  if(length(monitor)==0) {
    impact <- function(W) solve(W,S.press)
  } else {
    k.monitor <- index(names(monitor))
    impact <- function(W) solve(W,S.press)[k.monitor]
  }
  
  ## Return function to compute impact
  impact
}




