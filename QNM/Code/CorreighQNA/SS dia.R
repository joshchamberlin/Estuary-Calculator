## Functions to read and parse sign directed graphs created in dia
library(XML)

filename=SSSalmon

## Extract basic graph strcuture from a dia xml file.  Assumes the xml
## is uncompressed.
read.dia <- function(SSSalmon) {
  
  ## Read the xml and determine namespace
  dia <- xmlParse("SSSalmon.dia")
  ns <- xmlNamespaceDefinitions(dia,simplify=TRUE)
  
  fix.null <- function(x)
    if(is.null(x) || length(x)==0) NA else x
  
  ## Extract the name of a node in the graph
  node.label <- function(node) {
    label <- xpathSApply(node,"./dia:attribute[@name='text']//dia:string/text()",
                         xmlValue,namespaces=ns)
    label <- paste(gsub("^#(.*)#$","\\1",gsub(",|\n"," ",label)),collapse=" ")
    if(label!="") label else paste("Dia",xmlGetAttr(node,"id"),sep=":")
  }
  
  ## Extract location on the dia page
  node.position <- function(node)
    xpathSApply(node,"./dia:attribute[@name='obj_pos']/dia:point[@val]",
                xmlGetAttr,name="val",namespaces=ns)
  
  ## Extract color of node
  node.colour <- function(node)
    xpathSApply(node,"./dia:attribute[@name='inner_color']/dia:color[@val]",
                xmlGetAttr,name="val",namespaces=ns)
  
  ## Extract an attribute of an edge
  edge.attr <- function(edge,attr)
    fix.null(xpathSApply(edge,paste("./dia:attribute[@name='",attr,"']/dia:enum[@val]",sep=""),
                         xmlGetAttr,name="val",namespaces=ns))
  
  ## Extract an endpoint of an edge
  edge.connection <- function(edge,handle)
    fix.null(xpathSApply(edge,paste("./dia:connections/dia:connection[@handle='",handle,"']",sep=""),
                         xmlGetAttr,name="to",namespaces=ns))
  
  ## Extract node definitions
  nodes <- xpathApply(dia,"//dia:object[@type='Flowchart - Ellipse'] | //dia:object[@type='Flowchart - Box']",
                      function(node) list(id=xmlGetAttr(node,"id"),
                                          label=node.label(node),
                                          colour=node.colour(node),
                                          pos=node.position(node)),
                      namespaces=ns)
  
  ## Extract edge definitions
  edges <- xpathApply(dia,
                      "//dia:object[@type='Standard - Arc'] | //dia:object[@type='Standard - ZigZagLine'] | //dia:object[@type='Standard - Line']",
                      function(edge) list(from=edge.connection(edge,"0"),
                                          to=edge.connection(edge,"1"),
                                          line.style=edge.attr(edge,"line_style"),
                                          start.arrow=edge.attr(edge,"start_arrow"),
                                          end.arrow=edge.attr(edge,"end_arrow")))
  
  if(any(is.na(sapply(edges,"[[","from"))) ||
     any(is.na(sapply(edges,"[[","to"))))
    warning("Dia file contains unconnected edges")
  
  list(nodes=nodes,edges=edges)
}


## Translate the dia representation to the weighted edge list
## required for the simulation.
parse.dia <- function(dia) {
  
  ## Node id and labels
  nodes.id <- sapply(dia$nodes,"[[","id")
  nodes.label <- sapply(dia$nodes,"[[","label")
  
  ## Get endpoints
  from <- nodes.label[match(sapply(dia$edges,"[[",1),nodes.id)]
  to <- nodes.label[match(sapply(dia$edges,"[[",2),nodes.id)]
  species <- sort(unique(c(from,to)))
  from <- factor(from,levels=species)
  to <- factor(to,levels=species)
  
  ## Line style determines the edge grouping
  line <- sapply(dia$edges,"[[",3)
  group <- ifelse(is.na(line),0,as.numeric(line))
  
  ## Edge types are determined by arrow styles
  type <- c("N","P","U","Z")
  code <- c("8","1","5",NA)
  backward.types <- type[match(sapply(dia$edges,"[[",4),code)]
  forward.types <- type[match(sapply(dia$edges,"[[",5),code)]
  if(any(is.na(forward.types)|is.na(backward.types)))
    stop("Dia file contains unknown arrow type")
  
  ## Construct (directed) edge list
  edges <- rbind(data.frame(From=from,
                            To=to,
                            Group=group,
                            Type=factor(forward.types,type),
                            Pair=1:length(dia$edges)),
                 data.frame(From=to,
                            To=from,
                            Group=group,
                            Type=factor(backward.types,type),
                            Pair=1:length(dia$edges)))
  
  ## Drop zero weight edges
  edges <- edges[edges$Type!="Z",,drop=F]
  
  ## Return edge list
  edges
}


## Read a dia file and translate to edge descriptions
model.dia <- function(filename) {
  parse.dia(read.dia(filename))
}

