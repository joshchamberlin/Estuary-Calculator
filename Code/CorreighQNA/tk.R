## Functions to generate 'node selector' interface
library(tcltk)
library(tcltk2)
library(RColorBrewer)


node.selector <- function(nodes,W) {
  
  perturb.choices <- c("+","0","-")
  monitor.choices <- c("+","0","-","?")
  perturb.cache <- 7
  press.cache <- 0
  pal <- brewer.pal(n=5,"RdBu")[2:4]
  
  update <- function() {
    perturb <- match(sapply(perturb.vars,tclvalue),perturb.choices)-2
    perturb <- -perturb
    observe <- match(sapply(monitor.vars,tclvalue),monitor.choices)
    monitor <- which(observe<4)
    observe <- observe[monitor]-2
    if(any(perturb!=perturb.cache)) {
      perturb.cache <<- perturb
      press.cache <<- sapply(Ws,function(W) signum(solve(W,perturb)))
    }
    press <- press.cache
    if(length(monitor)>0)
      press <- press[,apply(press[monitor,,drop=F]==observe,2,all)]
    
    tab <- apply(press+2,1,tabulate,3)
    rownames(tab) <- c("+","0","-")
    colnames(tab) <- nodes
    tab <- tab/length(W)
    opar <- par(mar=c(6,10,1,1)+0.1)
    barplot(tab[,ncol(tab):1],horiz=T,cex.names=1.5,cex.axis=1.5,las=1,border=F,col=pal,xlim=c(0.0,1.0),
            xlab="")
    mtext(side=1,text=paste("\n Proportion \n (marginal likelihood = ",round(max(tab),2),")",sep=""),line=4,cex=1.5)
    par(opar)
    tkfocus(tk.win)
  }
  
  close <- function() {
    tkdestroy(tk.win)
  }
  
  radiogrid <- function(parent,label,rows,choices,vars) {
    tk.frame <- tk2labelframe(tk.win,text=label)
    for(col in 1:length(choices)) {
      tk.label <- tk2label(tk.frame,text=choices[col])
      tkgrid(tk.label,row=0,column=col)
    }
    for(row in 1:length(rows)) {
      tk.label <- tk2label(tk.frame,text=rows[row])
      tkgrid(tk.label,row=row,column=0,sticky="w")
      for(col in 1:length(choices)) {
        tk.rb <- tk2radiobutton(tk.frame,value=choices[col],variable=vars[[row]])
        tkgrid(tk.rb,row=row,column=col)
      }
    }
    tk.frame
  }
  
  
  tk.win <- tktoplevel()
  tktitle(tk.win) <- "Node Selector"
  perturb.vars <- lapply(nodes,function(x) tclVar("0"))
  tkgrid(radiogrid(tk.win,"Perturbed",nodes,perturb.choices,perturb.vars),row=0,column=0,padx=2,pady=2)
  monitor.vars <- lapply(nodes,function(x) tclVar("?"))
  tkgrid(radiogrid(tk.win,"Observed",nodes,monitor.choices,monitor.vars),row=0,column=1,padx=2,pady=2)
  tkgrid(tk2button(tk.win,text="Update",command=update),row=1,column=0)
  tkgrid(tk2button(tk.win,text="Close",command=close),row=1,column=1)
  tkfocus(tk.win)
}

