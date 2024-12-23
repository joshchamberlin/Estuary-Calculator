#############################################################################
# Salish Sea Marine Survival Project                                        #
# Qualitative Network Analysis                                              #
# Using diagraphs created in Dia software, use QNA packages and scripts to  # 
#   evaluate outcomes of press perturbations                                #
#                                                                           #
# Generated on 3/20/2016                                                    #
# K. Sobocinski                                                             #
# kathryn.sobocinski@noaa.gov                                               #
# Long Live the Kings/NOAA NWFSC                                            #
#############################################################################

# Manipulations of QPress code to get different output

# impact.barplot original function

#impact.barplot=function (sim, epsilon = 1e-05, main = "", cex.axis = 1) 
# {
#   edges <- sim$edges
#   As <- sim$A
#   nodes <- node.labels(edges)
#   action <- function(perturb, monitor, edges, check, slider) {
#     impact.barplot.action(nodes, As, perturb, monitor, epsilon = epsilon, 
#                           main = main, cex.axis = cex.axis)
#   }
#   interactive.selection(action, nodes, perturb = T, monitor = T)
# }


# impact.barplot
impact.barplot=function (sim, epsilon = 1e-05, main = "", cex.axis = 1) 
{
  print("hello")
  edges <- sim$edges
  As <- sim$A
  nodes <- node.labels(edges)
  action <- function(perturb, monitor, edges, check, slider) {
    impact.barplot.action(nodes, As, perturb, monitor, epsilon = epsilon, 
                          main =main , cex.axis = cex.axis)
  #print(As)
  print(nodes)
  }
  interactive.selection(action, nodes, perturb = T, monitor = T)
}
#main="Anthropogenic Impacts"

# impact.barplot.action  
impact.barplot.action <- function (nodes, As, perturb, monitor, epsilon = 1e-05, main = "", 
                                   cex.axis = 1) 
{
  #pal <- c("#92C5DE", "#808080", "#F4A582")
  pal <- c("firebrick4", "gray", "lightblue")
  results <- matrix(0, length(nodes), 3)
  for (i in 1:length(As)) {
    impact <- signum(drop(As[[i]] %*% perturb), epsilon = epsilon)
    if (all(monitor == impact, na.rm = T)) {
      results <- results + outer(impact, -1:1, "==")
      print(results)
    }
  }
  rownames(results) <- nodes
  lwidth <- max(strwidth(nodes, units = "inches", cex = cex.axis))
  opar <- par(mai = c(1, lwidth + 0.2, 0.4, 0.4) + 0.2)
  barplot(t(results), horiz = T, las = 1, border = F, col = pal, 
          xlab = "Simulations", main = main, cex.axis = cex.axis)
  par(opar)
  print(results)
}

# interactive.selection
interactive.selection=function (action, nodes, edges = NULL, slider = NULL, checkbox = NULL, 
          perturb = T, monitor = T) 
{
  tk.top <- tktoplevel()
  tktitle(tk.top) <- "Node Selector"
  label <- T
  w.perturb <- if (perturb) 
    radiogrid(tk.top, "Perturb", nodes, c(`-` = -1, `0` = 0, 
                                          `+` = 1), initial = 2, label.rows = label && !(label <- F))
  w.monitor <- if (monitor) 
    radiogrid(tk.top, "Monitor", nodes, c(`-` = -1, `0` = 0, 
                                          `+` = 1, `?` = NA), initial = 4, label.rows = label && 
                !(label <- F))
  w.edges <- if (!is.null(edges)) 
    checkedges(tk.top, "Edges", nodes, edges, label.rows = label && 
                 !(label <- F))
  w.checkbox <- if (!is.null(checkbox)) 
    checkbox(tk.top, checkbox, 0)
  w.slider <- if (!is.null(slider)) 
    slider(tk.top, slider$initial, slider$to, slider$from)
  update <- function() {
    action(perturb = if (!is.null(w.perturb)) 
      w.perturb$selected(), monitor = if (!is.null(w.monitor)) 
        w.monitor$selected(), edges = if (!is.null(w.edges)) 
          w.edges$selected(), check = if (!is.null(w.checkbox)) 
            w.checkbox$selected(), slider = if (!is.null(w.slider)) 
              w.slider$selected())
    Sys.sleep(0.1)
    tkfocus(tk.top)
  }
  close <- function() {
    tkdestroy(tk.top)
  }
  col <- -1
  if (!is.null(w.perturb)) 
    tkgrid(w.perturb$window, padx = 2, pady = 2, row = 0, 
           column = (col <- col + 1), sticky = "n")
  if (!is.null(w.monitor)) 
    tkgrid(w.monitor$window, padx = 2, pady = 2, row = 0, 
           column = (col <- col + 1), sticky = "n")
  if (!is.null(w.edges)) 
    tkgrid(w.edges$window, padx = 2, pady = 2, row = 0, 
           column = (col <- col + 1), sticky = "n")
  tk.frame <- tkframe(tk.top)
  tkgrid(tk2button(tk.frame, text = "Update", command = update), 
         tk2button(tk.frame, text = "Close", command = close))
  tkgrid(tk.frame, if (!is.null(w.checkbox)) 
    w.checkbox$window, if (!is.null(w.slider)) 
      w.slider$window)
  tkfocus(tk.top)
}

# radiogrid
radiogrid=function (parent, label, rows, choices, initial = 1, label.rows = T) 
{
  if (is.null(names(choices))) 
    names(choices) <- as.character(choices)
  initial <- rep(initial, length = length(rows))
  state <- lapply(initial, function(k) tclVar(names(choices)[k]))
  names(state) <- rows
  tk.frame <- tk2labelframe(parent, text = label)
  for (col in seq_along(choices)) tkgrid(tk2label(tk.frame, 
                                                  text = names(choices)[col]), row = 0, column = col)
  for (row in seq_along(rows)) {
    tkgrid(tk2label(tk.frame, text = if (label.rows) 
      rows[row]
      else ""), row = row, column = 0, sticky = "w")
    for (col in seq_along(choices)) tkgrid(tk2radiobutton(tk.frame, 
                                                          value = names(choices)[col], variable = state[[row]]), 
                                           row = row, column = col)
  }
  r <- list(window = tk.frame, selected = function() {
    r <- choices[sapply(state, tclvalue)]
    names(r) <- rows
    r
  }, state = state, choices = choices)
  class(r) <- "radiogrid"
  r
}

#System.simulate
system.simulate=function (n.sims, edges, required.groups = c(0), validators = NULL) 
{
  As <- vector("list", n.sims)
  ws <- matrix(0, n.sims, nrow(edges))
  s <- community.sampler(edges, required.groups)
  total <- 0
  stable <- 0
  accepted <- 0
  while (accepted < n.sims) {
    total <- total + 1
    z <- s$select(rnorm(1,0,2))
    W <- s$community()
    if (!stable.community(W)) 
      next
    stable <- stable + 1
    if (!all(as.logical(lapply(validators, function(v) v(W))))) 
      next
    accepted <- accepted + 1
    As[[accepted]] <- -solve(W)
    ws[accepted, ] <- s$weights(W)
  }
  colnames(ws) <- s$weight.labels
  list(edges = edges, A = As, w = ws, total = total, stable = stable, 
       accepted = accepted)
}

