####################################################################################################
#
# Modified panel.densityplot to allow shading of PDFs
#
#
####################################################################################################

panel.densityplot.poly <-
  function(x,
           darg = list(n = 512),
           plot.points = "jitter",
           ref = FALSE,
           groups = NULL,
           weights = NULL,
           ##              col = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
           ##              lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
           ##              lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
           ##              alpha = if (is.null(groups)) plot.line$alpha else superpose.line$alpha,
           ##              col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
           jitter.amount = 0.01 * diff(current.panel.limits()$ylim),
           type = "p",
           ...,
           identifier = "density") {
    # print(list(...))
    
    if (ref) {
      reference.line <- trellis.par.get("reference.line")
      panel.abline(h = 0,
                   col = reference.line$col,
                   lty = reference.line$lty,
                   lwd = reference.line$lwd,
                   identifier = paste(identifier, "abline"))
    }
    
    ## plot.line <- trellis.par.get("plot.line")
    ## superpose.line <- trellis.par.get("superpose.line")
    if (!is.null(groups)) {
      panel.superpose(x, darg = darg,
                      plot.points = plot.points, ref = FALSE,
                      groups = groups,
                      weights = weights,
                      panel.groups = panel.densityplot.poly,
                      jitter.amount = jitter.amount,
                      type = type,
                      ...)
    } else {
      
      density.fun <- function(x, weights, subscripts = TRUE, darg, ...) {
        ## wrapper to handle 'subscripts' without actually making
        ## it a formal argument to panel.densityplot
        do.call("density", c(list(x = x, weights = weights[subscripts]), darg ))
      }
      
      if (sum(!is.na(x)) > 1) {
        h   <- density.fun(x = x, weights = weights, ..., darg = darg)
        lim <- current.panel.limits()$xlim
        id  <- h$x > min(lim) & h$x < max(lim)
        panel.polygon(x=c(h$x[id][1],h$x[id],h$x[id][length(h$x[id])]), y=c(0,h$y[id],0), 
                      col=list(...)$col.line, alpha=list(...)$alpha.fill,
                      )
        panel.lines(x = h$x[id], y = h$y[id], ...,
                    identifier = identifier)
      }
      
      switch(as.character(plot.points),
             "TRUE" =
               panel.xyplot(x = x, y = rep(0, length(x)), type = type, ...,
                            identifier = identifier),
             "rug" =
               panel.rug(x = x, 
                         start = 0, end = 0,
                         x.units = c("npc", "native"),
                         type = type,
                         ...,
                         identifier = paste(identifier, "rug")),
             "jitter" =
               panel.xyplot(x = x,
                            y = jitter(rep(0, length(x)), amount = jitter.amount),
                            type = type,
                            ...,
                            identifier = identifier))
    }
}



54### END ###