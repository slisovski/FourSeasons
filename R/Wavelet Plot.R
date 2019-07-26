x = wt
ncol = 64
fill.cols = NULL
xlab = "Time"
ylab = "Period"
tol = 1
plot.cb = FALSE
plot.phase = FALSE
type = "power.corr.norm"
plot.coi = TRUE
lwd.coi = 1
col.coi = "white"
lty.coi = 1
alpha.coi = 0.5
plot.sig = TRUE
lwd.sig = 4
col.sig = "black"
lty.sig = 1
bw = FALSE
legend.loc = NULL
legend.horiz = FALSE
arrow.len = min(par()$pin[2]/30, par()$pin[1]/40)
arrow.lwd = arrow.len * 0.3
arrow.cutoff = 1
arrow.col = "black"
xlim = NULL
ylim = NULL
zlim = NULL
xaxt = "s"
yaxt = "s"
form = "%Y"


    fill.cols   <- c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    col.pal     <- colorRampPalette(fill.cols)
    fill.colors <- col.pal(ncol)
types <- c("power.corr.norm", "power.corr", "power.norm",
           "power", "wavelet", "phase")
type <- match.arg(tolower(type), types)

    x$power <- x$power.corr

    zvals <- log2(abs(x$power/x$sigma2))
    if (is.null(zlim)) {
      zlim <- range(c(-1, 1) * max(zvals))
    }
    zvals[zvals < zlim[1]] <- zlim[1]
    locs <- pretty(range(zlim), n = 5)
    leg.lab <- 2^locs

if (is.null(xlim)) {
  xlim <- range(x$t)
}
yvals <- log2(x$period)

if (is.null(ylim)) {
  ylim <- range(yvals)
} else {
  ylim <- log2(ylim)
}

  image(x$t, yvals, t(zvals), zlim = zlim, xlim = xlim, ylim = rev(ylim),
        xlab = xlab, ylab = ylab, yaxt = "n", xaxt = "n", col = fill.colors)
  box()

  axis(side = 1, at = xlocs)


  axis.locs <- axTicks(2)
  yticklab <- format(2^axis.locs, dig = 1)
  axis(2, at = axis.locs, labels = yticklab)


  polygon(x = c(x$t, rev(x$t)), lty = 1, lwd = 2,
          y = c(log2(x$coi), rep(max(log2(x$coi), na.rm = TRUE),
              length(x$coi))), col = adjustcolor("white", alpha.f = 0.5), border = NA)

  contour(x$t, yvals, t(x$signif), level = tol, col = "black",
            lwd = 2, add = TRUE, drawlabels = FALSE)

