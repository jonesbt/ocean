color.bar = function(zlim, col, log=FALSE, ...) {
    ## Make the colorbar
    vals = matrix(seq(length(col)), 1, length(col))
    y = seq(zlim[1], zlim[2], length=length(col))
    x = 1
    if(log)
        y = log10(y)
    image(x=x, y=y, vals, col=col, axes=FALSE, xlab="", ylab="")
    ## Add the labels
    at = pretty(zlim)
    lab = at
    if(log)
        at = log10(at)
    axis(4, at=at, lab=lab, las=1, ...)
}
