quiver <- function(x, y, u, v, length=0.25, scale=1, col="black", add=F) {
    #Scale arrow size relative to window size
    scale <- (par('fin')[1] * par('cxy')[1] / par('cin')[1]) * scale / max(sqrt(u^2 + v^2), na.rm=T)
#    length <- scale * length * max(sqrt(u^2 + v^2))
    #Plot at each (x,y) a vector of length (u,v) with head size size*(length of arrow)
    print(scale)
    print(max(u, na.rm=T))
    if(!add)
        plot(0, type ='n',
             xlim = c(min(x, na.rm=T) - max(abs(u)*scale, na.rm=T), max(x, na.rm=T) + max(abs(u)*scale, na.rm=T)),
             ylim = c(min(y, na.rm=T) - max(abs(v)*scale, na.rm=T), max(y, na.rm=T) + max(abs(v)*scale, na.rm=T)),
             xlab = "x",
             ylab = "y")
    all.data <- data.frame(x, y, u, v)[u != 0 || v != 0,]
  
    with(all.data,
         arrows(x, y, x + u * scale, y + v * scale, length=length))
}
