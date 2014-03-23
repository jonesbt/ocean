usr2in <- function() {
    usr <- par('usr')
    fin <- par('fin')
    x <- fin[1] / (usr[2] - usr[1])
    y <- fin[2] / (usr[4] - usr[3])
    return(c(x,y))
}

in2usr <- function() {
    return(1 / usr2in(x, y))
}
