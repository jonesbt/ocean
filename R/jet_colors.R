## This provides an equivalent of the matplotlib or MATLAB jet colorscheme.
## TODO: Use hsv colors to support alpha values
jet.colors <- function(n, alpha=1) {
    if(n > 0) {
        if(length(alpha) != 1 & length(alpha) != n) {
            print('Warning: using only first alpha value')
            alpha <- alpha[1]
        }
        if(length(alpha) == 1) {
            alpha <- rep(alpha, n)
        }
        return(colorRampPalette(c('blue', 'cyan', 'yellow', 'red', '#660000'))(n))
    } else {
        character()
    }
}

