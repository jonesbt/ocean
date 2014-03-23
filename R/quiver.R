# Origin: Nemo

##
#  Plots arrows indicating the velocity field along the grid defined by
#  expand.grid(x,y). The greatest magnitude velocity has an arrow with length
#  equal to the shortest distance between any 2 nodes. The u and v velocities
#  should be column major matrices or vectors of length length(x) * length(y). If zlim is provided, the arrows are scaled from zmin to zmax.
#  The arrows are drawn at nx equally spaced x locations and ny equally
#  spawced y locations.
#
#  \param x The x coordinates
#  \param y The y coordinates
#  \param u The u velocities
#  \param v The v velocities
#  \param nx Number of x locations
#  \param ny Number of y locations
#  \param add Should the quivers be added to the current plot (default false)
#  \param zlim Scaling parameters for the arrow lengths
source("../src/R/quiver_vector.R") # TODO Convert to package
source("../src/R/quiver_matrix.R")
quiver <- function(x, y, u, v, nx=length(x), ny=length(y), add=F, zlim=NA) {
    ## Check arguments and call more specific function
    if(length(u) == length(x)) {
        quiver_vector(x, y, u, v, nx, add, zlim)
    } else if(length(u) == (length(x) * length(y))) {
        quiver_matrix(x, y, u, v, nx, ny, add, zlim)
    } else {
        print("Unsupported dimensions for x, y, u, and v")
        return()
    }
}
