#' Finite Volume Community Ocean Model grid
#'
#' \code{fvcom.grid} provides a represenation of the unstructured triangular
#' sigma grid used by the Finite Volume Community Ocean Model (FVCOM). As a
#' disclaimer, please note that the author of this package is a user of FVCOM,
#' but is not affiliated with its development.
#'
#' @section Slots:
#'   \describe{
#'     \item{nodes.n}{The number of nodes in the grid.}
#'     \item{nodes.x}{x-coordinates of the nodes (m).}
#'     \item{nodes.y}{y-coordinates of the nodes (m).}
#'     \item{nodes.h}{z-coordinates (depth) of the nodes (m).}
#'     \item{nodes.lat}{Latitude of the nodes.}
#'     \item{nodes.lon}{Longitude of the nodes.}
#'     \item{elems.n}{Number of elements in the grid}
#'     \item{elems.v1}{1st set of node indices}
#'     \item{elems.v2}{2nd set of node indices}
#'     \item{elems.v3}{3rd set of node indices}
#'     \item{proj}{A string that could be passed to proj4::project to convert
#'                 x and y values to latitude and longitude.}
#'   }
#' 
#' @references http://fvcom.smast.umassd.edu/FVCOM/
setClass("fvcom.grid",
         representation(
             nodes.n="integer",
             nodes.x="numeric",
             nodes.y="numeric",
             nodes.h="numeric",
             nodes.lat="numeric",
             nodes.lon="numeric",
             elems.n="integer",
             elems.v1="integer",
             elems.v2="integer",
             elems.v3="integer",
             proj="character"
             )
         )

#' Load an FVCOM grid from a NetCDF output file.
#'
#' Loads enough of an FVCOM grid to use the other methods associated with
#' the \code{fvcom.grid} class. The (x,y,h) locations of the nodes and their
#' connections to form an unstructured triangular mesh are loaded.
#' @param filename The name of an output NetCDF file from FVCOM 2.7.
#' @return An instance of the \code{fvcom.grid} class.
loadFVCOMGrid27 <- function(filename, proj) {
    ncid <- nc_open(filename)
    x <- as.vector(ncvar_get(ncid, 'x'))
    y <- as.vector(ncvar_get(ncid, 'y'))
    h <- as.vector(ncvar_get(ncid, 'h'))
    if(proj != '') {
        ll <- project(data.frame(x, y), proj, inverse=TRUE)
    } else {
        ll <- data.frame(x=NULL, y=NULL)
    }
    ev <- ncvar_get(ncid, 'nv') ## Element vertices
    nc_close(ncid)
    return(new("fvcom.grid",
               nodes.n=length(x), nodes.x=x, nodes.y=y, nodes.h=h,
               nodes.lat=ll$y, nodes.lon=ll$x, proj=proj,
               elems.n=nrow(ev),
               elems.v1=ev[,1], elems.v2=ev[,2], elems.v3=ev[,3]))
}

#' Checks if the points (xy$x, xy$y) are in the \code{fvcom.grid} \code{grid}.
#'
#'  Given a grid and a point or set of points, \code{find.elem} returns
#'  the element number of each point in the grid. NA is returned for any
#'  point that does not lie within the grid. \code{x} and \code{y} must be
#'  the same length.
#' 
#' @param grid A \code{fvcom.grid} instance.
#' @param xy A \code{data.frame} with components \code{x} and \code{y} with
#'           the x and y locations of the points.
#' @return A vector of integer values of length \code{nrow(xy)}. The values
#'         are indexes into the elements of grid.
#'
#' @examples {
#' # Load the example grid
#' data(ocean.demo.grid)
#' # Create a regular matrix grid
#' lattice.grid = expand.grid(seq(min(grid@nodes.x), max(grid@nodes.x),
#'                            len=10), y=seq(min(grid@nodes.y),
#'                            max(grid@nodes.y), len=10))
#' elems = find.elem(ocean.demo.grid, lattice.grid, units="m")
#' # Plot the result
#' plot(lattice.grid$x, lattice.grid$y, pch=15,
#'      col=heat.colors(max(elems, na.rm=TRUE) + 2)[elems+2])
#' }
findElemFVCOMGrid <- function(grid, xy, units='ll') {
    if(units == 'll') {
        grid.x <- grid@nodes.lon        
        grid.y <- grid@nodes.lat
    } else if(units == 'm') {
        grid.x <- grid@nodes.x
        grid.y <- grid@nodes.y
    } else {
        stop("Invalid units, must be 'll' or 'm'.")
    }
    elems <- integer(nrow(xy))
    ## TODO Look into ways to avoid copying the node/element data.
    elems <- .C('R_find_element', PACKAGE='ocean',
                x_pts=as.double(xy$x), y_pts=as.double(-xy$y), n_pts=nrow(xy),
                x=as.double(grid.x), y=as.double(-grid.y),
                n_grid_pts=as.integer(grid@elems.n),
                v1=as.integer(grid@elems.v1 - 1), ## TODO Subtraction in
                v2=as.integer(grid@elems.v2 - 1), ## C code
                v3=as.integer(grid@elems.v3 - 1),
                elems=as.integer(elems))$elems + 1
    ## TODO Need to modify C code to return elem + 1 except if elem = -1
    ## TODO C code should use R NA
    #print(sum(elems == 0))
    elems[elems == 0] <- NA
    return(elems)
}
setGeneric("find.elem", findElemFVCOMGrid)
setMethod("find.elem", "fvcom.grid", findElemFVCOMGrid)
#' Get the depth at each node in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return A vector of length get.nnodes(grid) with the depth at each node.
getFVCOMDepth <- function(grid)
    return(grid@nodes.h)
setGeneric("get.depth", getFVCOMDepth)
setMethod("get.depth", "fvcom.grid", getFVCOMDepth)

#' Get the indices of the element vertices in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return A \code{data.frame} with \code{v1}, \code{v2}, and \code{v3}
#' elements that correspond to the indices of the nodes at vertices 1, 2, and
#' 3.
getFVCOMElems <- function(grid)
    return(data.frame(v1=grid@elems.v1, v2=grid@elems.v2, v3=grid@elems.v3))
setGeneric("get.elems", getFVCOMElems)
setMethod("get.elems", "fvcom.grid", getFVCOMElems)

#' Get the number of elements in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return The number of elements in \code{grid}.
getFVCOMNElems <- function(grid)
    return(grid@elems.n)
setGeneric("get.nelems", getFVCOMNElems)
setMethod("get.nelems", "fvcom.grid", getFVCOMNElems)

#' Get the number of nodes in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return The number of nodes in \code{grid}.
getFVCOMNNodes <- function(grid)
    return(grid@nodes.n)
setGeneric("get.nnodes", getFVCOMNNodes)
setMethod("get.nnodes", "fvcom.grid", getFVCOMNNodes)

#' Get the values of the nodes in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return A \code{data.frame} with \code{x}, \code{y}, and \code{h} elements
#' that correspond to the x, y, and depth of each node.
getFVCOMNodes <- function(grid)
    return(data.frame(x=grid@nodes.x, y=grid@nodes.y, h=grid@nodes.h))
setGeneric("get.nodes", getFVCOMNodes)
setMethod("get.nodes", "fvcom.grid", getFVCOMNodes)

#' Get the value of the projection string.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return The value of the projection string specified when the grid was
#'         created.
getFVCOMProj <- function(grid)
    return(grid@proj)
setGeneric("get.proj", getFVCOMProj)
setMethod("get.proj", "fvcom.grid", getFVCOMProj)

#' Convert a single scalar or node based quantity to element based. 
#' 
#' The length of \code{x} determines how it will be treated. If \code{x} has
#' length 1, it is returned as a single color. If the length of \code{x} is
#' the number of nodes in the grid, its value for each element is calculated
#' as the average of the value at the adjoining nodes. If the length of
#' \code{x} is the number of elements in the grid, it is returned as is. Any
#' other values throw an error.
#' \param grid An fvcom.grid instance.
#' \param x A vector of length 1 or \code{length get.nnodes(grid)}
#' \return A vector of length \code{get.nelems(grid)}
interpFVCOMGrid <- function(grid, x) {
    if(length(x) == 1) {
        ## Repeat the scalar value nelems times.
        x.ret <- rep(x, get.nelems(grid))
    } else if(length(x) == get.nnodes(grid)) {
        ## Average the values at each node to get the value in each element.
        x.ret <- sapply(seq(get.nelems(grid)), function(i)
                        return(mean(c(x[grid@elems.v1[i]],
                                      x[grid@elems.v2[i]],
                                      x[grid@elems.v3[i]]))))
    } else if(length(x) == get.nelems(grid)) {
        x.ret <- x
    } else {
        stop(paste('length(x) must be equal to 1, the number of nodes, or ',
                   'the number of elements\n', sep=''))
    }
    return(x.ret)
}
setGeneric("interp", interpFVCOMGrid)
setMethod("interp", "fvcom.grid", interpFVCOMGrid)

#' Plot a \code{fvcom.grid} instance as a heatmap.
#'
#' Given a vector of data, this function plots the data as a heatmap on
#' an unstructured grid. The length of the data vector must be as long as
#' either the number nodes in the grid or the number of elements in the
#' grid. The grid is currently stored as a data.frame, but will be
#' converted to an S4 object in the future.
#' 
#' @param x A \code{fvcom.grid} instance.
#' @param z A vector to plot as a heatmap.
#' @param units Either 'll' for latitude and longitude or 'm' for meters.
#' @param col A list of colors, such as that returned by bathy.colors.
#' @param add Should the plot be added to the current plot?
#' #' @param xlim x-limits for the plot.
#' @param ylim y-limits for the plot.
#' @param zlim z-limits for the plot. 
#' @param border.col Color of the element borders. If not provided the borders
#'                   will be colored to match the adjacent polygons.
#' @param bg.col Color of the background. The background is only plotted if
#'               add=F, otherwise bg.col is ignored.
#'
#' @examples {
#'   # Load a demonstration grid
#'   data(ocean.demo.grid)
#'   op = par(ask=TRUE)
#'   # Plot the grid in a single color
#'   image(grid, col='white')
#'   # Plot the grid in bathy colors
#'   image(grid)
#'   par(op)
#' }
imageFVCOMGrid <- function(x, z=get.depth(grid), units='ll',
                          col=bathy.colors(100), add=FALSE,
                          xlim=NA, ylim=NA, zlim=NA, legend=FALSE,
                          border.col=NA, bg.col='gray') {
    ## TODO Set aspect ratio automatically
    ## Check the parameters for validity.
    ## Convert the passed in values to an element based quantity.
    z <- interp(grid, z)
    ## Calculate z limits
    if(!is.na(zlim)[1]) {
        ## If zlimits were provided, any values that lie outside of them are
        ## converted to NA.
        z[z < zlim[1]] <- NA
        z[z > zlim[2]] <- NA
    } else {
        ## If no z limits were provided, calculate them as the minimum and
        ## maximum of z ignoring missing values.
        zlim <- c(min(z, na.rm=TRUE), max(z, na.rm=TRUE))
    }
    ## Get the lat/lon or x/y locations of the nodes surrounding the elements.
    ## c(rbind()) interleaves the vectors
    if(units == "ll") { # Lat/lon
        x <- grid@nodes.lon[c(rbind(grid@elems.v1, grid@elems.v2,
                                    grid@elems.v3))]
        y <- grid@nodes.lat[c(rbind(grid@elems.v1, grid@elems.v2,
                                    grid@elems.v3))]
    } else if(units == "m") {
        x <- grid@nodes.x[c(rbind(grid@elems.v1, grid@elems.v2,
                                  grid@elems.v3))]
        y <- grid@nodes.y[c(rbind(grid@elems.v1, grid@elems.v2,
                                  grid@elems.v3))]
    } else {
        stop("Invalid units, options are 'll' or 'm'.")
    }
    ## Calculate the colors for the polygons and the borders
    n.cols <- length(col)
    cols.idx <- floor((z - zlim[1]) / (zlim[2] - zlim[1]) * n.cols)
    cols.idx[cols.idx <= 0] <- 1
    cols.idx[cols.idx > n.cols] <- n.cols
    col <- col[cols.idx]
    if(is.na(border.col)[1])
        border.col <- col
    ## cut.poly adds NAs after each polygon, ensuring that the polygons are
    ## not connected to one another by spurious lines.
    cut.poly <- function(x, n.sides=3) {
        n <- length(x) %/% n.sides
        polys <- rep(NA, n*n.sides)
        polys[rep(c(rep(T, n.sides), F), n)] <- x
        return(polys)
    }
    ## Set the x and y limits.
    if(is.na(xlim[1]))
        xlim <- c(min(x), max(x))
    if(is.na(ylim[1]))
        ylim <- c(min(y), max(y))
    ## Do the actual plotting
    if(!add)
        image(matrix(1,1,1), col=bg.col,
              xlab = "Longitude",
              ylab = "Latitude",
              xlim=xlim, ylim=ylim)
    polygon(cut.poly(x), cut.poly(y), col=col, border=border.col)
    #if(legend) (TODO Add legend)
    #    legend(min(x), max(y),
    #           legend=c(round(zlim[2], 2), rep("", n.cols-2),
    #           round(zlim[1], 2)),
    #           bt="n",
    #           col=rev(cols),
    #           y.intersp=10/n.cols,
    #           pch=15,
    #           cex=2)
}
setMethod("image", "fvcom.grid", imageFVCOMGrid)

#' Checks if the points (xy$x, xy$y) are in the \code{fvcom.grid} \code{grid}.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @param xy A \code{data.frame} with components \code{x} and \code{y} with
#'           the x and y locations of the points.
#' @return A vector of logical values of length \code{nrow(xy)}. The ith
#'         element is \code{TRUE} if (\code{xy$x[i]}, \code{xy$y[i]}) is in
#'         \code{grid} and \code{FALSE} otherwise.
#'
#' @examples {
#' # Load the demo grid
#' data(ocean.demo.grid)
#' # Create a regular grid of test points
#' lattice.grid = expand.grid(seq(min(grid@nodes.x), max(grid@nodes.x),
#'                            len=10), y=seq(min(grid@nodes.y),
#'                            max(grid@nodes.y), len=10))
#' # Check which points are in the grid
#' in.grid = is.in.grid(ocean.demo.grid, lattice.grid, units="m")
#' # Plot the points that are in the grid
#' with(lattice.grid[in.grid,], plot(x, y))
#' }
isInFVCOMGrid <- function(grid, xy, units='ll')
    return(!is.na(find.elem(grid, xy, units)))
setGeneric("is.in.grid", isInFVCOMGrid)
setMethod("is.in.grid", "fvcom.grid", isInFVCOMGrid)

#' Plot the density of x and y on grid.
#' 
#' Plots the distribution of x and y on grid. This function follows the
#' method described in Simons et al 2013: first vertically integrating the
#' data, then dividing by the number of particles spawned, and finally
#' applying an Gaussian blur filter. The coordinates should be passed in as
#' x,y in meters, then are inverse projected into lat/long using the PROJ.4
#' library.  
#' 
#' @param grid A \code{fvcom.grid} instance.
#' @param x A vector of x location of points. NAs are not supported.
#' @param y A vector of y locations of points. NAs are not supported.
#' @param npoints The number of points to scale the density plot by. This
#'                defaults to the number of points passed in, but it may be
#'                useful to set it to a different value if only a subset of
#'                the points are being plotted (e.g. some points are outside
#'                of the domain).
#' @param res The resolution of the plot in the same dimensions as \code{xy}
#'            is given. Square boxes will be plotted with each side of length
#'            \code{res}.
#' @param sigma The standard deviation of the Gaussian smoothing filter to be
#'              applied. If no filter is required, set sigma=0.
#' @param log Should the density be log10 transformed before plotting?
#' @param bg.col The background color.
#' @param col A list of colors, such as that returned by heat.colors.
#' @param add Should the plot be added to the current plot?
#' @param xlim x-limits for the plot.
#' @param ylim y-limits for the plot.
#' @param lim.units Units for xlim and ylim. One of 'm' (meters) or 'll'
#'                  (latitude and longitude).
#' @param zlim z-limits for the plot.
#'
#' @examples {
#' # Load the demo grid
#' data(ocean.demo.grid)
#' # Generate artificial data from a Gaussian distribution
#' x = rnorm(10000, mean(ocean.demo.grid@nodes.x), 1000)
#' y = rnorm(10000, mean(ocean.demo.grid@nodes.y), 1000)
#' # Plot the density of the mixture
#' grd = pdd(ocean.demo.grid, x, y, res=100)
#' }
#'
#' @references {
#' Simons, R.D. and Siegel, D.A. and Brown K.S. 2013 Model sensitivity
#' and robustness in the estimation of larval transport: A study of
#' particle tracking parameters \emph{J. Marine Systems} 119--120:
#' 19--29.
#' }
pddFVCOMGrid <- function(grid, xy, npoints=nrow(xy), res=1000, sigma=0,
                         log=F, bg.col='gray', col=heat.colors(100), add=F,
                         xlim=c(min(xy$x, na.rm=TRUE), max(xy$x, na.rm=TRUE)),
                         ylim=c(min(xy$y, na.rm=TRUE), max(xy$y, na.rm=TRUE)),
                         lim.units = 'm',
                         zlim=NA) {
    ## TODO Use a matrix with rownames and colnames attrs
    ## Create a lattice grid for calculating the density.
    grd <- list(x=seq(xlim[1], xlim[2], by=res),
                y=seq(ylim[1], ylim[2], by=res))
    grd$data <- matrix(0, nrow=length(grd$x) - 1, ncol=length(grd$y) - 1)
    ## Bin the data into the grid
    bin.data <- function(x, y, grid) {
        out <- matrix(.C('R_bin_data',
                         as.double(x), as.double(y), as.integer(length(x)),
                         as.double(grid$x), as.double(grid$y),
                         as.integer(nrow(grid$data)),
                         as.integer(ncol(grid$data)),
                         data=as.integer(as.vector(grid$data)))$data,
                      nrow(grid$data))
        return(out)
    }
    grd$data <- bin.data(xy$x, xy$y, grd)
    if(sum(grd$data > 0) == 0)
        stop('No points were within the grid.')
    ## Rescale by the number of particles released
    grd$data <- grd$data / npoints
    ## Apply a 2D Gaussian filter with std dev = sigma
    ## TODO Add xy.units as an option
    if(sigma > 0)
        grd$data <- filter2d(grd$data, sigma)
    ## Log transform the data if necessary
    if(log) {
        grd$data[grd$data == 0] <- NA ## Because log10(0) = -Inf
        grd$data <- matrix(log10(grd$data), nrow(grd$data))
    }
    if(is.na(zlim[1]))
        zlim <- c(min(grd$data, na.rm=TRUE), max(grd$data, na.rm=TRUE))
    ## Project the grid x,y into lat/lon
    x.proj <- c(grd$x, rep(grd$x[1], length(grd$y)))
    y.proj <- c(rep(grd$y[1], length(grd$x)), grd$y)
    p <- project(data.frame(x=x.proj, y=y.proj),
                 proj=get.proj(grid), inverse=TRUE)
    grd$x <- p$x[seq(length(grd$x))]
    grd$y <- p$y[length(p$y) - rev(seq(length(grd$y))) + 1]
    ## Project xlim and ylim if necessary
    if(lim.units == 'm') {
        lim.proj <- project(data.frame(x=xlim, y=ylim),
                            proj=get.proj(grid), inverse=TRUE)
        xlim <- lim.proj$x
        ylim <- lim.proj$y
    }
    ## Set up a land mask
    ## Calculate the center of each grid cell. A cell is considered to be part
    ## of the grid if its center lies on the grid.
    ## TODO Adjust boundary cells to match grid exactly.
    grd$x.cent <- sapply(seq(length(grd$x) - 1), function(i)
                         mean(c(grd$x[i], grd$x[i + 1])))
    grd$y.cent <- sapply(seq(length(grd$y) - 1), function(i)
                         mean(c(grd$y[i], grd$y[i + 1])))
    mask <- expand.grid(x=grd$x.cent, y=grd$y.cent)
    mask$on.grid <- is.in.grid(grid, data.frame(x=mask$x, y=mask$y))
    ## TODO Why recast this? Convert row to column major? If so, just reverse
    ## the expand.grid arguments.
    grd$mask <- matrix(mask$on.grid, nrow(grd$data))
    grd$data[!grd$mask] <- NA
    rm(mask)
    
    ## Do the actual plotting
    image(matrix(1, 1, 1), xlim=xlim, ylim=ylim,
          col=bg.col, xlab='Longitude', ylab='Latitude')
    image(grd$data, x=grd$x, y=grd$y, col=col, add=TRUE, zlim=zlim)
    return(grd)
}
setGeneric("pdd", pddFVCOMGrid)
setMethod("pdd", "fvcom.grid", pddFVCOMGrid)

#' Plot an instance of the \code{fvcom.grid} class and overlay the 
#' trajectories given by \code{xy}.
#' 
#' @param x A \code{fvcom.grid} instance
#' @param xy A \code{list} with matrices \code{x} and \code{y} components that
#'           contain the trajectories to plot. The columns of \code{xy$x} are
#'           plotted against the columns of \code{xy$y}, so each particle
#'           trajectory should be in a column and each time index in a row.
#' @param plot.units The units for plotting. Either 'm' for meters or 'll' for
#'                    latitude and longitude.
#' @param xy.units The units of \code{xy}. Either 'm' for meters or 'll' for
#'                 latitude and longitude.
#' @param col A vector of colors for each trajectory. If this vector is
#'            shorter than \code{nrow(xy)}, it is recycled to the appropriate
#'            length. If it is longer than \code{nrow(xy)} only the first
#'            \code{nrow(xy)} components are used.
#' @param lwd The line width for the trajectories.
#'
#' @examples {
#' # Load the demo grid
#' data(ocean.demo.grid)
#' # Create a set of random trajectories.
#' x = apply(matrix(rnorm(10000, mean(grid@nodes.lon)), 100), 2, cumsum)
#' y = apply(matrix(rnorm(10000, mean(grid@nodes.lat)), 100), 2, cumsum)
#' plot(ocean.demo.grid, list(x=x, y=y))
plotFVCOMGrid <- function(x, xy, plot.units='ll', xy.units='ll', col='black',
                          lwd=1, lty=1) {
    ## Project xy if necessary
    if((xy.units == 'm') & (plot.units == 'll')) {
        xy.proj = project(data.frame(x=as.vector(xy$x), y=as.vector(xy$y)),
                          proj=get.proj(x), inverse=TRUE)
        xy$x = matrix(xy.proj$x, nrow=nrow(xy$x))
        xy$y = matrix(xy.proj$y, nrow=nrow(xy$y))
    } else if((xy.units == 'll') & (plot.units == 'm')) {
        xy.proj = project(data.frame(x=as.vector(xy$x), y=as.vector(xy$y)),
                          proj=get.proj(x), inverse=FALSE)
        xy$x = matrix(xy.proj$x, nrow=nrow(xy$x))
        xy$y = matrix(xy.proj$y, nrow=nrow(xy$y))
    }
    ## Plot the background, then plot the trajectories
    image(x, col='white', units=plot.units)
    matlines(xy$x, xy$y, col=col, lwd=lwd, lty=lty)
}
setMethod("plot", "fvcom.grid", plotFVCOMGrid)

#' Check if a \code{fvcom.grid} instance is valid.
validFVCOMGrid <- function(object) {
    if((length(object@nodes.x) != object@nodes.n) ||
       (length(object@nodes.y) != object@nodes.n) ||
       (length(object@nodes.h) != object@nodes.n))
       return("Number of x, y, and z coordinates of nodes must be equal.")
    else if((length(object@elems.v1) != object@elems.n) ||
            (length(object@elems.v2) != object@elems.n) ||
            (length(object@elems.v3) != object@elems.n))
        return("All elements must have 3 vertices.")
    else if((sum((c(object@nodes.x, object@nodes.y, object@nodes.h) == Inf))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0))
        return("Nodes may not have Inf or NA coordinates")
    else if((sum((c(object@nodes.x, object@nodes.y, object@nodes.h) == Inf))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0))
        return("Elems may not have Inf or NA indices")
    else return(TRUE)
}
setValidity("fvcom.grid", validFVCOMGrid)

#' Create a new FVCOM grid instance from a FVCOM NetCDF output file.
#' 
#' @param filename The name of an output NetCDF file from FVCOM.
#' @param proj A string to passed to proj4::project to convert x,y locations
#' to latitude and longitude.
#' @return An instance of the \code{fvcom.grid} class.
fvcom.grid <- function(filename, proj)
    return(loadFVCOMGrid27(filename, proj))

