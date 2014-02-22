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
#' @param grid A \code{fvcom.grid} instance.
#' @param xy A \code{data.frame} with components \code{x} and \code{y} with
#'           the x and y locations of the points.
#' @return A vector of logical values of length \code{nrow(xy)}. The ith
#'         element is \code{TRUE} if (\code{xy$x[i]}, \code{xy$y[i]}) is in
#'         \code{grid} and \code{FALSE} otherwise.
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
    elems <- .C('R_find_element', PACKAGE='ocean',
                x_pts=as.double(xy$x), y_pts=as.double(xy$y), n_pts=nrow(xy),
                x=as.double(grid.x), y=as.double(grid.y),
                n_grid_pts=as.integer(grid@elems.n),
                tri1=as.integer(grid@elems.v1 - 1), ## TODO Subtraction in
                tri2=as.integer(grid@elems.v2 - 1), ## C code
                tri3=as.integer(grid@elems.v3 - 1),
                elems=as.integer(elems))$elems + 1
    ## TODO Need to modify C code to return elem + 1 except if elem = -1
    ## TODO C code should use R NA
    elems[elems == 0] <- NA
    return(elems)
}
setGeneric("find.elem", isInFVCOMGrid)
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
imageFVCOMGrid <- function(x, z=get.depth(grid), units='ll',
                          col=bathy.colors(100), add=FALSE,
                          xlim=NA, ylim=NA, zlim=NA, legend=FALSE,
                          border.col=NA, bg.col='white') {
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
    
    ## cut.poly adds NAs after each polygon, ensuring the the polygons are
    ## not connected to one another by spurious lines.
    cut.poly <- function(x, n.sides=3) {
        n <- length(x) %/% n.sides
        polys <- rep(NA, n*n.sides)
        polys[rep(c(rep(T, n.sides), F), n)] <- x
        return(polys)
    }

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
    #if(legend) (TODO)
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
isInFVCOMGrid <- function(grid, xy, units='ll')
    return(!is.na(find.elem(grid, xy, units)))
setGeneric("is.in.grid", isInFVCOMGrid)
setMethod("is.in.grid", "fvcom.grid", isInFVCOMGrid)

#' Plot the density of x and y on grid.
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
#' @param zlim z-limits for the plot. 
pddFVCOMGrid <- function(grid, xy, npoints=nrow(xy), res=1000, sigma=0,
                         log=F, bg.col='white', col=heat.colors(100), add=F,
                         xlim=c(min(xy$x, na.rm=TRUE), max(xy$x, na.rm=TRUE)),
                         ylim=c(min(xy$y, na.rm=TRUE), max(xy$y, na.rm=TRUE)),
                         zlim=NA) {
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
    ## Rescale by the number of particles released
    grd$data <- grd$data / npoints
    ## Apply a 2D Gaussian filter with a 5km std dev
    ## TODO Add xy.units as an option
    grd$data <- filter2d(grd$data, sigma)
    ## Log transform the data if necessary
    if(log) {
        grd$data[grd$data == 0] <- NA ## log10(0) = -Inf
        grd$data <- matrix(log10(grd$data), nrow(grd$data))
    }
    if(is.na(zlim[1]))
        zlim <- c(min(grd$data), max(grd$data))
    
    ## Project the grid x,y into lat/lon
    x.proj <- c(grd$x, rep(grd$x[1], length(grd$y)))
    y.proj <- c(rep(grd$y[1], length(grd$x)), grd$y)
    p <- project(data.frame(x=x.proj, y=y.proj),
                 proj=grid@proj, inverse=T)
    grd$x <- p$x[seq(length(grd$x))]
    grd$y <- p$y[length(p$y) - rev(seq(length(grd$y))) + 1]
    ## TODO Project xlim, ylim
    
    ## TODO Check this
    x.proj <- c(grd$x, rep(grd$x[1], length(grd$y)))
    y.proj <- c(rep(grd$y[1], length(grd$x)), grd$y)
    p <- project(data.frame(x=x.proj, y=y.proj), proj=grid@proj, inverse=T)
    grd$x <- p$x[seq(length(grd$x))]
    grd$y <- p$y[length(p$y) - rev(seq(length(grd$y))) + 1]
    
    ## Set up a land mask
    ## Calculate the center of each grid cell. A cell is considered to be part
    ## of the grid if its center lies on the grid.
    ## TODO Adjust boundary cells to match grid exactly.
    grd$x.cent <- sapply(seq(length(grd$x) - 1), function(i)
                         mean(c(grd$x[i], grd$x[i + 1])))
    grd$y.cent <- sapply(seq(length(grd$y) - 1), function(i)
                         mean(c(grd$y[i], grd$y[i + 1])))
    mask <- expand.grid(x=grd$x.cent, y=grd$y.cent)
    mask$on.grid <- is.in.grid(mask$x.cent, mask$y.cent, grid)
    ## TODO Why recast this? Convert row to column major? If so, just reverse
    ## the expand.grid arguments.
    grd$mask <- matrix(mask$on.grid, nrow(grd$data))
    grd$data[!grd$mask] <- NA
    rm(mask)
    
    ## Do the actual plotting
    image(matrix(1, 1, 1), xlim=c(150, 151.25), ylim=c(-5.6, -4.5),
          col=bg.col, xlab='Longitude', ylab='Latitude')
    image(grd$data, x=grd$x, y=grd$y, col=cols, add=T, zlim=zlim)
    return(grd)
}
setGeneric("pdd", pddFVCOMGrid)
setMethod("pdd", "fvcom.grid", pddFVCOMGrid)


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

